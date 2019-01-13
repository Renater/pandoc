{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2019 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Ipynb
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Ipynb (Jupyter notebook JSON format) reader for pandoc.
-}
module Text.Pandoc.Readers.Ipynb ( readIpynb )
where
import Prelude
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Text.Pandoc.Options
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Logging
import Text.Pandoc.Definition
import Text.Pandoc.Ipynb as Ipynb
import Text.Pandoc.Class
import Text.Pandoc.MIME (MimeType, extensionFromMimeType)
import Text.Pandoc.UTF8
import Text.Pandoc.Error
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson as Aeson
import Control.Monad.Except (throwError)
import Text.Pandoc.Readers.Markdown (readMarkdown)

readIpynb :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readIpynb opts t = do
  case eitherDecode (BL.fromStrict $ TE.encodeUtf8 t) of
    Left err -> throwError $ PandocIpynbDecodingError err
    Right notebook -> notebookToPandoc opts notebook

notebookToPandoc :: PandocMonad m => ReaderOptions -> Notebook -> m Pandoc
notebookToPandoc opts notebook = do
  let cells = n_cells notebook
  let m = jsonMetaToMeta (n_metadata notebook)
  let lang = case M.lookup "kernelspec" m of
                   Just (MetaMap ks) ->
                      case M.lookup "language" ks of
                         Just (MetaString l) -> l
                         _ -> "python"
                   _ -> "python"
  bs <- mconcat <$> mapM (cellToBlocks opts lang) cells
  return $ B.setMeta "jupyter" (MetaMap m) $ B.doc bs

cellToBlocks :: PandocMonad m => ReaderOptions -> String -> Cell -> m B.Blocks
cellToBlocks opts lang c = do
  let Source ts = c_source c
  let source = mconcat ts
  let kvs = jsonMetaToPairs (c_metadata c) ++
            maybe mempty (\ec -> [("execution_count", show ec)])
             (c_execution_count c)
  let outputs = fromMaybe mempty $ c_outputs c
  let attachments = maybe mempty M.toList $ c_attachments c
  mapM_ addAttachment attachments
  case c_cell_type c of
    Ipynb.Markdown -> do
      Pandoc _ bs <- readMarkdown opts source
      return $ B.divWith ("",["cell","markdown"],kvs)
             $ B.fromList bs
    Ipynb.Raw -> do
      let format = fromMaybe "" $ lookup "format" kvs
      let format' =
            case format of
              "text/html"       ->  "html"
              "text/latex"      -> "latex"
              "application/pdf" -> "latex"
              "text/markdown"   -> "markdown"
              "text/x-rsrt"     -> "rst"
              _                 -> format
      return $ B.divWith ("",["cell","raw"],kvs) $ B.rawBlock format'
             $ T.unpack source
    Ipynb.Code -> do
      outputBlocks <- mconcat <$> mapM outputToBlock outputs
      return $ B.divWith ("",["cell","code"],kvs) $
        B.codeBlockWith ("",[lang],[]) (T.unpack source)
        <> outputBlocks

addAttachment :: PandocMonad m => (Text, MimeBundle) -> m ()
addAttachment (fname, mimeBundle) = do
  let fp = T.unpack fname
  case M.toList (unMimeBundle mimeBundle) of
    (mimeType, BinaryData bs):_ ->
      insertMedia fp (Just mimeType) (BL.fromStrict bs)
    (mimeType, TextualData t):_ ->
      insertMedia fp (Just mimeType) (BL.fromStrict $ TE.encodeUtf8 t)
    (mimeType, JsonData v):_ ->
      insertMedia fp (Just mimeType) (encode v)
    [] -> report $ CouldNotFetchResource fp "no attachment"

outputToBlock :: PandocMonad m => Output -> m B.Blocks
outputToBlock o = do
  let kvs = maybe mempty (\ec -> [("execution_count", show ec)])
             (o_execution_count o)
  let classes = ["output", map toLower . show . o_output_type $ o]
  let source = maybe mempty (T.unpack . mconcat . unSource) (o_text o)
  let streamName = maybe [] (\n -> [T.unpack n]) (o_name o)
  let data' = maybe mempty (M.toList . unMimeBundle) (o_data o)
  let metadata' = fromMaybe mempty (o_metadata o)
  innerContents <-
    case o_output_type o of
      Stream -> return $ B.codeBlockWith ("",streamName,[]) source
      Display_data -> mconcat <$> mapM (handleData metadata') data'
      Execute_result -> mconcat <$> mapM (handleData metadata') data'
  return $ B.divWith ("",classes,kvs) innerContents

handleData :: PandocMonad m
           => JSONMeta -> (MimeType, MimeData) -> m B.Blocks
handleData metadata' (mimeType, mimeData) = do
  -- normally metadata maps from mime types to key-value map;
  -- but not always...
  let meta = case M.lookup (T.pack mimeType) metadata' of
               Just v@(Object{}) ->
                 case fromJSON v of
                   Success m' -> m'
                   Error _   -> mempty
               _ -> mempty
  let metaPairs = jsonMetaToPairs meta
  case mimeData of
    BinaryData bs -> do
      let bl = BL.fromStrict bs
      -- SHA1 hash for filename
      let fname = showDigest (sha1 bl) ++
            case extensionFromMimeType mimeType of
              Nothing  -> ""
              Just ext -> '.':ext
      insertMedia fname (Just mimeType) bl
      return $ B.para $ B.imageWith ("",[],metaPairs) fname "" mempty
    TextualData t ->
      return $ B.codeBlockWith ("",[],metaPairs) $ T.unpack t
    JsonData v    ->
      return $ B.codeBlockWith ("",["json"],metaPairs) $ toStringLazy $ encode v

jsonMetaToMeta :: JSONMeta -> M.Map String MetaValue
jsonMetaToMeta = M.mapKeys T.unpack . M.map valueToMetaValue
  where
    valueToMetaValue :: Value -> MetaValue
    valueToMetaValue x@(Object{}) =
      case fromJSON x of
        Error s -> MetaString s
        Success jm' -> MetaMap $ jsonMetaToMeta jm'
    valueToMetaValue x@(Array{}) =
      case fromJSON x of
        Error s -> MetaString s
        Success xs -> MetaList $ map valueToMetaValue xs
    valueToMetaValue (Bool b) = MetaBool b
    valueToMetaValue (String t) = MetaString (T.unpack t)
    valueToMetaValue (Number n) = MetaString (show n)
    valueToMetaValue Aeson.Null = MetaString ""

jsonMetaToPairs :: JSONMeta -> [(String, String)]
jsonMetaToPairs = M.toList . M.mapMaybe
     (\case
        MetaString s -> Just s
        MetaBool True -> Just "true"
        MetaBool False -> Just "false"
        -- for now we skip complex cell metadata:
        _ -> Nothing) . jsonMetaToMeta
