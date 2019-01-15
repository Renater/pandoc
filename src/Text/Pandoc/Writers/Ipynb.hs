{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
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
   Module      : Text.Pandoc.Writers.Ipynb
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Ipynb (Jupyter notebook JSON format) writer for pandoc.

TODO:
[ ] Attachments (from mediabag)
[ ] Code cells (output)
-}
module Text.Pandoc.Writers.Ipynb ( writeIpynb )
where
import Prelude
import qualified Data.Map as M
import Data.List (partition, intersperse)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Ipynb as Ipynb
import Text.Pandoc.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson as Aeson
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.Writers.Shared (metaToJSON')
import Text.Pandoc.Writers.Markdown (writeMarkdown)

writeIpynb :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeIpynb opts d = do
  notebook <- pandocToNotebook opts d
  return $ encodeNotebook notebook

pandocToNotebook :: PandocMonad m
                 => WriterOptions -> Pandoc -> m (Notebook NbV4)
pandocToNotebook opts (Pandoc meta blocks) = do
  let blockWriter bs = writeMarkdown
           opts{ writerTemplate = Nothing } (Pandoc nullMeta bs)
  let inlineWriter ils = T.stripEnd <$> writeMarkdown
           opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain ils])
  let meta' = case lookupMeta "jupyter" meta of
                Just (MetaMap m') -> Meta m'
                _                 -> meta
  metadata' <- metaToJSON' blockWriter inlineWriter meta'
  let metadata = case fromJSON metadata' of
                   Error _ -> mempty -- TODO warning here? shouldn't happen
                   Success x -> x
  cells <- extractCells opts blocks
  return $ Notebook{
       n_metadata = metadata
     , n_nbformat = (4, 4)
     , n_cells = cells }

extractCells :: PandocMonad m => WriterOptions -> [Block] -> m [Cell a]
extractCells _ [] = return []
extractCells opts (Div (_id,classes,kvs) xs : bs)
  | "cell" `elem` classes
  , "markdown" `elem` classes = do
      let meta = pairsToJSONMeta kvs
      source <- writeMarkdown opts{ writerTemplate = Nothing }
                  (Pandoc nullMeta xs)
      let attachments = Nothing -- TODO add this later
      (Cell{
          c_cell_type = Markdown
        , c_source = Source $ breakLines source
        , c_metadata = meta
        , c_attachments = attachments } :) <$> extractCells opts bs
  | "cell" `elem` classes
  , "code" `elem` classes = do
      let isCode (CodeBlock (_,cl,_) _ ) = "python" `elem` cl
          isCode _                       = False
      let (codeBlocks, _outputBlocks) = partition isCode xs
      let codeContent = mconcat $ intersperse "\n"
              [t | CodeBlock _ t <- codeBlocks]
      let meta = pairsToJSONMeta kvs
      let outputs = mempty -- TODO add this later
      let exeCount = lookup "execution_count" kvs >>= safeRead
      (Cell{
          c_cell_type = Ipynb.Code {
                c_execution_count = exeCount
              , c_outputs = outputs
              }
        , c_source = Source $ breakLines $ T.pack codeContent
        , c_metadata = meta
        , c_attachments = Nothing } :) <$> extractCells opts bs
  | "cell" `elem` classes
  , "raw" `elem` classes =
      case xs of
        [RawBlock (Format f) raw] -> do
          let format' =
                case f of
                  "html"     -> "text/html"
                  "revealjs" -> "text/html"
                  "latex"    -> "text/latex"
                  "markdown" -> "text/markdown"
                  "rst"      -> "text/x-rst"
                  _          -> f
          (Cell{
              c_cell_type = Raw
            , c_source = Source $ breakLines $ T.pack raw
            , c_metadata = M.insert "format"
                             (Aeson.String $ T.pack format') mempty
            , c_attachments = Nothing } :) <$> extractCells opts bs
        _ -> extractCells opts bs
extractCells opts (CodeBlock (_id,classes,kvs) raw : bs)
  | "python" `elem` classes = do
      let meta = pairsToJSONMeta kvs
      let exeCount = lookup "execution_count" kvs >>= safeRead
      (Cell{
          c_cell_type = Ipynb.Code {
                c_execution_count = exeCount
              , c_outputs = []
              }
        , c_source = Source $ breakLines $ T.pack raw
        , c_metadata = meta
        , c_attachments = Nothing } :) <$> extractCells opts bs
extractCells opts (b:bs) = do
      let isCodeOrDiv (CodeBlock (_,cl,_) _) = "python" `elem` cl
          isCodeOrDiv (Div (_,cl,_) _)       = "cell" `elem` cl
          isCodeOrDiv _                      = False
      let (mds, rest) = break (isCodeOrDiv) bs
      let mdBlocks = b:mds
      source <- writeMarkdown opts{ writerTemplate = Nothing }
                  (Pandoc nullMeta mdBlocks)
      let attachments = Nothing -- TODO add this later
      (Cell{
          c_cell_type = Markdown
        , c_source = Source $ breakLines source
        , c_metadata = mempty
        , c_attachments = attachments } :) <$> extractCells opts rest

pairsToJSONMeta :: [(String, String)] -> JSONMeta
pairsToJSONMeta kvs =
  M.fromList [(T.pack k, case v of
                           "true"  -> Bool True
                           "false" -> Bool False
                           _       -> String (T.pack v))
             | (k,v) <- kvs , k /= "execution_count" ]
