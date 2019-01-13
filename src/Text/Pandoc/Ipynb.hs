{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
   Module      : Text.Pandoc.Ipynb
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structure and JSON serializers for ipynb (Jupyter notebook) format.
The format is documented here:
<https://nbformat.readthedocs.io/en/latest/format_description.html>.
We only support v4.  To convert an older notebook to v4 use nbconvert:
@ipython nbconvert --to=notebook testnotebook.ipynb@.
-}
module Text.Pandoc.Ipynb ( Notebook(..)
                         , JSONMeta
                         , Cell(..)
                         , Source(..)
                         , CellType(..)
                         , OutputType(..)
                         , Output(..)
                         , MimeData(..)
                         , MimeBundle(..)
                         , breakLines
                         )
where
import Prelude
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Control.Applicative ((<|>))
import qualified Data.ByteString.Base64 as Base64
import GHC.Generics
import Data.Char (toLower)
import Control.Monad (when)
import Text.Pandoc.MIME (MimeType)

customOptions :: Aeson.Options
customOptions = defaultOptions
                { fieldLabelModifier = drop 2
                , omitNothingFields = True
                , constructorTagModifier = map toLower
                }

data Notebook = Notebook
  { n_metadata       :: JSONMeta
  , n_nbformat       :: Int
  , n_nbformat_minor :: Int
  , n_cells          :: [Cell]
  } deriving (Show, Generic)

instance FromJSON Notebook where
  parseJSON = withObject "Notebook" $ \v -> do
    fmt <- v .:? "nbformat" .!= 0
    when (fmt < 4) $
      fail "only versions > 4 of the Jupyter notebook format are supported"
    fmtminor <- v .:? "nbformat_minor" .!= 0
    metadata <- v .: "metadata" <|> return mempty
    cells <- v .: "cells"
    return $
      Notebook{ n_metadata = metadata
              , n_nbformat = fmt
              , n_nbformat_minor = fmtminor
              , n_cells = cells
              }

instance ToJSON Notebook where
 toEncoding = genericToEncoding customOptions

type JSONMeta = M.Map Text Value

newtype Source = Source{ unSource :: [Text] }
  deriving (Show, Generic, Semigroup, Monoid)

instance FromJSON Source where
  parseJSON v = do
    ts <- parseJSON v <|> (:[]) <$> parseJSON v
    return $ Source ts

instance ToJSON Source where
  toJSON (Source ts) = toJSON ts

data Cell = Cell
  { c_cell_type        :: CellType
  , c_source           :: Source
  , c_metadata         :: JSONMeta
  , c_execution_count  :: Maybe Int
  , c_outputs          :: Maybe [Output]
  , c_attachments      :: Maybe (M.Map Text MimeBundle)
} deriving (Show, Generic)

instance FromJSON Cell where
  parseJSON = genericParseJSON customOptions

-- need manual instance because null execution_count can't
-- be omitted!
instance ToJSON Cell where
 toEncoding c = pairs $
      "cell_type" .= (c_cell_type c)
   <> "source" .= (c_source c)
   <> "metadata" .= (c_metadata c)
   <> case c_cell_type c of
         Code -> "execution_count" .= (c_execution_count c)
         _ -> mempty
   <> maybe mempty ("outputs" .=) (c_outputs c)
   <> maybe mempty ("attachments" .=) (c_attachments c)

data CellType =
    Markdown
  | Raw
  | Code
  deriving (Show, Generic)

instance FromJSON CellType where
  parseJSON = genericParseJSON customOptions

instance ToJSON CellType where
 toEncoding = genericToEncoding customOptions

data OutputType =
    Stream
  | Display_data
  | Execute_result
  deriving (Show, Generic)

instance FromJSON OutputType where
  parseJSON = genericParseJSON customOptions

instance ToJSON OutputType where
 toEncoding = genericToEncoding customOptions

data Output = Output{
    o_output_type     :: OutputType
  , o_name            :: Maybe Text
  , o_text            :: Maybe Source
  , o_data            :: Maybe MimeBundle
  , o_metadata        :: Maybe JSONMeta
  , o_execution_count :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON Output where
  parseJSON = genericParseJSON customOptions

instance ToJSON Output where
 toEncoding = genericToEncoding customOptions

data MimeData =
    BinaryData ByteString
  | TextualData Text
  | JsonData Value
  deriving (Show, Generic)

newtype MimeBundle = MimeBundle{ unMimeBundle :: M.Map MimeType MimeData }
  deriving (Show, Generic, Semigroup, Monoid)

instance FromJSON MimeBundle where
  parseJSON v = do
    m <- parseJSON v >>= mapM pairToMimeData . M.toList
    return $ MimeBundle $ M.fromList m

pairToMimeData :: (MimeType, Value) -> Aeson.Parser (MimeType, MimeData)
pairToMimeData ("text/plain", v) = do
  t <- parseJSON v <|> (mconcat <$> parseJSON v)
  return $ ("text/plain", TextualData t)
pairToMimeData ("application/json", v) = return $ ("application/json", JsonData v)
pairToMimeData (mt, v) = do
  t <- parseJSON v <|> (mconcat <$> parseJSON v)
  return (mt, BinaryData (Base64.decodeLenient . TE.encodeUtf8 $ t))

instance ToJSON MimeBundle where
  toJSON (MimeBundle m) =
    let mimeBundleToValue (BinaryData bs) =
          toJSON (breakLines $ TE.decodeUtf8 $ Base64.joinWith "\n" 64 $
                  Base64.encode bs)
        mimeBundleToValue (JsonData v) = v
        mimeBundleToValue (TextualData t) = toJSON (breakLines t)
    in  toJSON $ M.map mimeBundleToValue m

breakLines :: Text -> [Text]
breakLines t =
  let (x, y) = T.break (=='\n') t
  in  case T.uncons y of
         Nothing -> [x]
         Just (c, rest) -> (x <> T.singleton c) : breakLines rest

{- --- for testing only:
import qualified Data.ByteString.Lazy as BL

readNotebookFile :: FilePath -> IO Notebook
readNotebookFile fp = do
  bs <- BL.readFile fp
  case eitherDecode bs of
    Right nb -> return nb
    Left err -> error err

writeNotebookFile :: FilePath -> Notebook -> IO ()
writeNotebookFile fp = BL.writeFile fp . encode
-}
