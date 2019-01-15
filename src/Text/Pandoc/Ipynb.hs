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
                         , Output(..)
                         , MimeData(..)
                         , MimeBundle(..)
                         , breakLines
                         , encodeNotebook
                         )
where
import Prelude
import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty',
                                 keyOrder, Indent(Spaces))
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

encodeNotebook :: Notebook -> Text
encodeNotebook = TE.decodeUtf8 . BL.toStrict .
  encodePretty' defConfig{
      confIndent  = Spaces 1,
      confCompare = keyOrder
           [ "cells", "nbformat", "nbformat_minor",
             "cell_type", "output_type",
             "execution_count", "metadata",
             "outputs", "source",
             "data", "name", "text" ] }

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
 toJSON n = object
   [ "nbformat" .= (n_nbformat n)
   , "nbformat_minor" .= (n_nbformat_minor n)
   , "metadata" .= (n_metadata n)
   , "cells" .= (n_cells n)
   ]

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
 toJSON c = object $
   [ "cell_type" .= (c_cell_type c)
   , "source" .= (c_source c)
   , "metadata" .= (c_metadata c)
   ] ++
   case c_cell_type c of
     Code ->
      [ "execution_count" .= (c_execution_count c)
      , "outputs" .= fromMaybe [] (c_outputs c) ]
     Markdown ->
      maybe [] (\x -> ["attachments" .= x]) (c_attachments c)
     _ -> []

data CellType =
    Markdown
  | Raw
  | Code
  deriving (Show, Generic, Eq)

instance FromJSON CellType where
  parseJSON = genericParseJSON customOptions

instance ToJSON CellType where
 toJSON Markdown = String "markdown"
 toJSON Raw      = String "raw"
 toJSON Code     = String "code"

data Output =
    Stream
    { s_name            :: Text
    , s_text            :: Source }
  | Display_data
    { d_data            :: MimeBundle
    , d_metadata        :: JSONMeta
    }
  | Execute_result
    { e_execution_count :: Int
    , e_data            :: MimeBundle
    , e_metadata        :: JSONMeta
    }
  deriving (Show, Generic)

instance FromJSON Output where
  parseJSON = withObject "Object" $ \v -> do
    ty <- v .: "output_type"
    case ty of
      "stream" ->
        Stream
          <$> v .: "name"
          <*> v .: "text"
      "display_data" ->
        Display_data
          <$> v .: "data"
          <*> v .: "metadata"
      "execute_result" ->
        Execute_result
          <$> v .: "execution_count"
          <*> v .: "data"
          <*> v .: "metadata"
      _ -> fail $ "Unknown object_type " ++ ty

instance ToJSON Output where
  toJSON s@(Stream{}) = object
    [ "output_type" .= ("stream" :: Text)
    , "name" .= s_name s
    , "text" .= s_text s
    ]
  toJSON d@(Display_data{}) = object
    [ "output_type" .= ("display_data" :: Text)
    , "data" .= d_data d
    , "metadata" .= d_metadata d
    ]
  toJSON e@(Execute_result{}) = object
    [ "output_type" .= ("execute_result" :: Text)
    , "execution_count" .= e_execution_count e
    , "data" .= e_data e
    , "metadata" .= e_metadata e
    ]

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
