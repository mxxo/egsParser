{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib where

import Control.Applicative hiding (some, many)
import Control.Applicative.Permutations
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
-- import Text.Megaparsec.Debug -- dbg "parse_component_name" added in the parser shows each step
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Particle =
    Electron
  | Photon
  | Positron
  deriving (Eq, Show)

data Source = Source
  { name :: Text
  , library :: Text
  , particle :: Particle
  } deriving (Eq, Show)

data Egsinp = Egsinp
  { sources :: [Source]
  --, ausgab   :: Maybe Ausgab
  --, control  :: Control
  --, geometry :: Geometry
  --, media    :: Maybe Media
  --, variance :: Maybe Variance
  --, view     :: Maybe View
  } deriving (Eq, Show)

pPermGeometry :: Parser (Text, Maybe Text)
pPermGeometry = runPermutation $
    (,) <$> toPermutation (T.singleton <$> char ':')
        <*> toPermutation (optional $ T.singleton <$> (char '#'))

--pGeometry :: Parser (Text, Text)
--pGeometry =  do
--       a <- T.singleton <$> (char ':')
--       --a <- T.pack <$> (char ':')
--       --b <- T.pack <$> (char '#')
--       b <- T.singleton <$> (char '#')
--       pure (a, b)

pSource :: Parser Source
pSource = undefined

pEgsinp :: Parser Egsinp
pEgsinp = do
      sources <- some pSource <?> "source definition" -- at least one source

      pure Egsinp {..} -- form the input sources from our parsed components
--
--    uriScheme <- pScheme <?> "valid scheme" -- <?> is synonym for label primitive - gives nicer parse errors
--    void (char ':') -- scheme must be followed by a colon.
--    uriAuthority <- optional $ do -- could also wrap with try since it's a composite parser, not a primitive. -> however we have a somewhat confusing error message in that case
--        void (string "//")
--        authUser <- optional . try $ do -- same setup as ^ with optional arg wrapped in try.
--            user <- T.pack <$> some alphaNumChar <?> "username" -- some is like many but at least once.
--            void (char ':')
--            password <- T.pack <$> some alphaNumChar <?> "password"
--            void (char '@')
--            pure (user, password)
--        authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
--        authPort <- optional (char ':' *> label "port number" L.decimal) -- no try here, we know port must follow so we "demand a decimal using L.decimal".
--        pure Authority {..} -- uses the RecordWildCards extension (notice the names of the variables
--            -- if authHost was named authHost1 the record wouldn't populate the authHost field).
--    -- add our request parse steps
--    uriRequest <- do
--        optional $ void (string "/") -- optional / before the path
--        reqPath <- T.pack <$> some (alphaNumChar <|> char '/' <|> char '.') <?> "path"
--        reqQuery <- optional $ do
--            void (char '?')
--            query <- T.pack <$> some (alphaNumChar <|> char '=' <|> char '&') <?> "query"
--            pure query
--        reqFragment <- optional $ do
--            void (char '#')
--            fragment <- T.pack <$> (some printChar) <?> "fragment"
--            pure fragment
--        pure Request {..}
--    pure Uri {..}

