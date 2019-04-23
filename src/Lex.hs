{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module Lex (spaceConsumer, symbol) where

import Control.Applicative (empty) -- hiding (some, many)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space1)
-- import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- consumes all whitespace from the input
--   based off Mark Karpov's simple expression parser:
--   https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
spaceConsumer :: Parser ()
-- L.space is a general space consumer constructor
-- @arg 2: space1 is at least one, space is 0 or more
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = empty -- no block comments in EGS input files, pass the empty Applicative instance

-- a lexeme is assumed to not have any whitespace in front but perhaps following
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer -- uses our custom space consumer

-- a symbol is some fixed string, followed by whitespace
symbol :: Text-> Parser Text
symbol = L.symbol spaceConsumer

-- define the different blocks for
