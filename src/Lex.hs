{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module Lex (ausgab_block, spaceConsumer, symbol) where

import Control.Applicative (empty) -- hiding (some, many)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (between, Parsec)
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
    -- blockCmnt = L.skipBlockComment "#" "\n" -- tried to be from the pound to the end of the line but it didn't work... I think have to lex after every word match

-- a lexeme is assumed to not have any whitespace in front but perhaps following
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer -- uses our custom space consumer

-- a symbol is some fixed string, followed by whitespace
symbol :: Text-> Parser Text
symbol = L.symbol spaceConsumer

-- define the different blocks
block_ends :: Parser a -> (Parser a, Parser a)
block_ends name =
    ( symbol ":" *> symbol "start" *> name <* symbol ":"
    , symbol ":" *> symbol "stop"   *> name <* symbol ":" )

-- ausgab_start = fst $ block_ends (symbol "ausgab" <* symbol "object" <* symbol "definition")

ausgab_block :: Parser a -> Parser a
ausgab_block = between ausgab_start ausgab_end
                 where
                   (ausgab_start, ausgab_end) = block_ends (symbol "ausgab" <* symbol "object" <* symbol "definition")

--ausgab_start = void $ symbol ":" *> symbol "start" *> symbol "ausgab" *> symbol "object" *> symbol "definition" *> symbol ":"
-- ausgab_end   = void $ symbol ":" *> symbol "stop" *> symbol "ausgab" *> symbol "object" *> symbol "definition" *> symbol ":"

-- it's not really an error to have : start instead of :start
geometry_start = symbol ":" *> symbol "start" *> symbol "geometry" *> "definition" *> symbol ":"
