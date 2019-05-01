{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib where -- (egsinpParser)

-- import Control.Applicative hiding (some, many)
-- import Control.Applicative.Combinators (between)
import Control.Applicative.Permutations (toPermutation, runPermutation)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
-- import Text.Megaparsec hiding (State)
import Text.Megaparsec (between, choice, eof, label, many, optional, Parsec, parseTest, satisfy, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, alphaNumChar, space, string)
import Text.Megaparsec.Debug -- dbg "parse_component_name" added in the parser shows each step
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Lex (ausgab_block, integer, float, spaceConsumer, symbol)

type Parser = Parsec Void Text

data Particle =
    Electron
  | Photon
  | Positron
  deriving (Eq, Show)

data AusgabType
  = TrackScoring
  | DoseScoring -- add more parameters here
  deriving (Eq, Show)

pAusgabType :: Parser AusgabType
pAusgabType = choice
-- maybe use symbol instead?
  [ TrackScoring <$ symbol "egs_track_scoring"
  , DoseScoring  <$ symbol "egs_dose_scoring" ]

pParticle :: Parser Particle
pParticle = do
    void (symbol "charge")
    void equals
    choice
      [ Electron <$ symbol "-1"
      , Photon   <$ symbol  "0"
      , Positron <$ symbol  "1" ]

-- assignment :: Text -> Parser Text
-- assignment name = T.pack <$> (symbol name <*> assign <*> (some alphaNumChar))

--spaced_words :: [a] -> Parser a
--spaced_words []     = spaceConsumer -- clear the whitespace at the end of a line
--spaced_words (x:xs) = string x *> spaceConsumer *> spaced_words xs

-- geometry block is anything inside the delimiters
geometryBlock :: Parser a -> Parser a
geometryBlock = between (symbol ":" *> symbol "start" *> symbol "geometry" *> symbol "block" *> symbol ":")
                        (symbol ":" *> symbol "stop"   *> symbol "geometry" *> symbol "block" *> symbol ":")

-- all the symbols to handle whitespace might be unnecessary but its a first try
-- can successfully parse something inside the delimiters, ex below

-- $ parseTest (geometryBlock pPermGeometry <* eof) ":start geometry block:&#:end geometry block:"
--("&",Just "#")

-- output objects ("ausgab" == German for "output")
data Ausgab = Ausgab
  { objectName :: Text
  , ausgabType :: AusgabType
  } deriving (Eq, Show)
pAusgabBlock :: Parser (Maybe [Ausgab])
pAusgabBlock = dbg "outer block" . optional $ do
    dbg "outer header" (symbol ":" *> symbol "start" *> symbol "ausgab" *> symbol "object" *> symbol "definition" *> symbol ":")
    ausgabs <- some pAusgabObject <?> "ausgab parameter assignment"
    -- optional ((ausgab_block (many pAusgabObject)))
    dbg "outer footer" (symbol ":" *> symbol "stop" *> symbol "ausgab" *> symbol "object" *> symbol "definition" *> symbol ":")
    pure ausgabs


--pAusgabBlock :: Parser (Maybe [Ausgab])
--pAusgabBlock = --dbg "ausgab block" . optional $ do
--    -- void $ dbg "outer header" (symbol ":" *> symbol "start" *> symbol "ausgab" *> symbol "object" *> symbol "definition" *> symbol ":")
--    --ausgabs <- ausgab_block (many pAusgabObject)
--    optional ( dbg "outer block" (ausgab_block (many pAusgabObject)))
--    -- void $ dbg "outer footer" (symbol ":" *> symbol "stop" *> symbol "ausgab" *> symbol "object" *> symbol "definition" *> symbol ":")
--    --pure ausgabs

newline :: Parser Text
newline = symbol ""

equals :: Parser Text
equals = symbol "="

pAusgabObject :: Parser Ausgab
pAusgabObject = try $ do -- try is critical here -> need to be able to backtrack if there aren't any objects afterwards
    void $ dbg "header" (symbol ":" *> symbol "start" *> symbol "ausgab" *> symbol "object" *> symbol ":")
    ausgab <- dbg "ausgab obj" pAusgab <?> "ausgab object"
    void (symbol "")
    void $ dbg "footer" (symbol ":" *> symbol "stop" *> symbol "ausgab" *> symbol "object" *> symbol ":")
    -- void $ optional (symbol "")
    pure ausgab

pAusgab :: Parser Ausgab
pAusgab = runPermutation $
    Ausgab <$> toPermutation nameParser -- <$> toPermutation (T.pack <$> (symbol "name" *> symbol "=" *> some (alphaNumChar <|> space)))
           <*> toPermutation (label "library name" typeParser)
          where
            nameParser = do
                symbol "name" *> symbol "="
                T.pack <$> some alphaNumChar <* symbol "" <?> "ausgab object name"
            typeParser = do
                symbol "library" *> symbol "=" <?> "expecting library"
                pAusgabType <?> "ausgab type"

           -- <*> toPermutation (T.pack <$> (symbol "library" *> symbol "=" *> some alphaNumChar))

data Source = Source
  { name :: Text
  , library :: Text
  , particle :: Particle
  } deriving (Eq, Show)

data Egsinp = Egsinp
  { sources :: [Source]
  , input_ausgab  :: Maybe [Ausgab] -- [Ausgab]
  --, control  :: Control
  --, geometry :: Geometry
  --, media    :: Maybe Media
  --, variance :: Maybe Variance
  --, view     :: Maybe View
  } deriving (Eq, Show)

-- first level permutation
pPermGeometry :: Parser (Text, Maybe Text)
pPermGeometry = runPermutation $
    (,) <$> toPermutation (T.singleton <$> char '&')
        <*> toPermutation (optional $ T.singleton <$> (char '#'))

-- attempt to avoid repetition of <*> and toPermutation -> seems to require
-- some longwinded typeclass constraints here. If it gets bad enough maybe
-- we make a variadic function
--
-- pPermGeometry' :: Parser (Text, Text)
-- pPermGeometry' = runPermutation $
--     liftA2 (,) (T.singleton <$> char '&') (T.singleton <$> char '#')

-- list attempt -> didn't work because lists must be all the same type
-- permList =
--     [ T.singleton <$> char '&'
--     , optional $ T.singleton <$> char '#' ]

-- other attempt using tuple, doesn't seem to be an easy way to generalize to higher arity tuples
--perms :: (Parser Text, Parser (Maybe Text))
--perms =
--  ( T.singleton <$> char '&'
--  , optional $ T.singleton <$> char '#' )
-- permutation of permutations

pGen = runPermutation $
    (,) <$> toPermutation (pPermGeometry)
        <*> toPermutation (T.singleton <$> char '%')



-- in order combined with permutations
pOrdered = do
    a <- T.singleton <$> (char '/')
    b <- pPermGeometry
    c <- T.singleton <$> (char '|')
    pure (a,b,c)

-- pPerm = Parser

--pGeometry :: Parser (Text, Text)
--pGeometry =  do
--       a <- T.singleton <$> (char ':')
--       --a <- T.pack <$> (char ':')
--       --b <- T.pack <$> (char '#')
--       b <- T.singleton <$> (char '#')
--       pure (a, b)

pSource :: Parser Source
pSource = undefined

-- removes all whitespace before and ensures the file has an eof
egsinpParser :: Parser Egsinp
egsinpParser = between spaceConsumer eof pEgsinp

pEgsinp :: Parser Egsinp
pEgsinp = do
      input_ausgab <- pAusgabBlock <?> "ausgab object definition" -- at least one source
      sources <- pure []
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

