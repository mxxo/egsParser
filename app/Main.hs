{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import qualified Data.Text as T

import Lex
import Lib

-- pipe input file to the parser to test it
main :: IO ()
main = do
    input <- getContents
    parseTest pPermGeometry (T.pack input)

