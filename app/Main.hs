{-# LANGUAGE OverloadedStrings #-}

module Main where

-- external dependencies
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (Handle, IOMode(..), openFile)
import Text.Megaparsec (parseTest)
import qualified Data.Text as T
import Data.Text.IO (hGetContents)

-- our library
import Lib (egsinpParser)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        die "usage: ./Main.exe file.egsinp"
    else do
        let input_file = head args
        hndl <- openFile input_file ReadMode
        contents <- hGetContents $ hndl
        -- print contents -- dbg
        parseTest egsinpParser contents

