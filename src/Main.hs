{-# LANGUAGE QuasiQuotes #-}
import Types 
import Parser
import Data.Either
import Data.Text as DT
import Data.Attoparsec.Text as AP  
import Data.SuffixTree 
import System.Exit 

main = do f <- readFile "./sample.line" 
          let p = (parseLine . DT.pack) f 
          case lefts (p) of
               [] ->  print $ rights p
               otherwise -> exitWith (ExitFailure 1)



