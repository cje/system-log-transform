module Main (
   main 
) where

import TParser 
import Test.Framework
import Test.Framework.Providers.HUnit 
--import Test.HUnit  
-- import Test.HUnit.Base
-- import Distribution.TestSuite as CT 



--main :: IO () 
-- main = runTestTT parserTests >> print "Done" 
           
main :: IO () 
main = defaultMain parseTests 

parseTests :: [Test] 
parseTests = [
         testGroup "Single Term Parse" 
          [ 
            testGroup "Parse Date and Time into TermC" $ (hUnitTestToTests testParseDatetime)
          , testGroup "Parse Integer into TermC" $ (hUnitTestToTests testParseInteger) 
          , testGroup "Parse IPv4 into TermC" $ (hUnitTestToTests testParseIPv4) 
          , testGroup "Parse Text into TermC" $ (hUnitTestToTests testParseText)
          , testGroup "Parse LF into TermC" $ (hUnitTestToTests testParseLF)
          ], 
          testGroup "Multiple Term Parse"
          [ 
            testGroup "Parse end of lines using minimal choice parser" $ (hUnitTestToTests testParseEndsCombination) 
          , testGroup "Parse end of lines - testParseEndOfLine" $ (hUnitTestToTests testParseEndOfLine)
          , testGroup  "Parse test for simple string" $ (hUnitTestToTests testParseLineSimple) 
          , testGroup  "Parse test for empty string" $ (hUnitTestToTests testParseLineEmpty) 
          , testGroup "Parse a syslog line, OSX" $ (hUnitTestToTests testParseLine)  
          ]
        ] 
