module TParser where 
import Parser 
import Types 

import Control.Applicative 
import Control.Monad 
import Data.Attoparsec.Text as AP 
import Data.Text            as DT  
import Data.Either 
import Test.HUnit  
import Test.HUnit.Base
import Distribution.TestSuite 


-- parseText :: Text -> Either String TermC 
-- parseText l = eitherResult $ AP.feed ( AP.parse (term) l) DT.empty


--parseLine :: Text -> Either String [TermC] 
--parseLine l = eitherResult $ AP.feed ( AP.parse (AP.manyTill term AP.endOfInput) l) DT.empty


-- 
--

parseEnds  :: Parser TermC 
parseEnds  = do AP.takeWhile AP.isHorizontalSpace  
                AP.choice [ 
                            AP.endOfLine *> pure EndOfLineC 
                          ]

parseEndsRepeat l = eitherResult $ AP.feed ( AP.parse (AP.manyTill parseEnds AP.endOfInput) l) DT.empty 

testParseEndsCombination  = TestCase $ do let nlText = "\n\n" 
                                          let nlRef = [EndOfLineC, EndOfLineC]   
                                          case parseEndsRepeat (DT.pack nlText) of 
                                                Left err -> assertFailure err 
                                                Right res -> assertEqual "Parse newlines" nlRef res  

--
--

testParseDatetime = TestCase $ do let dateText = "May  6 21:33:55"
                                  let dateRef = Date May 6 
                                  let timeRef = Time 21 33 55 
                                  let dateTimeRef = Datetime dateRef timeRef
                                  let termRef = DatetimeC dateTimeRef 
                                  case parseText (DT.pack dateText) of
                                       Left err -> assertFailure err  
                                       Right res -> assertEqual "Parse a date" termRef res 

testParseInteger = TestCase $ do let integerText = " 12345 " 
                                 let integerRef = 12345
                                 let integerCRef = IntegerC integerRef 
                                 case parseText (DT.pack integerText) of 
                                      Left err -> assertFailure err 
                                      Right res -> assertEqual "Parse an integer" integerCRef res
testParseIPv4 = TestCase $ do let ipv4Text = " 1.2.3.4 4 asasd" 
                              let ipv4Ref = IPv4C (IPv4 1 2 3 4)  
                              case parseText (DT.pack ipv4Text) of 
                                   Left err -> assertFailure err 
                                   Right res -> assertEqual "Parse an IPv4 address" ipv4Ref res 

testParseText = TestCase $ do let textText = " 12x:" 
                              let textTextRef = TextC (DT.pack "12x:")  
                              case parseText (DT.pack textText) of 
                                   Left err -> assertFailure err 
                                   Right res -> assertEqual "Parse text" textTextRef res 

-- The parseLine interface should break things into lines, removing any newlines 
-- In theory, this means that the parser will never see a \n. 
-- This case exists to ensure that the parser does something sensible with newlines anyway. 
testParseLF = TestCase $ do let textLF = "  \t   \n" 
                            let textLFRef = TextC (DT.pack "\n")  
                            case parseText (DT.pack textLF) of 
                                   Left err -> assertFailure err 
                                   Right res -> assertEqual "Parse LF" textLFRef res 

testParseLineSimple = TestCase $ do let text = " abc" 
                                    let textRef = [TextC (DT.pack "abc")] :: [TermC]  
                                    let res = parseLine (DT.pack text)
                                    case lefts (res) of
                                           [] ->  assertEqual "Parse Simple" textRef (Prelude.head (rights res))  
                                           otherwise -> assertFailure $ show $ lefts res  


testParseLineEmpty = TestCase $ do let text = "" 
                                   let textRef = [] :: [TermC]  
                                   let res = parseLine (DT.pack text)
                                   case lefts (res) of
                                           [] ->  assertEqual "Parse Empty" textRef (Prelude.concat (rights res))  
                                           otherwise -> assertFailure $ show $ lefts res  

-- Check behaviour for empty lines 
testParseEndOfLine = TestCase $ do let nlText = " \na\n" 
                                   let nlRef = [TextC (DT.pack ""), TextC (DT.pack "a")]   
                                   let res = parseLine (DT.pack nlText)
                                   case lefts (res) of
                                   -- Successful parse
                                           [] ->  assertEqual "Parse EOL" nlRef (Prelude.concat (rights res))  
                                   -- Failed parse 
                                           otherwise -> assertFailure $ show $ lefts res  


testParseLine = TestCase $ do let l = "May  8 23:39:47 bracket.local sandboxd[39866] ([39865]): mdworker(39865) deny mach-lookup com.apple.ls.boxd" 
                              let dateRef = Date May 8  
                              let timeRef = Time 23 39 47 
                              let dateTimeRef = Datetime dateRef timeRef
                              let termRef = DatetimeC dateTimeRef 
                              let res = parseLine (DT.pack l)
                              case lefts (res) of
                                   -- Successful parse
                                           [] ->  assertEqual "Parse a line"  
                                                     [  termRef
                                                      , TextC (DT.pack "bracket.local")
                                                      , TextC (DT.pack "sandboxd[39866]")  
                                                      , TextC (DT.pack "([39865]):") 
                                                      , TextC (DT.pack "mdworker(39865)")
                                                      , TextC (DT.pack "deny") 
                                                      , TextC (DT.pack "mach-lookup") 
                                                      , TextC (DT.pack "com.apple.ls.boxd") 
                                                      ] 
                                                  (Prelude.concat (rights res))  
                                   -- Failed parse 
                                           otherwise -> assertFailure $ show $ lefts res  
