module Parser 
( parseLine
, parseText) 
where 

import Data.Text as DT
import Data.Attoparsec.Text as AP  
import Data.Attoparsec.Combinator as APC   
import Control.Applicative 
import Types 

term :: Parser TermC 
term = do AP.takeWhile AP.isHorizontalSpace  
          AP.choice [ -- These are evaluated in the order they are listed
                      -- and the first parser to succeed is the return value 
--                       AP.endOfLine *> pure EndOfLineC 
--                     , AP.endOfInput *> pure EndOfLineC 
                       DatetimeC <$> datetime
                     , IPv4C <$> ipv4 
                     , IntegerC <$> integer 
                     , TextC <$> text
                    ] 



-- RFC 3164 style date parser.

datetime :: Parser Datetime  
datetime = do d <- date
              AP.skipSpace 
              t <- time 
              return $ Datetime (d::Date) (t::Time)  

-- Date -- 
date :: Parser Date
date = do m <- month 
          AP.skipSpace 
          d <- day
          return (Date m d)  


month :: Parser Month 
month = jan <|> feb <|> mar <|> apr <|> may <|> jun <|> jul <|> aug <|> sep <|> oct <|> nov <|> dec 
       where jan = AP.stringCI (pack "Jan") *> pure January  
             feb = AP.stringCI (pack "Feb") *> pure February 
             mar = AP.stringCI (pack "Mar")  *> pure March  
             apr = AP.stringCI (pack "Apr")  *> pure April  
             may = AP.stringCI (pack "May")  *> pure May 
             jun = AP.stringCI (pack "Jun")  *> pure June
             jul = AP.stringCI (pack "Jul") *> pure July 
             aug = AP.stringCI (pack "Aug") *> pure August 
             sep = AP.stringCI (pack "Sep") *> pure September 
             oct = AP.stringCI (pack "Oct")  *> pure October 
             nov = AP.stringCI (pack "Nov") *> pure November
             dec = AP.stringCI (pack "Dec") *> pure December 

day :: Parser Day 
day = fmap toInteger decimal

-- End date -- 
-- Time -- 

time :: Parser Time 
time = do h <- hour 
          char ':' 
          m <- minute 
          char ':' 
          s <- second
          return (Time h m s)  

hour :: Parser Hour 
hour = fmap toInteger decimal 

minute :: Parser Minute  
minute = fmap toInteger decimal  

second :: Parser Second 
second =  fmap toInteger decimal  

-- End time -- 

-- IPv4 parser 
ipv4 :: Parser IPv4 
ipv4 = do a <- octet
          char '.' 
          b <- octet 
          char '.' 
          c <- octet 
          char '.' 
          d <- octet
          return $ IPv4 a b c d  
          
octet :: Parser Octet  
octet =  fmap ( toInteger) decimal
            --where check x | x <= 255  = x  
            --              | otherwise = Control.Applicative.empty  
-- End IPv4 parser 

-- Integer -- 
integer :: Parser Integer 
integer = do i <- decimal 
             char ' ' 
             return $ fromInteger i  
-- End Integer 

-- Text --
-- Tokenize by spaces.
text :: Parser Text 
text = AP.takeWhile (notInClass "\t ")  
-- End Text -- 



parseLine :: Text -> [Either String [TermC]]  
parseLine l = do let lineList = DT.lines l
                 -- Parse each line into a list of terms. 
                 let parseTerms t = parse (AP.manyTill term AP.endOfInput) t  
                 -- It is necessary to feed an empty string to the parser after parsing 
                 -- to avoid partial results. 
                 let parseF u =  AP.feed (parseTerms u) DT.empty 
                 
                 eitherResult <$> Prelude.map parseF lineList 


parseText :: Text -> Either String TermC 
parseText l = eitherResult $ AP.feed ( AP.parse (term) l) DT.empty

