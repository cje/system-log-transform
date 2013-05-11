module Types where 

import Data.Text as DT


-- Let's make types to contain each thing
-- we plan to parse.
data Month =   January | February | March | April | May | June 
             | July | August | September | October | November | December
             deriving (Eq, Ord, Show) 
type Day = Integer 
type Hour = Integer 
type Minute= Integer
type Second = Integer
data Date = Date Month Day deriving (Eq, Ord, Show) 
data Time = Time Hour Minute Second deriving (Eq, Ord, Show) 
data Datetime = Datetime Date Time deriving (Eq, Ord, Show) 
type Octet = Integer 
data IPv4 = IPv4 Octet Octet Octet Octet deriving (Eq, Ord, Show)  
data TermC = TextC Text 
             | IntegerC Integer 
             | IPv4C IPv4
             | DatetimeC Datetime
             | EndOfLineC  
               deriving (Show) 

instance Eq TermC where
-- Make equal if node types are equal 
    TextC a     == TextC b     = a == b  
    IntegerC a  == IntegerC b  = a == b  
    IPv4C a     == IPv4C b     = a == b  
    DatetimeC a == DatetimeC b = a == b  
    EndOfLineC  == EndOfLineC  = True  
-- If they aren't the same type, not equal
    _           == _           = False 

instance Ord TermC where
    TextC a     > TextC b      = a > b 
    IntegerC a  > IntegerC b   = a > b 
    IPv4C a     > IPv4C b      = a > b 
    DatetimeC a > DatetimeC b  = a > b 
    TextC a     > IntegerC b   = True
    IntegerC a  > IPv4C b      = True
    IPv4C a     > DatetimeC b  = True
    DatetimeC a > EndOfLineC   = True
    _           > _            = False 

