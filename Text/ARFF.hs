-- | Generate Attribute-Relation File Format (ARFF) files.
--
-- ARFF files are used by the WEKA data mining and machine learning framework.
--
-- <http://www.cs.waikato.ac.nz/~ml/weka/>
--
module Text.ARFF (
    -- * ARFF relations
    Relation,
    relation,
    encode,
    -- * Attribute constructors
    Attribute,
    a_string,
    a_real,
    a_int,
    a_nominal,
    a_nominalFromTo,
    a_dateFormat,
    a_date,
    -- * Value constructors
    Value,
    missing,
    string,
    real,
    int,
    nominal,
    date
) where

import Control.Arrow                    ((<<<))
import Data.Binary.Put                  (Put, runPut)
import Data.Maybe                       (mapMaybe)
import Data.ByteString.Lazy             (ByteString)
import Data.ByteString.Lex.Double       (readDouble)
import Data.Time.Format                 (FormatTime, formatTime)
import System.Locale                    (defaultTimeLocale)
import Text.Show.ByteString             (putAscii, putAsciiStr, showp, unlinesP, unwordsP)
import qualified Text.Show.ByteString   as B

-- | Show a 'Value' in the 'Put' monad.
type Putter = Value -> Put

-- | ARFF attribute type.
data Type =
    StringAttribute  { put :: Putter }
  | RealAttribute    { put :: Putter }
  | IntegerAttribute { put :: Putter }
  | forall a . Show a => NominalAttribute { put :: Putter, enum :: [a] }
  | DateAttribute    { put :: Putter, format :: String }

-- | Attribute with associated type and name.
data Attribute = Attribute { attrType :: Type, attrName :: String }

-- | ARFF value type.
data Value =
    MissingValue
  | StringValue  { fromStringValue  :: String }
  | RealValue    { fromRealValue    :: Double }
  | IntegerValue { fromIntegerValue :: Integer }
  | forall a . (Show a)       => NominalValue { fromNominalValue :: a }
  | forall a . (FormatTime a) => DateValue    { fromDateValue    :: a }

-- | ARFF relation.
data Relation = Relation
  { name       :: String,
    attributes :: [Attribute],
    values     :: [[Value]] }

-- | Construct a relation from a name, attributes and values.
relation :: String -> [Attribute] -> [[Value]] -> Relation
relation = Relation

-- | Convert a 'Relation' to its textual representation.
encode :: Relation -> ByteString
encode = runPut . putRelation

intersperseP :: Put -> [Put] -> Put
intersperseP _ []     = return ()
intersperseP _ (x:[]) = showp x
intersperseP d (x:xs) = x >> d >> intersperseP d xs

uncommaP :: [Put] -> Put
uncommaP = intersperseP (putAscii ',')

-- encloseP :: Put -> Put -> Put
-- encloseP e p = e >> p >> e

defaultDateFormat :: String
defaultDateFormat = "yyyy-MM-dd'T'HH:mm:ss"

fromISODateFormat :: String -> String
fromISODateFormat fmt | fmt == defaultDateFormat = "%Y-%m-%dT%X"
fromISODateFormat _ = error "BUG[fromISODateFormat]: ISO date formats not yet implemented"

putType :: Type -> Put
putType (StringAttribute _)     = putAsciiStr "string"
putType (RealAttribute _)       = putAsciiStr "real"
putType (IntegerAttribute _)    = putAsciiStr "integer"
putType (NominalAttribute _ es) = putAscii '{' >> (uncommaP <<< map (putAsciiStr.show)) es >> putAscii '}'
putType (DateAttribute _ fmt)   = unwordsP [putAsciiStr "date", showp fmt]

putAttribute :: Attribute -> Put
putAttribute (Attribute atype name) = unwordsP [putAsciiStr "@attribute", putAsciiStr name, putType atype]

putMissingValue :: Put
putMissingValue = putAscii '?'

instance Show (Value) where
    show (StringValue _)  = "String"
    show (RealValue _)    = "Real"
    show (IntegerValue _) = "Integer"
    show (NominalValue _) = "Nominal"
    show (DateValue _)    = "Date"

typeError :: String -> String -> Value -> Put
typeError attrName attrType value =
    error $ "Text.ARFF.encode: type mismatch -- attribute \"" ++ attrName ++ "\""
         ++ " has type " ++ attrType ++ ","
         ++ " but got " ++ (show value)

putString :: String -> Value -> Put
putString _ (StringValue x)   = showp x
putString _ MissingValue      = putMissingValue
putString a v                 = typeError a "String" v

putReal :: String -> Value -> Put
putReal _ (RealValue x)       = showp x
putReal _ MissingValue        = putMissingValue
putReal a v                   = typeError a "Real" v

putInteger :: String -> Value -> Put
putInteger _ (IntegerValue x) = showp x
putInteger _ MissingValue     = putMissingValue
putInteger a v                = typeError a "Integer" v

putNominal :: String -> Value -> Put
putNominal _ (NominalValue x) = putAsciiStr (show x)
putNominal _ MissingValue     = putMissingValue
putNominal a v                = typeError a "Nominal" v

putDate :: String -> String -> Value -> Put
putDate fmt a (DateValue d)   = showp (formatTime defaultTimeLocale fmt d)
putDate _   a MissingValue    = putMissingValue
putDate _   a v               = typeError a "Date" v

putRelation :: Relation -> Put
putRelation (Relation name attrs values) =
    unlinesP $
           [ unwordsP [putAsciiStr "@relation", putAsciiStr name] ]
        ++ map putAttribute attrs
        ++ [ putAsciiStr "@data" ]
        ++ map (uncommaP . zipWith ($) putters) values
     where putters = map (put . attrType) attrs

-- | String attribute constructor.
a_string :: String -> Attribute
a_string a = Attribute (StringAttribute (putString a)) a

-- | Real attribute constructor.
a_real :: String -> Attribute
a_real a = Attribute (RealAttribute (putReal a)) a

-- | Integer attribute constructor.
a_int :: String -> Attribute
a_int a = Attribute (IntegerAttribute (putInteger a)) a

-- | Nominal attribute constructor.
a_nominal :: (Show a) => [a] -> String -> Attribute
a_nominal xs a = Attribute (NominalAttribute (putNominal a) xs) a

-- | Nominal attribute constructor.
a_nominalFromTo :: (Enum a, Show a) => a -> a -> String -> Attribute
a_nominalFromTo lo hi = a_nominal (enumFromTo lo hi)

-- | Date attribute constructor.
-- Currently only supports a default date format, since we're lacking an
-- ISO-8601 format string parser.
a_dateFormat :: String -> String -> Attribute
a_dateFormat fmt a = Attribute (DateAttribute (putDate (fromISODateFormat fmt) a) fmt) a

-- | Construct a date attribute with the default date format
-- @yyyy-MM-dd'T'HH:mm:ss@.
a_date :: String -> Attribute
a_date = a_dateFormat defaultDateFormat

-- | Missing value.
missing :: Value
missing = MissingValue

-- | String value constructor.
string :: String -> Value
string = StringValue

-- | Real value constructor.
real :: Double -> Value
real = RealValue

-- | Integer value constructor.
int :: Integer -> Value
int = IntegerValue

-- | Nominal value constructor.
nominal :: (Show a) => a -> Value
nominal = NominalValue

-- | Date value constructor
date :: (FormatTime a) => a -> Value
date = DateValue
