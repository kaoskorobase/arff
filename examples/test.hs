import Data.ByteString.Lazy as B
import Data.Time            (UTCTime, addUTCTime, getCurrentTime)
import Text.ARFF            (date, int, missing, nominal, real, string)
import qualified Text.ARFF  as ARFF
import Prelude              hiding (Show)
import qualified Prelude    as P
import Text.Show.ByteString (Show(..), putAsciiStr)

data Strength =
    ExtremelyLow
  | Low
  | Medium
  | High
  | ExtremelyHigh
  deriving (Enum, Eq, P.Show)

instance Show Strength where
    showp = putAsciiStr . show

relation :: UTCTime -> ARFF.Relation
relation t =
    ARFF.relation "Prosemus"
        -- Header
       [ARFF.a_date "onset",
        ARFF.a_real "duration",
        ARFF.a_int  "note",
        ARFF.a_int  "velocity",
        ARFF.a_nominalFromTo ExtremelyLow ExtremelyHigh "metro",
        ARFF.a_string "cluster"]
        -- Data
       [[date (addUTCTime (realToFrac 0.277037) t) , real 0.4774   , int 63 , int 85 , nominal ExtremelyLow , string "C1"],
        [date (addUTCTime (realToFrac 0.556944) t) , real 0.750504 , int 65 , int 73 , nominal Low          , string "C2"],
        [date (addUTCTime (realToFrac 0.750504) t) , real 0.997803 , int 63 , int 81 , nominal ExtremelyLow , string "C2"],
        [date (addUTCTime (realToFrac 0.997803) t) , real 1.7697   , int 65 , int 83 , nominal Medium       , string "C2"],
        [date (addUTCTime (realToFrac 1.7697  ) t) , real 1.99961  , int 63 , int 63 , nominal ExtremelyLow , string "C1"],
        [date (addUTCTime (realToFrac 2.10516 ) t) , real 2.46032  , int 70 , int 84 , nominal High         , string "C2"],
        [date (addUTCTime (realToFrac 2.52704 ) t) , real 3.8507   , int 70 , int 93 , nominal Low          , string "C2"],
        [date (addUTCTime (realToFrac 4.25177 ) t) , real 4.48958  , int 68 , int 69 , nominal ExtremelyLow , string "C1"],
        [date (addUTCTime (realToFrac 4.55694 ) t) , real 4.7505   , int 70 , int 67 , nominal Low          , string "C2"],
        [date (addUTCTime (realToFrac 4.7505  ) t) , real 4.9978   , int 68 , int 81 , nominal ExtremelyLow , string "C2"],
        [date (addUTCTime (realToFrac 4.9978  ) t) , real 5.7505   , int 70 , int 89 , nominal Medium       , string "C2"],
        [date (addUTCTime (realToFrac 5.7505  ) t) , real 6.0139   , int 68 , int 88 , nominal ExtremelyLow , string "C1"],
        [date (addUTCTime (realToFrac 6.1644  ) t) , real 6.50429  , int 75 , int 86 , nominal ExtremelyLow , string "C2"],
        [date (addUTCTime (realToFrac 6.50429 ) t) , real 7.00479  , int 73 , int 74 , nominal Low          , string "C1"],
        [date (addUTCTime (realToFrac 7.01768 ) t) , real 7.45781  , int 72 , int 65 , nominal Medium       , string "C2"],
        [date (addUTCTime (realToFrac 7.50429 ) t) , real 8.00479  , int 70 , int 68 , nominal Low          , string "C1"],
        [date (addUTCTime (realToFrac 8.55694 ) t) , real 8.96425  , int 73 , int 58 , nominal Low          , string "C2"],
        [date (addUTCTime (realToFrac 8.96425 ) t) , real 9.3604   , int 70 , int 91 , nominal Medium       , string "C1"],
        [date (addUTCTime (realToFrac 9.36378 ) t) , real 9.63658  , int 66 , int 70 , nominal ExtremelyLow , string "C2"],
        [date (addUTCTime (realToFrac 9.69892 ) t) , real 9.94381  , int 58 , int 69 , nominal ExtremelyLow , string "C1"],
        [date (addUTCTime (realToFrac 10.1052 ) t) , real 10.9101  , int 65 , int 81 , nominal High         , string "C2"],
        [date (addUTCTime (realToFrac 11.0952 ) t) , real 11.6182  , int 63 , int 62 , nominal Medium       , string "C2"],
        [date (addUTCTime (realToFrac 12.2518 ) t) , real 12.4896  , int 61 , int 71 , nominal ExtremelyLow , string "C2"],
        [date (addUTCTime (realToFrac 12.5569 ) t) , real 12.7522  , int 63 , int 65 , nominal Low          , string "C1"],
        [date (addUTCTime (realToFrac 12.8012 ) t) , real 12.9547  , int 65 , int 76 , nominal ExtremelyLow , missing    ],
        [date (addUTCTime (realToFrac 12.9955 ) t) , real 13.5069  , int 68 , int 11 , nominal Medium       , string "C1"],
        [date (addUTCTime (realToFrac 13.527  ) t) , real 13.4966  , int 68 , int 94 , nominal Low          , string "C2"],
        [date (addUTCTime (realToFrac 13.4966 ) t) , real 13.8325  , int 70 , int 75 , nominal Low          , string "C2"],
        [date (addUTCTime (realToFrac 13.8325 ) t) , real 14.0093  , int 64 , int 73 , nominal ExtremelyLow , string "C1"],
        [date (addUTCTime (realToFrac 14.1052 ) t) , real 15.8096  , int 61 , int 64 , nominal High         , string "C0"] ]

main :: IO ()
main = getCurrentTime >>= B.putStr . ARFF.encode . relation
