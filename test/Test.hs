import TestTypes
import TestCalc
import TestParse
import TestIOActions

main :: IO Bool
main = do
  b <- sequence
    [ testTypes
    , testCalc
    , testParse
    , testIOActions
    ]
  return $ and b
