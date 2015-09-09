import TestTypes (testTypes)
import TestCalc (testCalc)
import TestParse (testParse)
import TestIOActions (testIOActions)

main :: IO Bool
main = do
    b <- sequence [testTypes, testCalc, testParse, testIOActions]
    return $ and b
