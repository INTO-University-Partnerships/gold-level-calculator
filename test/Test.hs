import TestParse (testParse)
import TestCalc (testCalc)
import TestIOActions (testIOActions)

main :: IO Bool
main = do
    b <- sequence [testParse, testCalc, testIOActions]
    return $ and b
