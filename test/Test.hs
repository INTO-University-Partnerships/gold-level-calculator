import TestParse (testParse)
import TestCalc (testCalc)

main :: IO Bool
main = do
    b <- sequence [testParse, testCalc]
    return $ and b
