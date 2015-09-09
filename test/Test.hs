import TestTypes (testTypes)
import TestCalc (testCalc)
import TestIOActions (testIOActions)

main :: IO Bool
main = do
    b <- sequence [testTypes, testCalc, testIOActions]
    return $ and b
