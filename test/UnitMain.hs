import Test.HUnit((@?=),Assertion)
import Test.Framework(defaultMainWithOpts)
import Test.Framework.Providers.HUnit(testCase)
import Data.Monoid(mempty)
import Jumpie.List(inductiveFilter)
import Data.Bool(Bool(True))
import Prelude((+))
import Data.Ord((<=))
import Data.List(sum,take)
import Data.Function(($))
import System.IO(IO)

singletonTest :: Assertion
singletonTest = (inductiveFilter (\x xs -> True) ["foo"]) @?= ["foo"]

sumFun x xs = x + sum xs <= 3

infTest :: Assertion
infTest = (take 1 $ inductiveFilter sumFun [1..]) @?= [1]

infTest2 :: Assertion
infTest2 = (take 2 $ inductiveFilter sumFun [1..]) @?= [1,2]

main :: IO ()
main = defaultMainWithOpts
       [testCase "singleton" singletonTest,
        testCase "infTest" infTest,
        testCase "infTest2" infTest2
       ]
       mempty
