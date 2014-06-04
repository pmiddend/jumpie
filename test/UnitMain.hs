import Test.HUnit((@?=),Assertion)
import Test.Framework(defaultMainWithOpts)
import Test.Framework.Providers.HUnit(testCase)
import Data.Monoid(mempty)
import Jumpie.List(inductiveFilter)
import Jumpie.Render(optimizePlats,RenderCommand(..))
import Jumpie.Geometry.Point(Point2(..))
import Data.Bool(Bool(True))
import Prelude((+))
import Data.Ord((<=))
import Data.List(sum,take)
import Data.Function(($))
import System.IO(IO)

singletonTest :: Assertion
singletonTest = (inductiveFilter (\x xs -> True) ["foo"]) @?= ["foo"]

optimizePlatsTest :: Assertion
optimizePlatsTest = optimizePlats [before,RenderSprite "platforml" (Point2 0 0),RenderSprite "platformm" (Point2 1 1),RenderSprite "platformr" (Point2 2 2),after] @?= [before,RenderSprite "platform1" (Point2 0 0),after]
  where before = RenderSprite "else" (Point2 0 0)
        after = RenderSprite "other" (Point2 0 0)

optimizePlatsTest2 :: Assertion
optimizePlatsTest2 = optimizePlats [RenderSprite "platforms" (Point2 0 0)] @?= [RenderSprite "platforms" (Point2 0 0)]

sumFun x xs = x + sum xs <= 3

infTest :: Assertion
infTest = (take 1 $ inductiveFilter sumFun [1..]) @?= [1]

infTest2 :: Assertion
infTest2 = (take 2 $ inductiveFilter sumFun [1..]) @?= [1,2]

main :: IO ()
main = defaultMainWithOpts
       [testCase "singleton" singletonTest,
        testCase "infTest" infTest,
        testCase "infTest2" infTest2,
        testCase "optimizePlatsTest" optimizePlatsTest
       ]
       mempty
