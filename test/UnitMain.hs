import           Data.Bool                      (Bool (True, False))
import           Data.Function                  (($))
import           Data.Int                       (Int)
import           Data.List                      (and, map, sum, take, (++))
import           Data.Maybe                     (Maybe (..))
import           Data.Monoid                    (mempty)
import           Data.Ord                       ((<=))
import           Data.String                    (String)
import           Data.Tuple                     (uncurry)
import           Jumpie.GameGeneration          (generateGame)
import           Jumpie.GameObject              (Box (..), BoxType (..),
                                                 GameObject (ObjectBox))
import           Jumpie.Geometry.Point          (Point2 (..))
import           Jumpie.List                    (inductiveFilter,
                                                 withPredecessor, withSuccessor)
import           Jumpie.Render                  (RenderCommand (..),
                                                 optimizePlats)
import           Prelude                        ((+))
import           System.IO                      (IO)
import           System.Random                  (getStdGen)
import           Test.Framework                 (defaultMainWithOpts)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertBool, (@?=))

withSuccessorTests :: [(String,Assertion)]
withSuccessorTests = [ ("withSuccessor1",withSuccessor ([] :: [Int]) @?= [])
                     , ("withSuccessor2",withSuccessor [1] @?= [(1,Nothing)])
                     , ("withSuccessor3",withSuccessor [1,2] @?= [(1,Just 2),(2,Nothing)])
                     , ("withSuccessor4",withSuccessor [1,2,3] @?= [(1,Just 2),(2,Just 3),(3,Nothing)])
                     ]

withPredecessorTests :: [(String,Assertion)]
withPredecessorTests = [ ("withPredecessor1",withPredecessor ([] :: [Int]) @?= [])
                       , ("withPredecessor2",withPredecessor [1] @?= [(Nothing,1)])
                       , ("withPredecessor3",withPredecessor [1,2] @?= [(Nothing,1),(Just 1,2)])
                       , ("withPredecessor4",withPredecessor [1,2,3] @?= [(Nothing,1),(Just 1,2),(Just 2,3)])
                     ]

validSuccessor :: (GameObject,Maybe GameObject) -> Bool
validSuccessor (ObjectBox (Box _ BoxLeft),Just (ObjectBox (Box _ BoxRight))) = True
validSuccessor (ObjectBox (Box _ BoxLeft),Just (ObjectBox (Box _ BoxMiddle))) = True
validSuccessor (ObjectBox (Box _ BoxLeft),_) = False
validSuccessor (ObjectBox (Box _ BoxMiddle),Just (ObjectBox (Box _ BoxRight))) = True
validSuccessor (ObjectBox (Box _ BoxMiddle),Just (ObjectBox (Box _ BoxMiddle))) = True
validSuccessor (ObjectBox (Box _ BoxMiddle),_) = False
validSuccessor _ = True

validPredecessor :: (Maybe GameObject,GameObject) -> Bool
validPredecessor (Just (ObjectBox (Box _ BoxMiddle)),ObjectBox (Box _ BoxMiddle)) = True
validPredecessor (Just (ObjectBox (Box _ BoxLeft)),ObjectBox (Box _ BoxMiddle)) = True
validPredecessor (_,ObjectBox (Box _ BoxMiddle)) = False
validPredecessor (Just (ObjectBox (Box _ BoxMiddle)),ObjectBox (Box _ BoxRight)) = True
validPredecessor (Just (ObjectBox (Box _ BoxLeft)),ObjectBox (Box _ BoxRight)) = True
validPredecessor (_,ObjectBox (Box _ BoxRight)) = False

generateGameTest :: Assertion
generateGameTest = do
  g <- getStdGen
  let gameObjects = generateGame g
  assertBool "There was at least one invalid successor" (and (map validSuccessor (withSuccessor gameObjects)))
  assertBool "There was at least one invalid predecessor" (and (map validPredecessor (withPredecessor gameObjects)))

singletonTest :: Assertion
singletonTest = (inductiveFilter (\x xs -> True) ["foo"]) @?= ["foo"]

optimizePlatsTest :: Assertion
optimizePlatsTest = optimizePlats
                      [ before
                      , RenderSprite "platforml" (Point2 0 0)
                      , RenderSprite "platformm" (Point2 1 0)
                      , RenderSprite "platformr" (Point2 2 0)
                      , after
                      ]
                      @?=
                      [ before
                      , RenderSprite "platform1" (Point2 0 0)
                      , after
                      ]
  where before = RenderSprite "else" (Point2 0 0)
        after = RenderSprite "other" (Point2 0 0)

optimizePlatsTest2 :: Assertion
optimizePlatsTest2 = optimizePlats [RenderSprite "platforms" (Point2 0 0)] @?= [RenderSprite "platforms" (Point2 0 0)]

optimizePlatsTest3 :: Assertion
optimizePlatsTest3 = optimizePlats
                      [ before
                      , RenderSprite "platforml" (Point2 0 0)
                      , RenderSprite "platformm" (Point2 1 0)
                      , RenderSprite "platformm" (Point2 2 0)
                      , RenderSprite "platformr" (Point2 3 0)
                      , after
                      ]
                      @?=
                      [ before
                      , RenderSprite "platform2" (Point2 0 0)
                      , after
                      ]
  where before = RenderSprite "else" (Point2 0 0)
        after = RenderSprite "other" (Point2 0 0)

optimizePlatsTest4 :: Assertion
optimizePlatsTest4 = optimizePlats
                      [ before
                      , RenderSprite "platforml" (Point2 0 0)
                      , RenderSprite "platformr" (Point2 2 0)
                      , after
                      ]
                      @?=
                      [ before
                      , RenderSprite "platform0" (Point2 0 0)
                      , after
                      ]
  where before = RenderSprite "else" (Point2 0 0)
        after = RenderSprite "other" (Point2 0 0)

sumFun x xs = x + sum xs <= 3

infTest :: Assertion
infTest = (take 1 $ inductiveFilter sumFun [1..]) @?= [1]

infTest2 :: Assertion
infTest2 = (take 2 $ inductiveFilter sumFun [1..]) @?= [1,2]

testCases = [ ("singleton",singletonTest)
            , ("infTest",infTest)
            , ("infTest2",infTest2)
            , ("optimizePlatsTest",optimizePlatsTest)
            , ("optimizePlatsTest2",optimizePlatsTest2)
            , ("optimizePlatsTest3",optimizePlatsTest3)
            ] ++ withSuccessorTests

main :: IO ()
main = defaultMainWithOpts (map (uncurry testCase) testCases) mempty
