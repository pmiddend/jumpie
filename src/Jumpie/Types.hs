module Jumpie.Types(
  TimeDelta(TimeDelta),
  RectReal,
  timeDelta,
  IncomingAction(..),
  GameObject(..),
  isBox,
  isPlayer,
  isSensorLine,
  GameState,
  SensorLine(SensorLine),
  line,
  Box(Box),
  box,
  GameTicks(GameTicks),
  tickValue,
  GameData(GameData),
  gdSurfaces,
  gdScreen,
  LineSegmentReal,
  Player(Player),
  playerPosition,
  playerMode,
  playerVelocity,
  OutgoingAction(..),
  PlayerMode(..),
  PointReal,
  Real,
  RectInt,
  FrameState(FrameState),
  getTimeDelta,
  getKeydowns,
  Keydowns,
  ImageId,
  ImageMap,
  ImageDescFile,
  SurfaceData,
  SurfaceMap
  ) where

import Prelude(Double)
import Data.Bool(Bool(..))
import Data.Eq(Eq)
import Jumpie.Geometry.LineSegment(LineSegment)
import Data.Word(Word64)
import Data.String(String)
import Jumpie.Geometry.Rect(Rect())
import Data.Map.Strict(Map)
import Data.Int(Int)
import Jumpie.Geometry.Point(Point2(..))
import Graphics.UI.SDL.Types(Surface)
import Graphics.UI.SDL.Keysym(SDLKey)
import System.FilePath

newtype TimeDelta = TimeDelta { timeDelta :: Double }

newtype SensorLine = SensorLine { line :: LineSegment PointReal }
newtype Box = Box { box :: RectReal }

newtype GameTicks = GameTicks { tickValue :: Word64 }

type RectInt = Rect (Point2 Int)

type ImageId = String

type ImageMap = Map ImageId RectInt

type ImageDescFile = FilePath

type SurfaceData = (Surface,RectInt)

type SurfaceMap = Map ImageId SurfaceData

data GameData = GameData {
                gdSurfaces :: SurfaceMap,
                gdScreen :: Surface
              }

type Real = Double
type PointReal = Point2 Real
type RectReal = Rect PointReal
type LineSegmentReal = LineSegment PointReal

data PlayerMode = Ground | Air deriving(Eq)

data Player = Player {
  playerPosition :: PointReal,
  playerMode :: PlayerMode,
  playerVelocity :: PointReal
  }

data OutgoingAction = Collision

data IncomingAction = PlayerLeft | PlayerRight | PlayerJump deriving(Eq)

data GameObject = ObjectPlayer Player | ObjectBox Box | ObjectSensorLine SensorLine

isBox :: GameObject -> Bool
isBox (ObjectBox _) = True
isBox _ = False

isPlayer :: GameObject -> Bool
isPlayer (ObjectPlayer _) = True
isPlayer _ = False

isSensorLine :: GameObject -> Bool
isSensorLine (ObjectSensorLine _) = True
isSensorLine _ = False

type GameState = [GameObject]

type Keydowns = [SDLKey]

data FrameState = FrameState {
  getTimeDelta :: TimeDelta,
  getKeydowns :: Keydowns
  }
