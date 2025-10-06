{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Robot (
  -- Con tipo data(..) se exporta todo lo que tiene, constructor incluido.
  Robot(..),
  Turret(..),
  MovementAction(..),
  MemoryValue(..),
  detectedAgent,
  isRobotAlive,
  countActiveRobots,
  updateRobotVelocity
) where

import Entities
import Geometry
import qualified Data.Map as Map

data Robot = Rob
  { 
    robotPosition   :: Position, 
    robotVelocity   :: Velocity,
    robotSize  :: Size,
    robotVertices :: [Point],
    robotEnergy     :: Scalar, 
    robotRadarRange :: Scalar, 
    robotOrientation :: Angle, 
    robotTurret     :: Turret,
    robotMemory :: Map.Map String MemoryValue
  } deriving (Show, Eq)

instance GameEntity Robot where
  position = robotPosition
  velocity = robotVelocity
  vertices = robotVertices
  size = robotSize
  orientation = robotOrientation
  setPosition r pos = r { robotPosition = pos }
  setVelocity r vel = r { robotVelocity = vel }
  setVertices r verts = r { robotVertices = verts }
  setSize r siz = r { robotSize = siz }
  setOrientation r ori = r { robotOrientation = ori }

data Turret = Turr
  {
    turretOrientation :: Angle
  } deriving(Show , Eq)

-- MovementAction es un Enum
data MovementAction
  = MoveForward Scalar
  | MoveBackward Scalar
  | Rotate Angle
  | MultiplyVelocity Scalar
  deriving(Show , Eq)

-- Tipo de datos flexible para almacenar diferentes tipos de información
data MemoryValue 
  = IntValue Int
  | StringValue String
  | PositionValue Position
  | BoolValue Bool
  | ScalarValue Scalar
  | VectorValue Vector
  | RobotListValue [Robot]
  deriving (Show, Eq)

-- Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar.
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 = distanceBetween (position r1) (position r2) <= robotRadarRange r1

-- True si la energía del robot es mayor a 0.
isRobotAlive :: Robot -> Bool
isRobotAlive r = robotEnergy r > 0

-- Contar los robots que están vivos.
countActiveRobots :: [Robot] -> Int
countActiveRobots lr = length lv
  where lv = [robot | robot <- lr, isRobotAlive robot]

-- Nota: Se han eliminado/modificado funciones redundantes de entregas anteriores al implementar GameEntity.
-- Actualizar velocidad basada en la acción de movimiento informada por el bot.
updateRobotVelocity :: Robot -> MovementAction -> Robot
updateRobotVelocity r (MultiplyVelocity speed) = setVelocity r (prodByScalar speed (velocity r))
updateRobotVelocity r (Rotate angleDif) = setVelocity rotatedRobot reducedVelocity
  where
    rotatedRobot = updateOrientation r angleDif
    reducedVelocity = prodByScalar (1 - angleDif/(2*pi)) (velocity r)
updateRobotVelocity r (MoveForward speed) = setVelocity r (add2D (velocity r) (prodByScalar speed (angleFactor (orientation r))))
updateRobotVelocity r (MoveBackward speed) = setVelocity r (subVec (velocity r) (prodByScalar speed (angleFactor (orientation r))))