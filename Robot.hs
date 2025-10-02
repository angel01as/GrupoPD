{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Robot (
    Robot,
    MovementAction,
    detectedAgent,
    isRobotAlive,
    countActiveRobots,
    updateRobotVelocity,
    updateVelocity,
    updatePosition
) where
import Entities
import Geometry
import qualified Data.Map as Map

data Robot = Rob
  { robotSize  :: Size, 
  position   :: Position, 
  velocity   :: Vector,
  energy     :: Scalar, 
  radarRange :: Scalar, 
  orientation:: Angle, 
  turret     :: RobotTurret,
  robotMemory :: Map.Map String MemoryValue
  } deriving (Show, Eq)

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
detectedAgent r1 r2 = distanceBetween (position r1) (position r2) <= radarRange r1

-- True si la energía del robot es mayor a 0.
isRobotAlive :: Robot -> Bool
isRobotAlive r = energy r > 0

-- Contar los robots que están vivos.
countActiveRobots :: [Robot] -> Int
countActiveRobots lr = length lv
  where lv = [robot | robot <- lr, isRobotAlive robot]

-- Actualiza la velocidad de un robot con una velocidad dada.
updateRobotVelocity :: Robot -> Vector -> Robot
updateRobotVelocity r v = r { velocity = v}

-- Actualizar velocidad basada en la acción de movimiento informada por el bot.
updateVelocity :: Robot -> MovementAction -> Robot
updateVelocity r (MultiplyVelocity speed) = updateRobotVelocity r (prodByScalar speed (velocity r))
updateVelocity r (Rotate angleDif) = r { velocity = (prodByScalar (1 - angleDif/(2*pi)) (velocity r)), orientation = ((orientation r) + angleDif)}
updateVelocity r (MoveForward speed) = updateRobotVelocity r (add2D (velocity r) (prodByScalar speed (angleFactor (orientation r))))
updateVelocity r (MoveBackward speed) = updateRobotVelocity r (subVec (velocity r) (prodByScalar speed (angleFactor (orientation r))))

-- Actualizar una posición en función de la velocidad y el incremento de tiempo.
updatePosition :: Robot -> Scalar -> Robot
updatePosition r t = r { position = newPos }
  where newPos = add2D (position r) (prodByScalar t (velocity r))