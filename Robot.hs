{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Robot (
  -- Con tipo data(..) se exporta todo lo que tiene, constructor incluido.
  Robot(..),
  Turret(..),
  RobotSpriteProfile(..),
  MovementAction(..),
  MemoryValue(..),
  detectedAgent,
  isRobotAlive,
  countActiveRobots,
  updateRobotVelocity,
  canShoot,
  updateTurretCooldown,
  shootProjectile,
  afterShooting,
  createBasicRobot,
  multiplyMovementAction,
  getMovementActionValue,
  rotateTurretTowards,
  setTurretAngle,
  spriteProfileFor
) where

import Entities
import Geometry
import qualified Data.Map as Map

data RobotSpriteProfile = RobotSpriteProfile
  {
    rspBodySpritePixels :: (Float, Float),
    rspTurretSpritePixels :: (Float, Float),
    rspBodyHeightMeters :: Scalar,
    rspTurretForwardOffsetRatio :: Scalar,
    rspTurretScale :: Scalar,
    rspCollisionScale :: Scalar
  } deriving (Show, Eq)

spriteProfileFor :: String -> RobotSpriteProfile
spriteProfileFor behavior = case behavior of
  "aggressive" -> mkProfile (628, 360) (666, 273) 12 1.0 0.88
  "defensive"  -> mkProfile (843, 503) (917, 326) 12 1.0 0.88
  "sniper"     -> mkProfile (551, 256) (975, 233) 10.0 0.78 0.88
  _             -> mkProfile (512, 512) (512, 200) 12 1.0 0.88
  where
    mkProfile :: (Float, Float) -> (Float, Float) -> Scalar -> Scalar -> Scalar -> RobotSpriteProfile
    mkProfile bodyPx turretPx bodyHeight turretScale collisionScale = RobotSpriteProfile
      { rspBodySpritePixels = bodyPx
      , rspTurretSpritePixels = turretPx
      , rspBodyHeightMeters = bodyHeight
      , rspTurretForwardOffsetRatio = 0.25
      , rspTurretScale = turretScale
      , rspCollisionScale = collisionScale
      }

profileBodySize :: RobotSpriteProfile -> Size
profileBodySize profile = (widthMeters, heightMeters)
  where
    (bodyPxW, bodyPxH) = rspBodySpritePixels profile
    heightMeters = rspBodyHeightMeters profile
    aspectRatio = bodyPxW / bodyPxH
    widthMeters = aspectRatio * heightMeters

centeredRectangleFromSize :: Size -> [Point]
centeredRectangleFromSize (w, h) =
  [(-halfW, -halfH), (halfW, -halfH), (halfW, halfH), (-halfW, halfH)]
  where
    halfW = w / 2
    halfH = h / 2

uniformScaleSize :: Scalar -> Size -> Size
uniformScaleSize factor (w, h) = (w * factor, h * factor)

data Robot = Rob
  { 
    robotID :: ID,
    robotPosition   :: Position, 
    robotVelocity   :: Velocity,
    robotSize  :: Size,
    robotVertices :: [Point],
    robotEnergy     :: Scalar,
    robotMaxEnergy  :: Scalar, 
    robotRadarRange :: Scalar, 
    robotOrientation :: Angle, 
    robotTurret     :: Turret,
    robotMemory :: Map.Map String MemoryValue,
    robotBehavior :: String, -- Nombre del comportamiento, no la función
    robotLastUpdateTime :: Scalar, -- Ultimo momento de actualizacion
    robotCurrentInstruction :: Maybe String -- Nombre de la instrucción actual
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
    turretOrientation :: Angle,
    turretCooldown :: Scalar,
    turretMaxCooldown :: Scalar,
    turretDamage :: Scalar,
    turretRange :: Scalar
  } deriving(Show , Eq)

-- MovementAction es un Enum
data MovementAction
  = MoveForward Scalar
  | MoveBackward Scalar
  | Rotate Angle
  | MultiplyVelocity Scalar
  deriving(Show , Eq)

-- Aplica un factor multiplicador sobre la acción.
multiplyMovementAction :: Scalar -> MovementAction -> MovementAction
multiplyMovementAction factor (MoveForward s) = MoveForward (factor * s)
multiplyMovementAction factor (MoveBackward s) = MoveBackward (factor * s)
multiplyMovementAction factor (Rotate a) = Rotate (factor * a)
multiplyMovementAction factor (MultiplyVelocity s) = MultiplyVelocity (factor * s)

-- Sacamos el valor envuelto
getMovementActionValue :: MovementAction -> Scalar
getMovementActionValue (MoveForward s)       = s
getMovementActionValue (MoveBackward s)      = s
getMovementActionValue (Rotate a)            = a
getMovementActionValue (MultiplyVelocity s)  = s

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
updateRobotVelocity r (Rotate angleDif) = setVelocity rotatedRobot rotatedVelocity
  where
    -- Tenemos que girar el robot, su torreta y sus vértices.
    rotatedTurret = (robotTurret r) { turretOrientation = turretOrientation (robotTurret r) + angleDif }
    rotatedRobot = (updateOrientation r angleDif) { robotTurret = rotatedTurret }
    rotatedVelocity = rotateVector (velocity r) angleDif
updateRobotVelocity r (MoveForward speed) = setVelocity r (add2D (velocity r) (prodByScalar speed (angleFactor (orientation r))))
-- Retroceso más suave: antes era speed * 2.0, lo reducimos para evitar aceleraciones excesivas
updateRobotVelocity r (MoveBackward speed) = setVelocity r (subVec (velocity r) (prodByScalar (speed * 1.2) (angleFactor (orientation r))))

-- ============================================================================
-- FUNCIONES PARA MANEJAR LA TORRETA
-- ============================================================================

-- Verifica si el robot puede disparar (cooldown terminado)
canShoot :: Robot -> Bool
canShoot r = turretCooldown (robotTurret r) <= 0

-- Actualiza el cooldown de la torreta
updateTurretCooldown :: Robot -> Scalar -> Robot
updateTurretCooldown r deltaTime = r { robotTurret = turret { turretCooldown = max 0 (turretCooldown turret - deltaTime) } }
  where turret = robotTurret r

-- Crea un proyectil si el robot puede disparar
shootProjectile :: Robot -> Maybe Projectile
shootProjectile r 
  | canShoot r = Just $ Proj
    { 
      projectilePosition = projectilePos,
      projectileVelocity = prodByScalar 60 muzzleDirection,
      projectileVertices = projectileVerts,
      projectileSize = (projectileLength, projectileThickness),
      projectileOrientation = turretOrientation (robotTurret r),
      projectileDamage = turretDamage (robotTurret r),
      projectileID = -1,
      projectileOwnerID = robotID r
    }
  | otherwise = Nothing
  where
    turretAngle = turretOrientation (robotTurret r)
    muzzleDirection = angleFactor turretAngle
    profile = spriteProfileFor (robotBehavior r)
    (bodyPxW, bodyPxH) = rspBodySpritePixels profile
    (turretPxW, turretPxH) = rspTurretSpritePixels profile
    (sx, sy) = size r
    turretScale = rspTurretScale profile
    turretWidthMeters = sx * (turretPxW / bodyPxW) * turretScale
    turretHeightMeters = sy * (turretPxH / bodyPxH) * turretScale
    projectileThickness = turretHeightMeters * 0.22
    projectileAspectRatio = 887 / 236
    projectileLength = projectileThickness * projectileAspectRatio
    turretForwardOffset = sx * rspTurretForwardOffsetRatio profile
    muzzleOffset = turretForwardOffset + turretWidthMeters / 2
    projectileCenterOffset = muzzleOffset + projectileLength / 2
    projectilePos = add2D (position r) (prodByScalar projectileCenterOffset muzzleDirection)
    projectileLocalVerts = centeredRectangleFromSize (projectileLength, projectileThickness)
    projectileVerts = map (add2D projectilePos) (rotateVertices projectileLocalVerts turretAngle)

-- Actualiza el robot después de disparar (reinicia cooldown)
afterShooting :: Robot -> Robot
afterShooting r = r { robotTurret = turret { turretCooldown = turretMaxCooldown turret } }
  where turret = robotTurret r

-- ============================================================================
-- FUNCIÓN PARA CREAR ROBOTS BÁSICOS
-- ============================================================================

-- Crea un robot básico con comportamiento AI
createBasicRobot :: Position -> String -> ID -> Robot
createBasicRobot pos behaviorName newID = Rob
  { robotPosition = pos
  , robotVelocity = (0, 0)
  , robotSize = baseSize
  , robotVertices = map (add2D pos) collisionVerts
  , robotEnergy = 100
  , robotMaxEnergy = 100
  , robotRadarRange = behaviorRadarRange
  , robotOrientation = 0
  , robotTurret = Turr
    { turretOrientation = 0
    , turretCooldown = 0
    , turretMaxCooldown = behaviorTurretMaxCooldown
    , turretDamage = behaviorTurretDamage
    , turretRange = behaviorTurretRange
    }
  , robotMemory = Map.empty
  , robotBehavior = behaviorName
  , robotLastUpdateTime = 0
  , robotCurrentInstruction = Nothing,
    robotID = newID
  }
  where
    profile = spriteProfileFor behaviorName
    baseSize = profileBodySize profile
    collisionSize = uniformScaleSize (rspCollisionScale profile) baseSize
    collisionVerts = centeredRectangleFromSize collisionSize
    (behaviorRadarRange, behaviorTurretRange, behaviorTurretMaxCooldown, behaviorTurretDamage) = behaviorCombatProfile behaviorName

behaviorCombatProfile :: String -> (Scalar, Scalar, Scalar, Scalar)
behaviorCombatProfile behavior = case behavior of
  "sniper"    -> (38, 42, 0.65, 24)
  "aggressive"-> (30, 28, 0.9, 22)
  "defensive" -> (32, 30, 1.05, 18)
  _            -> (28, 26, 1.0, 20)

-- ============================================================================
-- FUNCIONES PARA CONTROLAR LA TORRETA
-- ============================================================================

-- Establece el ángulo de la torreta directamente
setTurretAngle :: Robot -> Angle -> Robot
setTurretAngle r angle = r { robotTurret = turret { turretOrientation = angle } }
  where turret = robotTurret r

-- Rota la torreta hacia una posición objetivo
rotateTurretTowards :: Robot -> Position -> Robot
rotateTurretTowards r targetPos = 
  let targetAngle = angleToTarget (position r) targetPos
  in setTurretAngle r targetAngle