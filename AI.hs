{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module AI (
  -- Tipos del DSL
  BotBehavior,
  BotCondition(..),
  BotCommand(..),
  BotInstruction(..),
  AIExecutionResult(..),
  -- Funciones del DSL
  move, moveBackward, rotate, multiplyVelocity, shoot, wait, rotateTurret, ifThen, ifThenElse, sequence,
  -- Condiciones
  hasTarget, isLowEnergy, isUnderAttack, distanceTo, angleTo, isNearMapEdgeCondition, isNearObstacle,
  -- Decisor de comportamientos
  decideBotBehavior,
  -- Ejecutor de comandos
  executeAICommands, updateRobotAI,
  -- Comportamientos de ejemplo
  aggressiveBot, defensiveBot, exampleBot, sniperBot,
  -- Utilidades nuevas (exportadas para tests/extensión)
  safeRandomTurn, adjustTurretAngle, avoidEdgeSmart, avoidObstacleSmart, avoidObstacleSmartImmediate, interpolateAngle, resetIfStuck
) where

import Robot (Robot(..), MovementAction(..), Turret(..), MemoryValue(..), isRobotAlive, detectedAgent, updateRobotVelocity, shootProjectile, afterShooting, updateTurretCooldown, multiplyMovementAction)
import Entities (Projectile(..), GameEntity(..), Explosion(..), ID, Obstacle(..), ObstacleType(..))
import Geometry (Angle, Scalar, distanceBetween, angleToTarget)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Prelude hiding (sequence, repeat)
import GameState
import Debug.Trace
import Geometry (add2D, angleFactor, prodByScalar, deg2rad)
import Collisions (willCollideNextFrame)
-- ============================================================================
-- DSL PARA ACCIONES DEL BOT
-- ============================================================================

-- Comandos básicos que puede ejecutar un bot
data BotCommand
  = MovementCommand MovementAction  -- Comando de movimiento 
  | ShootCommand                    -- Disparar
  | WaitCommand Scalar              -- Esperar un tiempo
  | SetMemoryCommand String MemoryValue  -- Guardar valor en memoria
  | ClearMemoryCommand String       -- Limpiar memoria
  | RotateTurretCommand Angle       -- Rotar la torreta
  | DoNothingCommand
  | ClearBlockCommand
  deriving (Show, Eq)

-- Condiciones que puede evaluar un bot
data BotCondition
  = HasTarget                    -- ¿Tiene un enemigo cerca?
  | IsLowEnergy Scalar           -- ¿Energía baja?
  | IsUnderAttack                -- ¿Está siendo atacado?
  | DistanceToTarget Scalar      -- ¿Distancia al enemigo < X?
  | AngleToTarget Angle          -- ¿Ángulo al enemigo < X?
  | MemoryEquals String MemoryValue  -- ¿Memoria tiene valor X?
  | IsNearMapEdge                -- ¿Está cerca del borde del mapa?
  | IsNearObstacle               -- ¿Está cerca de un obstáculo?
  | Not BotCondition             -- Negación lógica
  | And BotCondition BotCondition    -- Y lógico
  | Or BotCondition BotCondition     -- O lógico
  | BoolCondition Bool               -- Valor booleano arbitrario
  deriving (Show, Eq)

-- Instrucciones que puede ejecutar un bot
data BotInstruction
  = Simple BotCommand        -- Comando simple
  | Conditional BotCondition BotInstruction BotInstruction  -- if-then-else
  | Sequence [BotInstruction]   -- Secuencia de instrucciones
  deriving (Show, Eq)

-- Comportamiento del bot que recibe estado del juego y devuelve instrucciones
type BotBehavior = GameState -> Robot -> BotInstruction -- El tipo de dato lo usamos para identificar el tipo de comportamiento del bot.
-- Behavior es una función que recibe el estado del juego y el robot y devuelve una instrucción.

-- ============================================================================
-- FUNCIONES DEL DSL
-- ============================================================================

-- Comandos básicos
move :: Scalar -> BotInstruction
move speed = Simple (MovementCommand (MoveForward speed))

moveBackward :: Scalar -> BotInstruction
moveBackward speed = Simple (MovementCommand (MoveBackward speed))

rotate :: Angle -> BotInstruction
rotate angle = Simple (MovementCommand (Rotate angle))

multiplyVelocity :: Scalar -> BotInstruction
multiplyVelocity factor = Simple (MovementCommand (MultiplyVelocity factor))

shoot :: BotInstruction
shoot = Simple ShootCommand

wait :: Scalar -> BotInstruction
wait time = Simple (WaitCommand time)

rotateTurret :: Angle -> BotInstruction
rotateTurret angle = Simple (RotateTurretCommand angle)

setMemory :: String -> MemoryValue -> BotInstruction
setMemory key value = Simple (SetMemoryCommand key value)

clearMemory :: String -> BotInstruction
clearMemory key = Simple (ClearMemoryCommand key)

-- Combinadores de instrucciones
ifThen :: BotCondition -> BotInstruction -> BotInstruction
ifThen cond instruction = Conditional cond instruction (Simple DoNothingCommand)

ifThenElse :: BotCondition -> BotInstruction -> BotInstruction -> BotInstruction
ifThenElse = Conditional

sequence :: [BotInstruction] -> BotInstruction
sequence = Sequence

-- ============================================================================
-- CONDICIONES DEL DSL
-- ============================================================================

hasTarget :: BotCondition
hasTarget = HasTarget

isLowEnergy :: Scalar -> BotCondition
isLowEnergy = IsLowEnergy

isUnderAttack :: BotCondition
isUnderAttack = IsUnderAttack

distanceTo :: Scalar -> BotCondition
distanceTo = DistanceToTarget

angleTo :: Angle -> BotCondition
angleTo = AngleToTarget

memoryEquals :: String -> MemoryValue -> BotCondition -- ¿Tengo guardado en memoria un valor específico?
memoryEquals = MemoryEquals

isNearMapEdgeCondition :: BotCondition
isNearMapEdgeCondition = IsNearMapEdge

-- Nuevo: cerca de obstáculo
isNearObstacle :: BotCondition
isNearObstacle = IsNearObstacle

-- ============================================================================
-- EVALUADOR DE CONDICIONES
-- ============================================================================

-- Evalúa una condición y devuelve True o False
evalCondition :: BotCondition -> GameState -> Robot -> Bool

-- CONDICIÓN: ¿Tiene el robot un objetivo en rango de su radar?
evalCondition HasTarget gs robot =
  any (\r -> r /= robot && detectedAgent robot r) (Map.elems (gameRobots gs))

-- CONDICIÓN: ¿La energía del robot está por debajo del umbral?
evalCondition (IsLowEnergy threshold) _ robot =
  robotEnergy robot < threshold -- True si la energía del robot es menor al umbral.

-- CONDICIÓN: ¿Está el robot siendo atacado?
evalCondition IsUnderAttack gs robot =
  any (\p -> distanceBetween (position robot) (position p) < 5) (Map.elems (gameProjectiles gs))

-- CONDICIÓN: ¿Está el enemigo más cercano a una distancia menor que maxDist?
evalCondition (DistanceToTarget maxDist) gs robot =
  case findNearestEnemy robot (gameRobots gs) of
    Nothing -> False
    Just enemy -> distanceBetween (position robot) (position enemy) < maxDist

-- CONDICIÓN: ¿Está el enemigo más cercano dentro del ángulo especificado?
evalCondition (AngleToTarget maxAngle) gs robot =
  case findNearestEnemy robot (gameRobots gs) of
    Nothing -> False
    Just enemy -> abs (angleToTarget (position robot) (position enemy) - orientation robot) < maxAngle

-- CONDICIÓN: ¿La memoria del robot contiene un valor específico?
evalCondition (MemoryEquals key value) _ robot = -- _ es el estado del juego y el robot.
  case Map.lookup key (robotMemory robot) of -- Lookup es una función que busca un valor en un mapa.
    Nothing -> False
    Just memValue -> value == memValue

-- CONDICIÓN: ¿Está el robot cerca del borde del mapa?
evalCondition IsNearMapEdge gs robot = isNearMapEdge gs robot

-- CONDICIÓN: ¿Está cerca de algún obstáculo (colisión o proximidad leve)?
evalCondition IsNearObstacle gs robot = any nearObs (Map.elems (gameObstacles gs))
  where
    (rx, ry) = position robot
    (rw, _) = size robot
    nearObs o =
      let (ox, oy) = obstaclePosition o
          (ow, _) = obstacleSize o
          threshold = max rw ow + 3.0
      in distanceBetween (rx, ry) (ox, oy) <= threshold

-- OPERADORES LÓGICOS
evalCondition (Not cond) gs robot = not (evalCondition cond gs robot)
evalCondition (And cond1 cond2) gs robot =
  evalCondition cond1 gs robot && evalCondition cond2 gs robot
evalCondition (Or cond1 cond2) gs robot =
  evalCondition cond1 gs robot || evalCondition cond2 gs robot

evalCondition (BoolCondition b) _ _ = b

-- Encuentra el enemigo más cercano
findNearestEnemy :: Robot -> Map.Map ID Robot -> Maybe Robot
findNearestEnemy robot enemies =
  let aliveEnemies = filter (\r -> r /= robot && isRobotAlive r) (Map.elems enemies)
  in if null aliveEnemies
     then Nothing
     else Just (minimumBy (\a b -> compare (distanceBetween (position robot) (position a))
                                           (distanceBetween (position robot) (position b))) aliveEnemies)

-- Verifica si el robot está cerca del borde del mapa
-- Detección robusta de borde usando sensores delantero y trasero.
-- Considera el tamaño del robot y un margen configurable.
isNearMapEdge :: GameState -> Robot -> Bool
isNearMapEdge gs robot =
  let (nearF, nearB) = edgeSensors gs robot
  in nearF || nearB

-- Devuelve (frontNear, backNear) en base a sensores colocados en el centro del borde delantero y trasero.
edgeSensors :: GameState -> Robot -> (Bool, Bool)
edgeSensors gs robot =
  let (mapW, mapH) = gameStageSize gs
      (rw, rh) = size robot
      margin = max 2 (max rw rh * 0.75) -- margen relativo al tamaño del robot
      (x, y) = position robot
      ori = orientation robot
      halfLen = 0.5 * max rw rh
      frontPt = add2D (x, y) (prodByScalar halfLen (angleFactor ori))
      backPt  = add2D (x, y) (prodByScalar (-halfLen) (angleFactor ori))
      near (px, py) = px < (-mapW/2 + margin) || px > (mapW/2 - margin) ||
                      py < (-mapH/2 + margin) || py > (mapH/2 - margin)
  in (near frontPt, near backPt)

-- ============================================================================
-- DECISOR DE COMPORTAMIENTOS
-- ============================================================================

-- Decide qué instrucciones debe ejecutar un bot basado en su comportamiento y el estado del juego
decideBotBehavior :: BotBehavior -> GameState -> Robot -> [BotCommand]
decideBotBehavior botBehavior gs robot =
  let instruction = botBehavior gs robot -- Compor
  in decideInstruction instruction gs robot

-- Decide qué comandos ejecutar basado en una instrucción
decideInstruction :: BotInstruction -> GameState -> Robot -> [BotCommand]
decideInstruction (Simple cmd) _ _ = [cmd]
decideInstruction (Conditional cond thenInstruction elseInstruction) gs robot =
  if evalCondition cond gs robot -- si se cumple
  then decideInstruction thenInstruction gs robot
  else decideInstruction elseInstruction gs robot
decideInstruction (Sequence instructions) gs robot =  -- Se vuelve a llamar porque es una secuencia de instrucciones.
  concatMap (\instruction -> decideInstruction instruction gs robot) instructions

-- ============================================================================
-- COMPORTAMIENTOS DE EJEMPLO
-- ============================================================================

-- Bot agresivo que busca enemigos y los ataca de frente
aggressiveBot :: BotBehavior
aggressiveBot gs robot =
  -- 1) Evitar obstáculo; 2) Evitar borde
  ifThenElse isNearObstacle
    (avoidObstacleSmart gs robot)
    (ifThenElse isNearMapEdgeCondition
      (avoidEdgeSmart gs robot)
    (
      let enemy = fromMaybe robot (findNearestEnemy robot (gameRobots gs))
          hasEnemy = enemy /= robot
          targetPos = position enemy
          distToEnemy = distanceBetween (position robot) targetPos
          -- Rotación del cuerpo para encarar al enemigo (rápida)
          bodyTarget = angleToTarget (position robot) targetPos
          (bodyStep, _) = interpolateAngle (orientation robot) bodyTarget 2.2
          -- Ajuste suave de la torreta para disparo
          (turretStep, turretAligned) = adjustTurretAngle "aggressive" gs robot
          needAdvance = distToEnemy > 15
          needRetreat = distToEnemy < 10
          lowEnergy = robotEnergy robot < 25
          retreatAngle = normalizeAngle (bodyTarget + pi)
          (escapeStep, _) = interpolateAngle (orientation robot) retreatAngle 2.2
      in if hasEnemy
         then sequence [
           setMemory "mode" (StringValue (if lowEnergy then "retreating" else "attacking")),
           -- Aiming y disparo
           rotateTurret turretStep,
           ifThen (BoolCondition turretAligned) shoot,
           -- Gestión de distancia
           ifThenElse (BoolCondition lowEnergy)
             (sequence [ rotate escapeStep, move 0.8 ])
             (ifThenElse (BoolCondition needAdvance)
                (sequence [ rotate bodyStep, move 0.9 ])
                (ifThenElse (BoolCondition needRetreat)
                   (sequence [ rotate bodyStep, moveBackward 0.7 ])
                   (sequence [ rotate bodyStep ])
                )
             ),
           wait 0.05
         ]
         -- 2) Sin enemigo: patrulla y recentra torreta lentamente
      else sequence [
           setMemory "mode" (StringValue "patrolling"),
           rotateTurret (fst (adjustTurretAngle "aggressive" gs robot)),
           move 0.4,
           rotate (pi/24),
           wait 0.15
         ]
    ))

-- Bot francotirador que mantiene distancia y dispara con precisión
sniperBot :: BotBehavior
sniperBot gs robot =
  ifThenElse isNearObstacle
    (avoidObstacleSmart gs robot)
    (ifThenElse isNearMapEdgeCondition
      (avoidEdgeSmart gs robot)
    (
      let enemy = fromMaybe robot (findNearestEnemy robot (gameRobots gs))
          hasEnemy = enemy /= robot
          targetPos = position enemy
          distToEnemy = distanceBetween (position robot) targetPos
          (turretStep, turretAligned) = adjustTurretAngle "sniper" gs robot
          optimal = 32
      in if hasEnemy
         then sequence [
           setMemory "mode" (StringValue "sniping"),
           rotateTurret turretStep, -- solo torreta
           ifThenElse (BoolCondition (distToEnemy > optimal + 3))
             (move 0.25)
             (ifThenElse (BoolCondition (distToEnemy < optimal - 3))
                (moveBackward 0.35)
                (Simple DoNothingCommand)
             ),
           ifThen (BoolCondition turretAligned) shoot,
           wait 0.15
         ]
      else sequence [
           setMemory "mode" (StringValue "searching"),
           rotateTurret (fst (adjustTurretAngle "sniper" gs robot)),
           wait 0.25
         ]
    ))

-- Bot defensivo que se protege y calcula sus movimientos
defensiveBot :: BotBehavior
defensiveBot gs robot =
  ifThenElse isNearObstacle
    (avoidObstacleSmart gs robot)
    (ifThenElse isNearMapEdgeCondition
      (avoidEdgeSmart gs robot)
    (
      let enemy = fromMaybe robot (findNearestEnemy robot (gameRobots gs))
          hasEnemy = enemy /= robot
          targetPos = position enemy
          distToEnemy = distanceBetween (position robot) targetPos
          -- Estrategia: strafe alrededor manteniendo distancia segura
          baseAngle = angleToTarget (position robot) targetPos
          -- Dirección de strafe determinista por robot
          strafeSign = if (robotID robot `mod` 2 == 0) then 1 else (-1) :: Int
          desiredBody = normalizeAngle (baseAngle + fromIntegral strafeSign * (pi/2))
          (bodyStep, _) = interpolateAngle (orientation robot) desiredBody 1.8
          (faceStep, _) = interpolateAngle (orientation robot) baseAngle 1.8
          (turretStep, turretAligned) = adjustTurretAngle "defensive" gs robot
          safeMin = 12
          safeMax = 20
      in if hasEnemy
         then sequence [
           setMemory "mode" (StringValue "defending"),
           rotateTurret turretStep,
           ifThen (BoolCondition turretAligned) shoot,
           ifThenElse (BoolCondition (distToEnemy < safeMin))
             (sequence [ rotate faceStep, moveBackward 0.6 ])
             (ifThenElse (BoolCondition (distToEnemy > safeMax))
                (sequence [ rotate faceStep, move 0.5 ])
                (sequence [ rotate bodyStep, move 0.5 ])
             ),
           wait 0.08
         ]
      else sequence [
           setMemory "mode" (StringValue "patrolling"),
           rotate (pi/18),
           move 0.25,
           rotateTurret (fst (adjustTurretAngle "defensive" gs robot)),
           wait 0.2
         ]
    ))

-- (definida más abajo con una versión robusta)

turretBot :: BotBehavior
turretBot gs robot = sequence [
        rotateTurret (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - turretOrientation (robotTurret robot)),
        wait 1,
        shoot,
        wait 1
        ]

stupidBot :: BotBehavior
stupidBot _ _ =
  ifThenElse isNearMapEdgeCondition
    (sequence [
      -- Si está cerca del borde, girar 90 grados para cambiar dirección
      rotate (pi/2),
      wait 0.5
    ])
    (sequence [
      -- Si no está cerca del borde, comportamiento normal
      wait 1,
      move 1,
      rotate (pi/16)
    ])

-- Bot de ejemplo simple
exampleBot :: BotBehavior
exampleBot = aggressiveBot

-- ============================================================================
-- EJECUTOR DE COMANDOS AI (Simplificado, robusto)
-- ============================================================================

-- Resultado de ejecutar comandos AI
data AIExecutionResult = AIExecutionResult
  { updatedRobot :: Robot
  , newProjectiles :: [Projectile]
  , newExplosions :: [Explosion]
  } deriving (Show, Eq)

-- Limpia cualquier rastro de bloqueo heredado (v2) y el nuevo "waitRemaining"
clearAllBlocks :: Robot -> Robot
clearAllBlocks r = r { robotMemory = cleaned }
  where
    m0 = robotMemory r
    cleaned = Map.delete "waitRemaining"
            $ Map.delete "blockPoint"
            $ Map.delete "sequenceLength"
            $ Map.delete "blockPointAcumulator"
            $ Map.delete "blockPointAcumulatorEnd"
            $ Map.delete "blockPointTimer" m0

-- Ejecuta una lista de comandos AI sobre un robot.
-- Movimiento y rotación de torreta son incrementales por frame (no bloquean).
-- Solo "wait" bloquea los siguientes comandos durante su duración.
executeAICommands :: [BotCommand] -> Robot -> Scalar -> AIExecutionResult
executeAICommands commands robot deltaTime =
  let -- Si hay una espera activa, la actualizamos y no procesamos comandos.
      waitRemaining = case Map.lookup "waitRemaining" (robotMemory robot) of
                        Just (ScalarValue t) -> t
                        _ -> 0
      (robotAfterWait, shouldSkip) =
        if waitRemaining > 0
          then let t' = max 0 (waitRemaining - deltaTime)
                   r' = robot { robotMemory = Map.insert "waitRemaining" (ScalarValue t') (robotMemory robot) }
               in (r', True)
          else (clearLegacyIfAny robot, False)
  in if shouldSkip
      then AIExecutionResult robotAfterWait [] []
      else let (r', projs', expls') = processList robotAfterWait [] [] commands
           in AIExecutionResult r' projs' expls'
  where
    -- Limpiar bloqueos antiguos si existen
    clearLegacyIfAny r
      | Map.member "blockPoint" (robotMemory r) = clearAllBlocks r
      | otherwise = r

    processList :: Robot -> [Projectile] -> [Explosion] -> [BotCommand] -> (Robot, [Projectile], [Explosion])
    processList r ps es [] = (r, ps, es)
    processList r ps es (c:cs) = case c of
      MovementCommand action ->
        let r' = updateRobotVelocity r (multiplyMovementAction deltaTime action)
        in processList r' ps es cs
      RotateTurretCommand speed ->
        let t = robotTurret r
            applied = speed * deltaTime
            r' = r { robotTurret = t { turretOrientation = turretOrientation t + applied } }
        in processList r' ps es cs
      ShootCommand ->
        case shootProjectile r of
          Just p -> processList (afterShooting r) (p:ps) es cs
          Nothing -> processList r ps es cs
      WaitCommand t ->
        let r' = r { robotMemory = Map.insert "waitRemaining" (ScalarValue t) (robotMemory r) }
        in (r', ps, es) -- Bloquea el resto de comandos este frame
      SetMemoryCommand k v -> processList (r { robotMemory = Map.insert k v (robotMemory r) }) ps es cs
      ClearMemoryCommand k -> processList (r { robotMemory = Map.delete k (robotMemory r) }) ps es cs
      DoNothingCommand -> processList r ps es cs
      ClearBlockCommand -> processList (clearAllBlocks r) ps es cs

-- Actualiza un robot con su comportamiento AI
updateRobotAI :: Robot -> GameState -> Scalar -> AIExecutionResult
updateRobotAI robot gs deltaTime =
  let -- Actualizar cooldown de la torreta
      -- Usamos trace para debug: Muestra el primer argumento y devuelve el segundo. Por ejecución perezosa hay que utilizar el valor res.
      condTrace :: String -> a -> a
      condTrace msg res
        | (gameDebugInfo gs) && mod (gameFrame gs) 60 == 0 = trace msg res
        | otherwise = res

      robotWithUpdatedCooldown = updateTurretCooldown robot deltaTime

      -- Obtener comportamiento por nombre
      behavior = getBehaviorByName (robotBehavior robot)

      -- Decidir comportamiento
      commands = decideBotBehavior behavior gs robotWithUpdatedCooldown

      -- Ejecutar comandos
      result = executeAICommands (condTrace (show (robotID robot) ++ ": " ++ show commands ++ "\n" ++ show (robotMemory robot)) commands) robotWithUpdatedCooldown deltaTime

      -- Actualizar tiempo de última actualización y eliminar waitingTime si procede.
      finalRobot = (updatedRobot result) { robotLastUpdateTime = gameTime gs }

      --finalRobot
      --  | Map.member "waitingTime" (robotMemory preFinalRobot) = condTrace (show (robotID waitFinalRobot) ++ ": " ++ show (robotMemory waitFinalRobot)) waitFinalRobot
      --  | otherwise = preFinalRobot
      --  where
      --    waitFinalRobot
      --      | memLEQ currentWaitingTime (ScalarValue 0) = preFinalRobot { robotMemory = Map.delete "waitingTime" (robotMemory preFinalRobot)}
      --      | otherwise = preFinalRobot
      --      where
      --        memLEQ :: MemoryValue -> MemoryValue -> Bool
      --        memLEQ (ScalarValue x) (ScalarValue y) = x <= y
      --        currentWaitingTime = (robotMemory preFinalRobot) Map.! "waitingTime"
  in result { updatedRobot = finalRobot }

-- Obtiene un comportamiento por su nombre
getBehaviorByName :: String -> BotBehavior
getBehaviorByName "aggressive" = aggressiveBot
getBehaviorByName "defensive" = defensiveBot
getBehaviorByName "stupid" = stupidBot
getBehaviorByName "turret" = turretBot
getBehaviorByName "sniper" = sniperBot
getBehaviorByName _ = aggressiveBot -- Comportamiento por defecto

-- ============================================================================
-- Utilidades de orientación/ángulos y ayudas pedidas
-- ============================================================================

-- Normaliza un ángulo al rango [-pi, pi]
normalizeAngle :: Angle -> Angle
normalizeAngle a = atan2 (sin a) (cos a)

-- Interpola de forma segura entre current y target con un paso máximo por segundo (maxStep)
-- Devuelve (delta, aligned) donde delta es el paso recomendado (con signo) a aplicar como velocidad angular.
interpolateAngle :: Angle -> Angle -> Angle -> (Angle, Bool)
interpolateAngle current target maxStep =
  let diff = normalizeAngle (target - current)
      ad = abs diff
      aligned = ad <= maxStep * 0.5 -- tolerancia dependiente del paso disponible
      step = signum diff * min ad maxStep
  in (step, aligned)

-- Determinista: genera un giro aleatorio en [minDeg,maxDeg] con signo aleatorio.
safeRandomTurn :: GameState -> Robot -> (Float, Float) -> Angle
safeRandomTurn gs r (minDeg, maxDeg) =
  let t = gameTime gs
      key = fromIntegral (robotID r) :: Float
      base = sin (t * 3.0 + key * 12.9898) * 43758.5453
      frac = base - fromIntegral (floor base :: Int)
      ampDeg = minDeg + frac * (maxDeg - minDeg)
      sgn = if sin (t * 1.37 + key * 78.233) > 0 then 1 else -1 :: Float
  in sgn * deg2rad ampDeg

-- Ajusta la torreta hacia un objetivo (o hacia el frente si no hay target) con velocidad/tolerancia por tipo.
-- Devuelve (velocidadAngular, estaAlineada)
adjustTurretAngle :: String -> GameState -> Robot -> (Angle, Bool)
adjustTurretAngle behaviorName gs robot =
  let (speed, tol) = case behaviorName of
                        "aggressive" -> (3.0, 0.12)
                        "defensive"  -> (2.2, 0.08)
                        "sniper"     -> (1.6, 0.05)
                        _             -> (2.5, 0.1)
      mEnemy = findNearestEnemy robot (gameRobots gs)
      desired = case mEnemy of
                  Just e -> angleToTarget (position robot) (position e)
                  Nothing -> orientation robot
      current = turretOrientation (robotTurret robot)
      (step, _) = interpolateAngle current desired speed
      aligned = abs (normalizeAngle (desired - current)) <= tol
  in (step, aligned)

-- Evitación inteligente de bordes. Secuencia corta de backoff + giro aleatorio + avance opcional.
avoidEdgeSmart :: GameState -> Robot -> BotInstruction
avoidEdgeSmart gs r =
  let (frontNear, backNear) = edgeSensors gs r
      turn = safeRandomTurn gs r (30, 60)
  in if frontNear && backNear then
       sequence [ moveBackward 0.7, rotate turn, wait 0.15, move 0.6 ]
     else if frontNear then
       sequence [ moveBackward 0.5, rotate turn, wait 0.10 ]
     else if backNear then
       sequence [ move 0.5, rotate turn, wait 0.10 ]
     else Simple DoNothingCommand

-- Evitación de obstáculos: giro corto + pequeña retirada
avoidObstacleSmart :: GameState -> Robot -> BotInstruction
avoidObstacleSmart gs r =
  let turn = safeRandomTurn gs r (75, 105)
  in sequence [ moveBackward 0.5, rotate turn, wait 0.1 ]

-- Versión inmediata usada por el bucle del juego antes de aplicar la IA:
-- si una colisión con obstáculo es inminente, frena, retrocede rápido y gira inteligentemente.
avoidObstacleSmartImmediate :: GameState -> Robot -> Robot
avoidObstacleSmartImmediate gs r =
  let obstacles = filter (\o -> obstacleType o == Solid) (Map.elems (gameObstacles gs))
      imminent = filter (\o -> willCollideNextFrame r o 0.3) obstacles
  in if null imminent
       then r
       else
         let (ox, oy) = obstaclePosition (head imminent)
             (rx, ry) = position r
             _dx = rx - ox; dy = ry - oy
             -- alternar signo cuando hay múltiples obstáculos cercanos para desbloquear
             baseTurn = if dy > 0 then (pi/2) else (-pi/2)
             altSign = if length imminent > 1 && (gameFrame gs + robotID r) `mod` 2 == 0 then (-1) else 1 :: Int
             turn = fromIntegral altSign * baseTurn
             stopped = setVelocity r (0,0)
             backed  = updateRobotVelocity stopped (MoveBackward 0.8)
             rotated = updateRobotVelocity backed (Rotate turn)
         in rotated

-- Resetea bloqueos si detecta estados antiguos o esperas absurdas (para compatibilidad)
resetIfStuck :: Robot -> Scalar -> BotInstruction
resetIfStuck r _
  | Map.member "blockPoint" (robotMemory r) = Simple ClearBlockCommand
  | otherwise = Simple DoNothingCommand

-- Evita avisos de top-level no usados para algunos constructores DSL exportados
_aiKeep :: ()
_aiKeep = clearMemory `seq` memoryEquals `seq` ()
