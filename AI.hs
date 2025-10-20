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
  hasTarget, isLowEnergy, isUnderAttack, distanceTo, angleTo, isNearMapEdgeCondition,
  -- Decisor de comportamientos
  decideBotBehavior,
  -- Ejecutor de comandos
  executeAICommands, updateRobotAI,
  -- Comportamientos de ejemplo
  aggressiveBot, defensiveBot, exampleBot
) where

import Robot (Robot(..), MovementAction(..), Turret(..), MemoryValue(..), isRobotAlive, detectedAgent, updateRobotVelocity, canShoot, shootProjectile, afterShooting, updateTurretCooldown, multiplyMovementAction)
import Entities (Projectile(..), GameEntity(..), Explosion(..), createExplosion, ID)
import Geometry (Position, Angle, Scalar, distanceBetween, angleToTarget, Size)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Prelude hiding (sequence, repeat)
import GameState
import Debug.Trace
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
  | Not BotCondition             -- Negación lógica
  | And BotCondition BotCondition    -- Y lógico
  | Or BotCondition BotCondition     -- O lógico
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
ifThen cond instruction = Conditional cond instruction (Simple (WaitCommand 0))

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

-- ============================================================================
-- EVALUADOR DE CONDICIONES
-- ============================================================================

-- Evalúa una condición y devuelve True o False
evalCondition :: BotCondition -> GameState -> Robot -> Bool

-- CONDICIÓN: ¿Tiene el robot un objetivo en rango de su radar?
evalCondition HasTarget gs robot =
  any (\r -> r /= robot && detectedAgent robot r) (gameRobots gs)

-- CONDICIÓN: ¿La energía del robot está por debajo del umbral?
evalCondition (IsLowEnergy threshold) _ robot =
  robotEnergy robot < threshold -- True si la energía del robot es menor al umbral.

-- CONDICIÓN: ¿Está el robot siendo atacado?
evalCondition IsUnderAttack gs robot =
  any (\p -> distanceBetween (position robot) (position p) < 5) (gameProjectiles gs)

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

-- OPERADORES LÓGICOS
evalCondition (Not cond) gs robot = not (evalCondition cond gs robot)
evalCondition (And cond1 cond2) gs robot =
  evalCondition cond1 gs robot && evalCondition cond2 gs robot
evalCondition (Or cond1 cond2) gs robot =
  evalCondition cond1 gs robot || evalCondition cond2 gs robot

-- Encuentra el enemigo más cercano
findNearestEnemy :: Robot -> Map.Map ID Robot -> Maybe Robot
findNearestEnemy robot enemies =
  let aliveEnemies = filter (\r -> r /= robot && isRobotAlive r) (Map.elems enemies)
  in if null aliveEnemies
     then Nothing
     else Just (minimumBy (\a b -> compare (distanceBetween (position robot) (position a))
                                           (distanceBetween (position robot) (position b))) aliveEnemies)

-- Verifica si el robot está cerca del borde del mapa
isNearMapEdge :: GameState -> Robot -> Bool
isNearMapEdge gs robot =
  let (x, y) = position robot
      (mapWidth, mapHeight) = gameStageSize gs
      margin = 10  -- Margen de seguridad desde el borde
  in x < (-mapWidth/2 + margin) || x > (mapWidth/2 - margin) ||
     y < (-mapHeight/2 + margin) || y > (mapHeight/2 - margin)

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

-- Bot agresivo que busca enemigos y los ataca
aggressiveBot :: BotBehavior
aggressiveBot gs robot =
  ifThenElse hasTarget
    (sequence [
      -- Si tiene objetivo, apuntar y disparar
      setMemory "mode" (StringValue "attacking"),
      rotateTurret (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - turretOrientation (robotTurret robot)),
      -- Evitar bordes del mapa
      ifThen isNearMapEdgeCondition (rotate (pi/2)),
      ifThen (And hasTarget (Not (isLowEnergy 10))) shoot,  -- Solo disparar si el objetivo sigue vivo y tenemos energía
      move 1.0
    ])
    (sequence [
      -- Si no tiene objetivo, buscar
      setMemory "mode" (StringValue "searching"),
      rotate (pi/4),  -- Girar 45 grados
      -- Evitar bordes del mapa
      ifThen isNearMapEdgeCondition (rotate (pi/2)),
      move 0.5,
      wait 0.1
    ])

-- Bot defensivo que evita enemigos cuando tiene poca energía
defensiveBot :: BotBehavior
defensiveBot gs robot =
  ifThenElse (isLowEnergy 30)
    (sequence [
      -- Si tiene poca energía, huir pero sin salirse del mapa
      setMemory "mode" (StringValue "fleeing"),
      ifThen isNearMapEdgeCondition (rotate (pi/2)),  -- Si está cerca del borde, girar
      moveBackward 1.0,  -- Moverse hacia atrás más despacio
      wait 0.5
    ])
    (ifThenElse hasTarget
      (sequence [
        -- Si tiene objetivo y energía suficiente, atacar
        setMemory "mode" (StringValue "attacking"),
        rotateTurret (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - turretOrientation (robotTurret robot)),
        ifThen (And hasTarget (Not (isLowEnergy 10))) shoot  -- Solo disparar si el objetivo sigue vivo
      ])
      (sequence [
        -- Patrullar
        setMemory "mode" (StringValue "patrolling"),
        ifThen isNearMapEdgeCondition (rotate (pi/2)),  -- Si está cerca del borde, girar
        move 0.5,  -- Moverse más despacio
        rotate (pi/8),
        wait 0.2
      ])
    )

turretBot :: BotBehavior
turretBot gs robot = sequence [
        rotateTurret (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - turretOrientation (robotTurret robot)),
        wait 1,
        shoot,
        wait 1
        ]

stupidBot :: BotBehavior
stupidBot gs robot = 
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
-- EJECUTOR DE COMANDOS AI
-- ============================================================================

-- Resultado de ejecutar comandos AI
data AIExecutionResult = AIExecutionResult
  { updatedRobot :: Robot
  , newProjectiles :: [Projectile]
  , newExplosions :: [Explosion]
  } deriving (Show, Eq)

-- Ejecuta una lista de comandos AI sobre un robot, respetando el tiempo de espera.
executeAICommands :: [BotCommand] -> Robot -> Scalar -> AIExecutionResult
-- executeAICommands commands robot deltaTime =
--   let (updatedRobot', spawnedProjectiles', spawnedExplosions') = foldl (executeCommand deltaTime) (robot, [], []) commands
--   in AIExecutionResult updatedRobot' spawnedProjectiles' spawnedExplosions'
executeAICommands commands robot' deltaTime = let
  (updatedRobot', spawnedProjectiles', spawnedExplosions') = processCommands commands (robot', [], [])

  processCommands :: [BotCommand] -> (Robot, [Projectile], [Explosion]) -> (Robot, [Projectile], [Explosion])
  processCommands [] (robot, projectiles, explosions) = (robot, projectiles, explosions)
  processCommands (cmd:cmds) (robot, projectiles, explosions) = let waitingTime = Map.findWithDefault (ScalarValue 0) "waitingTime" (robotMemory robot)
    in if memGT waitingTime (ScalarValue deltaTime)
      -- Si el tiempo de espera restante es mayor que el transcurrido desde el último frame no ejecutamos nada y actualizamos el tiempo de espera restante.
      -- Map.adjust usa el valor asociado a la clave en una función que devuelve el nuevo valor.
    then (robot { robotMemory = Map.adjust (\(ScalarValue x) -> memMax (ScalarValue (x - deltaTime)) (ScalarValue 0)) "waitingTime" (robotMemory robot)}, projectiles, explosions)
    else processCommands cmds (executeCommand deltaTime (robot, projectiles, explosions) cmd)

  memGT :: MemoryValue -> MemoryValue -> Bool
  memGT (ScalarValue a) (ScalarValue b) = a > b

  memMax :: MemoryValue -> MemoryValue -> MemoryValue
  memMax (ScalarValue a) (ScalarValue b) = ScalarValue (max a b)

  in AIExecutionResult updatedRobot' spawnedProjectiles' spawnedExplosions'

-- Ejecuta un comando individual
executeCommand :: Scalar -> (Robot, [Projectile], [Explosion]) -> BotCommand -> (Robot, [Projectile], [Explosion])
executeCommand deltaTime (robot, projectiles, explosions) (MovementCommand action) =
  (updateRobotVelocity robot (multiplyMovementAction deltaTime action), projectiles, explosions)

executeCommand _ (robot, projectiles, explosions) ShootCommand =
  case shootProjectile robot of
    Just projectile -> (afterShooting robot, projectile : projectiles, explosions)
    Nothing -> (robot, projectiles, explosions)

executeCommand _ (robot, projectiles, explosions) (WaitCommand time)
  | Map.member "waitingTime" (robotMemory robot) = (robot, projectiles, explosions) -- Si ya tiene waitingTime, según el control actual no debemos volver a establecerlo porque se crearía un bloqueo infinito.
  | otherwise = (robot { robotMemory = Map.insert "waitingTime" (ScalarValue time) (robotMemory robot)}, projectiles, explosions) -- Si no, establecemos waitingTime

executeCommand _ (robot, projectiles, explosions) (SetMemoryCommand key value) =
  (robot { robotMemory = Map.insert key value (robotMemory robot) }, projectiles, explosions)

executeCommand _ (robot, projectiles, explosions) (ClearMemoryCommand key) =
  (robot { robotMemory = Map.delete key (robotMemory robot) }, projectiles, explosions)

executeCommand deltaTime (robot, projectiles, explosions) (RotateTurretCommand angle) =
  (robot { robotTurret = turret { turretOrientation = turretOrientation turret + angle * deltaTime } }, projectiles, explosions)
  where turret = robotTurret robot

-- Actualiza un robot con su comportamiento AI
updateRobotAI :: Robot -> GameState -> Scalar -> AIExecutionResult
updateRobotAI robot gs deltaTime =
  let -- Actualizar cooldown de la torreta
      robotWithUpdatedCooldown = updateTurretCooldown robot deltaTime

      -- Obtener comportamiento por nombre
      behavior = getBehaviorByName (robotBehavior robot)

      -- Decidir comportamiento
      commands = decideBotBehavior behavior gs robotWithUpdatedCooldown

      -- Usamos trace para debug: Muestra el primer argumento y devuelve el segundo. Por ejecución perezosa hay que utilizar "debug".
      debug
        | floor (gameTime gs + deltaTime) > floor (gameTime gs) = trace (show (robotID robot) ++ ": " ++ show commands) commands
        | otherwise = commands
      -- Ejecutar comandos
      result = executeAICommands debug robotWithUpdatedCooldown deltaTime

      -- Actualizar tiempo de última actualización y eliminar waitingTime si procede.
      preFinalRobot = (updatedRobot result) { robotLastUpdateTime = gameTime gs }

      finalRobot
        | Map.member "waitingTime" (robotMemory preFinalRobot) = waitFinalRobot
        | otherwise = preFinalRobot
        where
          waitFinalRobot
            | memLEQ currentWaitingTime (ScalarValue 0) = preFinalRobot { robotMemory = Map.delete "waitingTime" (robotMemory preFinalRobot)}
            | otherwise = preFinalRobot
            where
              memLEQ :: MemoryValue -> MemoryValue -> Bool
              memLEQ (ScalarValue x) (ScalarValue y) = x <= y
              currentWaitingTime = (robotMemory preFinalRobot) Map.! "waitingTime"
  in result { updatedRobot = finalRobot }

-- Obtiene un comportamiento por su nombre
getBehaviorByName :: String -> BotBehavior
getBehaviorByName "aggressive" = aggressiveBot
getBehaviorByName "defensive" = defensiveBot
getBehaviorByName "stupid" = stupidBot
getBehaviorByName "turret" = turretBot
getBehaviorByName _ = aggressiveBot -- Comportamiento por defecto
