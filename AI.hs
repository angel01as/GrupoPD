{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module AI (
  -- Tipos del DSL
  BotBehavior,
  BotCondition(..),
  BotCommand(..),
  BotInstruction(..),
  GameState(..),
  -- Funciones del DSL
  move, moveBackward, rotate, multiplyVelocity, shoot, wait, ifThen, ifThenElse, sequence,
  -- Condiciones
  hasTarget, isLowEnergy, isUnderAttack, distanceTo, angleTo,
  -- Decisor de comportamientos
  decideBotBehavior,
  -- Comportamientos de ejemplo
  aggressiveBot, defensiveBot, exampleBot
) where

import Robot (Robot(..), MovementAction(..), Turret(..), MemoryValue(..), isRobotAlive, detectedAgent)
import Entities (Projectile(..), GameEntity(..))
import Geometry (Position, Angle, Scalar, distanceBetween, angleToTarget)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Prelude hiding (sequence, repeat)

-- ============================================================================
-- DSL PARA ACCIONES DEL BOT
-- ============================================================================

-- Tipo de datos para representar el estado del juego
data GameState = GameState
  { gameRobots :: [Robot]   -- Lista de todos los robots
  , gameProjectiles :: [Projectile] -- Lista de todos los proyectiles
  , gameTime :: Scalar -- Tiempo actual del juego
  } deriving (Show, Eq)

-- Comandos básicos que puede ejecutar un bot
data BotCommand
  = MovementCmd MovementAction  -- Comando de movimiento 
  | ShootCmd                    -- Disparar
  | WaitCmd Scalar              -- Esperar un tiempo
  | SetMemoryCmd String MemoryValue  -- Guardar valor en memoria
  | ClearMemoryCmd String       -- Limpiar memoria
  deriving (Show, Eq)

-- Condiciones que puede evaluar un bot
data BotCondition
  = HasTarget                    -- ¿Tiene un enemigo cerca?
  | IsLowEnergy Scalar           -- ¿Energía baja?
  | IsUnderAttack                -- ¿Está siendo atacado?
  | DistanceToTarget Scalar      -- ¿Distancia al enemigo < X?
  | AngleToTarget Angle          -- ¿Ángulo al enemigo < X?
  | MemoryEquals String MemoryValue  -- ¿Memoria tiene valor X?
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
type BotBehavior = GameState -> Robot -> BotInstruction

-- ============================================================================
-- FUNCIONES DEL DSL
-- ============================================================================

-- Comandos básicos
move :: Scalar -> BotInstruction
move speed = Simple (MovementCmd (MoveForward speed))

moveBackward :: Scalar -> BotInstruction
moveBackward speed = Simple (MovementCmd (MoveBackward speed))

rotate :: Angle -> BotInstruction
rotate angle = Simple (MovementCmd (Rotate angle))

multiplyVelocity :: Scalar -> BotInstruction
multiplyVelocity factor = Simple (MovementCmd (MultiplyVelocity factor))

shoot :: BotInstruction
shoot = Simple ShootCmd

wait :: Scalar -> BotInstruction
wait time = Simple (WaitCmd time)

setMemory :: String -> MemoryValue -> BotInstruction
setMemory key value = Simple (SetMemoryCmd key value)

clearMemory :: String -> BotInstruction
clearMemory key = Simple (ClearMemoryCmd key)

-- Combinadores de instrucciones
ifThen :: BotCondition -> BotInstruction -> BotInstruction
ifThen cond instruction = Conditional cond instruction (Simple (WaitCmd 0))

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

memoryEquals :: String -> MemoryValue -> BotCondition
memoryEquals = MemoryEquals

-- ============================================================================
-- EVALUADOR DE CONDICIONES
-- ============================================================================

-- Evalúa una condición y devuelve True o False
evalCondition :: BotCondition -> GameState -> Robot -> Bool

-- CONDICIÓN: ¿Tiene el robot un objetivo en rango de su radar?
evalCondition HasTarget gs robot = 
  any (\r -> r /= robot && detectedAgent robot r) (gameRobots gs)

-- CONDICIÓN: ¿La energía del robot está por debajo del umbral?
evalCondition (IsLowEnergy threshold) _ robot = robotEnergy robot < threshold

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
evalCondition (MemoryEquals key value) _ robot = 
  case Map.lookup key (robotMemory robot) of
    Nothing -> False
    Just memValue -> value == memValue

-- OPERADORES LÓGICOS
evalCondition (Not cond) gs robot = not (evalCondition cond gs robot)
evalCondition (And cond1 cond2) gs robot = 
  evalCondition cond1 gs robot && evalCondition cond2 gs robot
evalCondition (Or cond1 cond2) gs robot = 
  evalCondition cond1 gs robot || evalCondition cond2 gs robot

-- Encuentra el enemigo más cercano
findNearestEnemy :: Robot -> [Robot] -> Maybe Robot
findNearestEnemy robot enemies = 
  let aliveEnemies = filter (\r -> r /= robot && isRobotAlive r) enemies
  in if null aliveEnemies 
     then Nothing
     else Just (minimumBy (\a b -> compare (distanceBetween (position robot) (position a)) 
                                           (distanceBetween (position robot) (position b))) aliveEnemies)

-- ============================================================================
-- DECISOR DE COMPORTAMIENTOS
-- ============================================================================

-- Decide qué instrucciones debe ejecutar un bot basado en su comportamiento y el estado del juego
decideBotBehavior :: BotBehavior -> GameState -> Robot -> [BotCommand]
decideBotBehavior botBehavior gs robot = 
  let instruction = botBehavior gs robot
  in decideInstruction instruction gs robot

-- Decide qué comandos ejecutar basado en una instrucción
decideInstruction :: BotInstruction -> GameState -> Robot -> [BotCommand]
decideInstruction (Simple cmd) _ _ = [cmd]
decideInstruction (Conditional cond thenInstruction elseInstruction) gs robot =
  if evalCondition cond gs robot
  then decideInstruction thenInstruction gs robot
  else decideInstruction elseInstruction gs robot
decideInstruction (Sequence instructions) gs robot = 
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
      rotate (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - orientation robot),
      shoot,
      move 1.0
    ])
    (sequence [
      -- Si no tiene objetivo, buscar
      setMemory "mode" (StringValue "searching"),
      rotate (pi/4),  -- Girar 45 grados
      move 0.5,
      wait 0.1
    ])

-- Bot defensivo que evita enemigos cuando tiene poca energía
defensiveBot :: BotBehavior
defensiveBot gs robot = 
  ifThenElse (isLowEnergy 30)
    (sequence [
      -- Si tiene poca energía, huir
      setMemory "mode" (StringValue "fleeing"),
      moveBackward 2.0,  -- Moverse hacia atrás
      rotate (pi),  -- Girar 180 grados
      wait 1.0
    ])
    (ifThenElse hasTarget
      (sequence [
        -- Si tiene objetivo y energía suficiente, atacar
        setMemory "mode" (StringValue "attacking"),
        rotate (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - orientation robot),
        shoot
      ])
      (sequence [
        -- Patrullar
        setMemory "mode" (StringValue "patrolling"),
        move 0.3,
        rotate (pi/8),
        wait 0.2
      ])
    )

-- Bot de ejemplo simple
exampleBot :: BotBehavior
exampleBot = aggressiveBot
