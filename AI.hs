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
  aggressiveBot, defensiveBot, exampleBot, sniperBot
) where

import Robot (Robot(..), MovementAction(..), Turret(..), MemoryValue(..), isRobotAlive, detectedAgent, updateRobotVelocity, canShoot, shootProjectile, afterShooting, updateTurretCooldown, multiplyMovementAction, getMovementActionValue)
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

-- Bot agresivo que busca enemigos y los ataca de frente
aggressiveBot :: BotBehavior
aggressiveBot gs robot =
  ifThenElse isNearMapEdgeCondition
    -- PRIORIDAD 1: Evitar bordes del mapa
    (sequence [
      setMemory "mode" (StringValue "avoiding_edge"),
      rotate (pi/2),  -- Girar 90 grados
      wait 0.3
    ])
    (ifThenElse hasTarget
      -- PRIORIDAD 2: Si detecta enemigo, atacar
      (let enemy = fromMaybe robot (findNearestEnemy robot (gameRobots gs))
           targetPos = position enemy
           distToEnemy = distanceBetween (position robot) targetPos
           targetAngle = angleToTarget (position robot) targetPos
           turretAngle = turretOrientation (robotTurret robot)
           angleDiff = normalizeAngle (targetAngle - turretAngle)
           turretAligned = abs angleDiff < 0.15  -- Tolerancia de ~8.6 grados
       in sequence [
         setMemory "mode" (StringValue "attacking"),
         
         -- Rotar torreta hacia el enemigo
         ifThenElse (BoolCondition (not turretAligned))
           (rotateTurret angleDiff)
           (Simple ClearBlockCommand),
         
         -- Movimiento según distancia
         ifThenElse (BoolCondition (distToEnemy > 15))
           (move 0.8)  -- Avanzar si está lejos
           (ifThenElse (BoolCondition (distToEnemy < 8))
             (moveBackward 0.5)  -- Retroceder si está muy cerca
             (Simple DoNothingCommand)),  -- Mantener posición en distancia media
         
         -- Disparar solo si la torreta está alineada y tiene energía
         ifThen (And (BoolCondition turretAligned) (Not (isLowEnergy 15))) shoot,
         wait 0.1
       ])
      -- PRIORIDAD 3: Patrullar si no hay enemigos
      (sequence [
        setMemory "mode" (StringValue "patrolling"),
        move 0.5,
        rotate (pi/16),  -- Girar lentamente
        wait 0.2
      ])
    )

-- Bot francotirador que mantiene distancia y dispara con precisión
sniperBot :: BotBehavior
sniperBot gs robot =
  ifThenElse isNearMapEdgeCondition
    -- PRIORIDAD 1: Evitar bordes
    (sequence [
      setMemory "mode" (StringValue "avoiding_edge"),
      rotate (pi/2),
      wait 0.3
    ])
    (ifThenElse hasTarget
      -- PRIORIDAD 2: Si detecta enemigo, mantener distancia óptima
      (let enemy = fromMaybe robot (findNearestEnemy robot (gameRobots gs))
           targetPos = position enemy
           distToEnemy = distanceBetween (position robot) targetPos
           targetAngle = angleToTarget (position robot) targetPos
           turretAngle = turretOrientation (robotTurret robot)
           angleDiff = normalizeAngle (targetAngle - turretAngle)
           turretAligned = abs angleDiff < 0.1  -- Más precisión que agresivo
           optimalDistance = 30  -- Distancia óptima de francotirador
       in sequence [
         setMemory "mode" (StringValue "sniping"),
         
         -- SIEMPRE rotar torreta hacia enemigo (no rotar el cuerpo)
         rotateTurret angleDiff,
         
         -- Ajustar distancia sin perder la orientación
         ifThenElse (BoolCondition (distToEnemy > optimalDistance + 5))
           (move 0.3)  -- Avanzar lentamente si está demasiado lejos
           (ifThenElse (BoolCondition (distToEnemy < optimalDistance - 5))
             (moveBackward 0.4)  -- Retroceder si está muy cerca
             (Simple DoNothingCommand)),  -- Mantener posición óptima
         
         -- Disparar solo si está perfectamente alineado y en rango
         ifThen (And (BoolCondition turretAligned) 
                     (And (BoolCondition (distToEnemy <= turretRange (robotTurret robot))) 
                          (Not (isLowEnergy 20)))) 
                shoot,
         
         wait 0.2  -- Pausas más largas entre acciones
       ])
      -- PRIORIDAD 3: Buscar enemigos sin moverse mucho
      (sequence [
        setMemory "mode" (StringValue "searching"),
        rotate (pi/8),  -- Girar lentamente buscando
        wait 0.5  -- Esperar más tiempo
      ])
    )

-- Bot defensivo que se protege y calcula sus movimientos
defensiveBot :: BotBehavior
defensiveBot gs robot =
  ifThenElse isNearMapEdgeCondition
    -- PRIORIDAD 1: Evitar bordes
    (sequence [
      setMemory "mode" (StringValue "avoiding_edge"),
      rotate (pi/2),
      wait 0.3
    ])
    (ifThenElse (isLowEnergy 40)
      -- PRIORIDAD 2: Si tiene poca energía, huir
      (sequence [
        setMemory "mode" (StringValue "fleeing"),
        ifThenElse hasTarget
          (let enemy = fromMaybe robot (findNearestEnemy robot (gameRobots gs))
               targetPos = position enemy
               targetAngle = angleToTarget (position robot) targetPos
               -- Ángulo opuesto para huir
               escapeAngle = normalizeAngle (targetAngle + pi)
               robotAngle = orientation robot
               turnDiff = normalizeAngle (escapeAngle - robotAngle)
           in sequence [
             rotate (turnDiff * 0.5),  -- Girar para alejarse
             move 0.7,  -- Moverse rápido
             wait 0.2
           ])
          (sequence [
            move 0.4,
            rotate (pi/6),
            wait 0.3
          ])
      ])
      -- PRIORIDAD 3: Si tiene energía, defender
      (ifThenElse hasTarget
        (let enemy = fromMaybe robot (findNearestEnemy robot (gameRobots gs))
             targetPos = position enemy
             distToEnemy = distanceBetween (position robot) targetPos
             targetAngle = angleToTarget (position robot) targetPos
             turretAngle = turretOrientation (robotTurret robot)
             angleDiff = normalizeAngle (targetAngle - turretAngle)
             turretAligned = abs angleDiff < 0.15
             safeDistance = 12  -- Distancia de seguridad
         in sequence [
           setMemory "mode" (StringValue "defending"),
           
           -- Rotar torreta hacia enemigo
           rotateTurret angleDiff,
           
           -- Mantener distancia de seguridad
           ifThenElse (BoolCondition (distToEnemy < safeDistance))
             (moveBackward 0.5)  -- Alejarse si está muy cerca
             (ifThenElse (BoolCondition (distToEnemy > safeDistance + 10))
               (move 0.4)  -- Acercarse si está muy lejos
               (Simple DoNothingCommand)),  -- Mantener posición
           
           -- Disparar si está alineado
           ifThen (And (BoolCondition turretAligned) (Not (isLowEnergy 15))) shoot,
           wait 0.2
         ])
        -- PRIORIDAD 4: Patrullar con cautela
        (sequence [
          setMemory "mode" (StringValue "patrolling"),
          move 0.3,
          rotate (pi/12),
          wait 0.4
        ])
      )
    )

-- Normaliza un ángulo al rango [-pi, pi]
normalizeAngle :: Angle -> Angle
normalizeAngle angle = angle - 2 * pi * fromIntegral (round (angle / (2 * pi)) :: Int)

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

-- Crea un punto de bloqueo en una secuencia de BotCommand. NO es un control de flujo completo.
-- Usar esta función en todas las acciones bloqueantes: Wait, MovementCommand, RotateTurret
-- Hay que actualizar el valor de acumulator en cada frame. Se debe cumplir acumulator >= acumulatorEnd, terminando el bloqueo cuando acumulator <= acumulatorEnd.
-- Estas actualizaciones en principio deben hacerse en executeCommand.
createBlockPoint :: Robot -> Int -> Int -> Scalar -> Scalar -> Robot
createBlockPoint robot blockPoint sequenceLength acumulator acumulatorEnd = robot { robotMemory = updatedMemory}
  where
    updatedMemory = Map.insert "blockPoint" (IntValue blockPoint) $
                    Map.insert "sequenceLength" (IntValue sequenceLength) $
                    Map.insert "blockPointAcumulator" (ScalarValue acumulator) $
                    Map.insert "blockPointAcumulatorEnd" (ScalarValue acumulatorEnd) (robotMemory robot)

-- Limpia el punto de bloqueo
clearBlockPoint :: Robot -> Robot
clearBlockPoint robot = robot { robotMemory = updatedMemory}
  where
    updatedMemory = Map.delete "blockPoint" $
                    Map.delete "sequenceLength" $
                    Map.delete "blockPointAcumulator" $
                    Map.delete "blockPointAcumulatorEnd" (robotMemory robot)

-- Ejecuta una lista de comandos AI sobre un robot, respetando el tiempo de espera.
executeAICommands :: [BotCommand] -> Robot -> Scalar -> AIExecutionResult
-- executeAICommands commands robot deltaTime =
--   let (updatedRobot', spawnedProjectiles', spawnedExplosions') = foldl (executeCommand deltaTime) (robot, [], []) commands
--   in AIExecutionResult updatedRobot' spawnedProjectiles' spawnedExplosions'
executeAICommands commands robot' deltaTime = AIExecutionResult updatedRobot' spawnedProjectiles' spawnedExplosions'
  where
  (preProcessedCommands, preProcessedRobot) = preProcessCommands commands robot'
  (updatedRobot', spawnedProjectiles', spawnedExplosions') = processCommands preProcessedCommands (preProcessedRobot, [], [])

  preProcessCommands :: [BotCommand] -> Robot -> ([BotCommand], Robot)
  preProcessCommands [] s = ([], s)
  preProcessCommands cmds robot
      -- Si el tiempo de espera restante es mayor que el transcurrido desde el último frame no ejecutamos nada y actualizamos el tiempo de espera restante.
      -- Map.adjust usa el valor asociado a la clave en una función que devuelve el nuevo valor.
    -- | memGT waitingTime (ScalarValue deltaTime) = ([], robot { robotMemory = Map.adjust (\(ScalarValue x) -> memMax (ScalarValue (x - deltaTime)) (ScalarValue 0)) "waitingTime" (robotMemory robot)})
    | Map.member "blockPoint" (robotMemory robot) = (nextCmds, updatedRobot)
    | otherwise = (cmds, robot)
    where
      (nextCmds, updatedRobot)
        -- Si el valor de sequenceLength no coincide con length cmds ha habido un cambio de rama y hay que limpiar.
        | sequenceLength /= length cmds = (cmds, cleanRobot)
        -- Si acumulator < acumulatorEnd + margen de error, ha terminado el bloqueo y debemos continuar la ejecución.
        | acumulator < acumulatorEnd + 1e-5 = (jumpCmds, cleanRobot)
        -- En este caso solo debe seguir la ejecución de la acción bloqueante y quizá de las siguientes.
        | otherwise = (jumpCmds, updatedJumpRobot)
        where 
          -- El valor de la clave blockPoint será un un IntValue (índice del salto a realizar según el último bloqueo)
          memory = robotMemory robot
          blockPoint = (\(IntValue x) -> x) $ memory Map.! "blockPoint"
          sequenceLength = (\(IntValue a) -> a) $ memory Map.! "sequenceLength"
          acumulator = (\(ScalarValue x) -> x) $ memory Map.! "blockPointAcumulator"
          acumulatorEnd = (\(ScalarValue x) -> x) $ memory Map.! "blockPointAcumulatorEnd"
          blockingCmd = cmds !! blockPoint -- Comando de bloqueo
          jumpCmds = drop (blockPoint + 1) cmds -- Comandos después del bloqueo
          cleanRobot = clearBlockPoint robot
          (updatedJumpRobot, _, _) = executeCommand deltaTime (robot, [], []) (blockPoint, length commands) blockingCmd

  processCommands :: [BotCommand] -> (Robot, [Projectile], [Explosion]) -> (Robot, [Projectile], [Explosion])
  processCommands [] (robot, projectiles, explosions) = (robot, projectiles, explosions)
  processCommands (cmd:cmds) (robot, projectiles, explosions)
    | Map.member "blockPoint" (robotMemory robot) = (robot, projectiles, explosions)
    | otherwise = processCommands cmds (executeCommand deltaTime (robot, projectiles, explosions) (currentIndex, totalCommands) cmd)
    where
      totalCommands = length commands
      currentIndex = totalCommands - length cmds - 1


-- Ejecuta un comando individual
type BotCommandIndex = Int
type BotCommandContext = (BotCommandIndex, Int) -- Índice y número total de comandos en la secuencia

executeCommand :: Scalar -> (Robot, [Projectile], [Explosion]) -> BotCommandContext -> BotCommand -> (Robot, [Projectile], [Explosion])
-- Bloqueante
executeCommand deltaTime (robot, projectiles, explosions) (index, seqLen) (MovementCommand action)
  | Map.member "blockPoint" (robotMemory robot) = (updatedRobotWhileBlocking, projectiles, explosions)
  | otherwise = (createBlockPoint appliedRobot index seqLen (getMovementActionValue action - appliedActionValue) 0, projectiles, explosions)
  where
    appliedAction = multiplyMovementAction deltaTime action
    appliedActionValue = getMovementActionValue appliedAction
    appliedRobot = updateRobotVelocity robot appliedAction
    acc = (\(ScalarValue x) -> x) $ (robotMemory robot) Map.! "blockPointAcumulator"
    updatedRobotWhileBlocking
      | acc - appliedActionValue > 1e-5 = appliedRobot { robotMemory = Map.insert "blockPointAcumulator" (ScalarValue (acc - appliedActionValue)) (robotMemory appliedRobot) }
      | otherwise = clearBlockPoint appliedRobot

executeCommand _ (robot, projectiles, explosions) _ ShootCommand =
  case shootProjectile robot of
    Just projectile -> (afterShooting robot, projectile : projectiles, explosions)
    Nothing -> (robot, projectiles, explosions)
-- Bloqueante
executeCommand deltaTime (robot, projectiles, explosions) (index, seq) (WaitCommand time)
  | Map.member "blockPoint" (robotMemory robot) = (updatedRobot, projectiles, explosions) -- Ya está bloqueando, tenemos que actualizar el acumulador.
  | otherwise = (createBlockPoint robot index seq time 0, projectiles, explosions) -- No está bloqueando, empezamos a bloquear.
  where 
    waitingTime = (\(ScalarValue x) -> x) $ (robotMemory robot) Map.! "blockPointAcumulator"

    updatedRobot
      | waitingTime - deltaTime > 1e-5 = robot { robotMemory = Map.insert "blockPointAcumulator" (ScalarValue (waitingTime - deltaTime)) (robotMemory robot)}
      | otherwise = clearBlockPoint robot

executeCommand _ (robot, projectiles, explosions) _ (SetMemoryCommand key value) =
  (robot { robotMemory = Map.insert key value (robotMemory robot) }, projectiles, explosions)

executeCommand _ (robot, projectiles, explosions) _ (ClearMemoryCommand key) =
  (robot { robotMemory = Map.delete key (robotMemory robot) }, projectiles, explosions)
executeCommand _ (robot, projectiles, explosions) _ DoNothingCommand =
  (robot, projectiles, explosions)

executeCommand _ (robot, projectiles, explosions) _ ClearBlockCommand =
  (clearBlockPoint robot, projectiles, explosions)
-- Bloqueante
executeCommand deltaTime (robot, projectiles, explosions) (index, seqLen) (RotateTurretCommand angle)
  | Map.member "blockPoint" (robotMemory robot) = (updatedRobotWhileBlocking, projectiles, explosions)
  | otherwise = (createBlockPoint appliedRobot index seqLen (angle - appliedAngle) 0, projectiles, explosions)
  where
    turret = robotTurret robot
    appliedAngle = angle * deltaTime
    appliedRobot = robot { robotTurret = turret { turretOrientation = turretOrientation turret + appliedAngle } }
    acc = (\(ScalarValue x) -> x) $ (robotMemory robot) Map.! "blockPointAcumulator" 
    updatedRobotWhileBlocking
      | acc - appliedAngle > 1e-5 = appliedRobot { robotMemory = Map.insert "blockPointAcumulator" (ScalarValue (acc - appliedAngle)) (robotMemory appliedRobot) }
      | otherwise = clearBlockPoint appliedRobot

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
