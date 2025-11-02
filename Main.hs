import Game (playGame, generateRandomObstacles, generateRandomObstaclesWithRobots)
import Robot (createBasicRobot, Robot(..))
import GameState
import RandomUtils (generatePositionFromSeed, generateSafeRobotPosition, generateNonOverlappingObstacles)
import Entities (Obstacle(..))

import qualified Data.Map as Map (Map, fromList, insert, empty, toList)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)

import Graphics.Gloss(Picture)
import Graphics.Gloss.Juicy(loadJuicy)

import Rendering(usedImages)
import Data.Default
import UIButton

-- Lista de comportamientos disponibles en orden cíclico
-- Si hay 1 bot: aggressive
-- Si hay 2 bots: aggressive, aggressive
-- Si hay 3 bots: aggressive, defensive, sniper
-- Si hay 4+ bots: se repite el ciclo
availableBehaviors :: [String]
availableBehaviors = ["aggressive", "defensive", "sniper"]

-- Ciclar comportamiento a la derecha
cycleBehavior :: String -> String
cycleBehavior b = case b of
    "aggressive" -> "defensive"
    "defensive"  -> "sniper"
    "sniper"     -> "aggressive"
    _             -> "aggressive"

-- Ciclar comportamiento a la izquierda
cycleBehaviorPrev :: String -> String
cycleBehaviorPrev b = case b of
    "aggressive" -> "sniper"
    "defensive"  -> "aggressive"
    "sniper"     -> "defensive"
    _             -> "aggressive"

-- Punto de entrada al juego.

main :: IO()
main = do
    -- Estado inicial del torneo: robots en posiciones aleatorias
    let window = (1000, 700)  -- Tamaño de la ventana en píxeles (ancho, alto)
    let stageSize = (200, 140)  -- Tamaño del escenario de juego en unidades internas (ancho, alto) - AUMENTADO
    
    -- Obtener semilla inicial del tiempo del sistema
    time <- getPOSIXTime
    let seedBase = realToFrac time  -- Convertir el tiempo a Float para usarlo como semilla
    
    -- Calcular los límites (bounds) del área jugable
    -- bounds = (mitad del ancho, mitad del alto) → si stageSize = (200, 140), entonces bounds = (100, 70)
    -- Esto define que los robots pueden aparecer entre -100 y +100 en X, y entre -70 y +70 en Y
    let bounds = (fst stageSize / 2, snd stageSize / 2)
    
    -- Generar posiciones aleatorias para cada robot verificando que no se solapen
    -- Distancia mínima entre robots: 15 unidades
    let minRobotDistance = 15.0 :: Float
    let generateRobotPositions :: Int -> Int -> [(Float, Float)] -> [(Float, Float)]
        generateRobotPositions currentId maxId existingPositions
            | currentId > maxId = []
            | otherwise =
                let candidatePos = findSafeRobotPos currentId existingPositions 0
                in candidatePos : generateRobotPositions (currentId + 1) maxId (candidatePos : existingPositions)
        
        findSafeRobotPos :: Int -> [(Float, Float)] -> Int -> (Float, Float)
        findSafeRobotPos rid existingPos attempt
            | attempt >= 500 = generatePositionFromSeed bounds (seedBase * fromIntegral rid) rid  -- Fallback después de 500 intentos
            | otherwise =
                let candidatePos = generatePositionFromSeed bounds (seedBase + fromIntegral attempt * 0.137) rid
                    tooClose = any (\pos -> distanceBetween candidatePos pos < minRobotDistance) existingPos
                in if tooClose then findSafeRobotPos rid existingPos (attempt + 1) else candidatePos
        
        distanceBetween (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
    
    let [pos1, pos2, pos3] = generateRobotPositions 1 3 []
    
    -- Robots iniciales con comportamientos variados
    let robots = Map.fromList
            [ 
                (1, createBasicRobot pos1 "aggressive" 1),
                (2, createBasicRobot pos2 "defensive" 2),
                (3, createBasicRobot pos3 "sniper" 3)
            ]
    imagesMap <- loadImages usedImages
    let initialState = def 
                            { 
                                gameWindowSize = window,
                                gameRobots = robots,
                                gameStageSize = stageSize,  -- Usar el stageSize definido arriba (200, 140)
                                gameImages = imagesMap,
                                gameSeed = seedBase,
                                gameBotConfigs = [(1,"aggressive"),(2,"defensive"),(3,"sniper")],
                                gameButtons = makeButtons def { gameBotConfigs = [(1,"aggressive"),(2,"defensive"),(3,"sniper")], gameWindowSize = window },
                                gameTotalRobotCount = 3  -- Iniciar con 3 robots por defecto
                            }
    playGame initialState

-- Carga varias imágenes y devuelve un diccionario con las rutas como clave
loadImages :: [String] -> IO (Map.Map String Picture)
loadImages [] = pure Map.empty -- Como IO es una mónada hay que envolver los valores
loadImages (p:ps) = do
  maybePic <- loadJuicy p
  rest <- loadImages ps
  case maybePic of
    Just pic -> pure $ Map.insert p pic rest
    Nothing  -> do
      putStrLn $ "[X] No se pudo cargar: " ++ p
      pure rest

makeButtons :: GameState -> [UIButton GameState]
makeButtons gs =
    concat
        [ [decCountBtn, incCountBtn]
        , concatMap rowButtons (zip [0..] (gameBotConfigs gs))
        , [playButton]
        ]
    where
        -- reconstruye botones tras cualquier cambio
        rebuild :: GameState -> GameState
        rebuild s = s { gameButtons = makeButtons s }

        -- Control del número de tanques
        decCountBtn = UIButton
            { buttonPosition = (-0.25, 0.6)
            , buttonSize     = (0.08, 0.10)
            , buttonText     = "<"
            , buttonHandler  = \s -> let n = length (gameBotConfigs s)
                                                                in if n > 1
                                                                         then rebuild $ s { gameBotConfigs = take (n-1) (gameBotConfigs s)
                                                                                                            , gameTotalRobotCount = n-1 }
                                                                         else s
            }
        incCountBtn = UIButton
            { buttonPosition = (0.25, 0.6)
            , buttonSize     = (0.08, 0.10)
            , buttonText     = ">"
            , buttonHandler  = \s ->
                let n = length (gameBotConfigs s)
                    nextId = if null (gameBotConfigs s) then 1 else (maximum (map fst (gameBotConfigs s)) + 1)
                in rebuild $ s { gameBotConfigs = gameBotConfigs s ++ [(nextId, "aggressive")]
                               , gameTotalRobotCount = n+1 }
            }

        -- Botones por fila para ciclar el tipo de cada tanque
        rowButtons :: (Int, (Int, String)) -> [UIButton GameState]
        rowButtons (idx, (rid, beh)) =
            let n = max 1 (length (gameBotConfigs gs))
                spacing = 0.6 / fromIntegral n
                yTop = 0.2
                y = yTop - fromIntegral idx * spacing
            in [ UIButton { buttonPosition = (-0.15, y), buttonSize = (0.08, 0.10), buttonText = "<"
                                        , buttonHandler = \s -> let upd = map (\(i,b) -> if i==rid then (i, cycleBehaviorPrev b) else (i,b)) (gameBotConfigs s)
                                                                                         in rebuild $ s { gameBotConfigs = upd } }
                 , UIButton { buttonPosition = (0.15, y), buttonSize = (0.08, 0.10), buttonText = ">"
                                        , buttonHandler = \s -> let upd = map (\(i,b) -> if i==rid then (i, cycleBehavior b) else (i,b)) (gameBotConfigs s)
                                                                                         in rebuild $ s { gameBotConfigs = upd } }
                 ]

        playButton = UIButton 
            { buttonPosition = (0, -0.7)
            , buttonSize     = (0.8, 0.2)
            , buttonText     = "Jugar"
            , buttonHandler  = startGame
            }

        -- Inicia juego a partir de gameBotConfigs
        startGame :: GameState -> GameState
        startGame s =
            let seedBase = gameSeed s
                stageSize = gameStageSize s
                bounds = (fst stageSize / 2, snd stageSize / 2)
                cfgs = if null (gameBotConfigs s)
                         then [(1, "aggressive")]
                         else gameBotConfigs s
                
                -- Función auxiliar para verificar distancia entre robots
                minRobotDist = 15.0 :: Float
                distanceBetween (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
                
                -- PASO 1: Generar robots primero (sin obstáculos aún)
                generateRobotsSequentially :: [(Int, String)] -> [(Float, Float)] -> [(Int, Robot)]
                generateRobotsSequentially [] _ = []
                generateRobotsSequentially ((rid, behavior):rest) existingPositions =
                    let safePos = findSafePosition rid 0 existingPositions
                        robot = createBasicRobot safePos behavior rid
                    in (rid, robot) : generateRobotsSequentially rest (safePos : existingPositions)
                
                findSafePosition :: Int -> Int -> [(Float, Float)] -> (Float, Float)
                findSafePosition rid attempt existingPos
                    | attempt >= 500 = generatePositionFromSeed bounds (seedBase * fromIntegral rid) rid  -- Fallback sin obstáculos
                    | otherwise =
                        let candidatePos = generatePositionFromSeed bounds (seedBase + fromIntegral attempt * 0.137) rid
                            tooCloseToRobot = any (\pos -> distanceBetween candidatePos pos < minRobotDist) existingPos
                        in if tooCloseToRobot then findSafePosition rid (attempt + 1) existingPos else candidatePos
                
                newRobots = Map.fromList (generateRobotsSequentially cfgs [])
                
                -- PASO 2: Ahora generar obstáculos verificando posiciones de robots
                robotPositions = [robotPosition r | (_, r) <- Map.toList newRobots]
                newObstaclesList = generateRandomObstaclesWithRobots stageSize (realToFrac seedBase) robotPositions
                newObstacles = Map.fromList [ (obstacleID o, o) | o <- newObstaclesList ]
                
            in s { gameIsInMenu = False
                 , gameRobots = newRobots
                 , gameObstacles = newObstacles
                 }