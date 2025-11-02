import Game
import Robot (createBasicRobot)
import GameState
import RandomUtils (generatePositionFromSeed)

import qualified Data.Map as Map (Map, fromList, insert, empty)
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
    let stageSize = (100, 70)  -- Tamaño del escenario de juego en unidades internas (ancho, alto)
    
    -- Obtener semilla inicial del tiempo del sistema
    time <- getPOSIXTime
    let seedBase = realToFrac time  -- Convertir el tiempo a Float para usarlo como semilla
    
    -- Calcular los límites (bounds) del área jugable
    -- bounds = (mitad del ancho, mitad del alto) → si stageSize = (100, 70), entonces bounds = (50, 35)
    -- Esto define que los robots pueden aparecer entre -50 y +50 en X, y entre -35 y +35 en Y
    let bounds = (fst stageSize / 2, snd stageSize / 2)
    
    -- Generar posiciones aleatorias para cada robot usando la misma semilla pero IDs diferentes
    -- Cada robot obtiene una posición única dentro del área definida por bounds
    let pos1 = generatePositionFromSeed bounds seedBase 1  -- Posición para robot ID 1
    let pos2 = generatePositionFromSeed bounds seedBase 2  -- Posición para robot ID 2
    let pos3 = generatePositionFromSeed bounds seedBase 3  -- Posición para robot ID 3
    
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
                                gameStageSize = (100, 70),
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
            let y = 0.3 - fromIntegral idx * 0.2
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
                newRobots = Map.fromList
                    [ (rid, createBasicRobot (generatePositionFromSeed bounds seedBase rid) behavior rid)
                    | (rid, behavior) <- cfgs ]
            in s { gameIsInMenu = False, gameRobots = newRobots }