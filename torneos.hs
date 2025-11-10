module Torneos where

import System.IO
import Data.List (intercalate)
import Data.Char (isSpace)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import Graphics.Gloss (Picture)
import Graphics.Gloss.Juicy (loadJuicy)

import Game (playGameWithCallback)
import GameState
import Rendering (usedImages)
import RandomUtils (generatePositionFromSeed, generateNonOverlappingObstacles)
import Robot (createBasicRobot)
import Entities (Obstacle(..))
import Geometry (Size)
import TournamentStats (writeTournamentStats, writeAggregateStats)

-- Parser muy simple de config.txt con formato:
-- bots: aggressive,defensive,sniper
-- stage: 200x140
-- tournaments: 10

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseConfig :: String -> ([(Int,String)], Size, Int)
parseConfig content = (zip ids botList, (w,h), tournaments)
  where
    ls = map (takeWhile (/='\r')) $ lines content
    findKey k = case [ drop 1 (dropWhile (/=':') l) | l <- ls, take (length k) l == k ] of
                  (v:_) -> trim v
                  [] -> error $ "Missing config key: " ++ k
    botsStr = findKey "bots"
    botList = map (trim) $ splitByComma botsStr
    ids = [1..length botList]
    stageStr = findKey "stage"
    (w,h) = case span (/='x') stageStr of
              (a, 'x':b) -> (read a :: Float, read b :: Float)
              _ -> error "stage must be WxH"
    tournaments = read (findKey "tournaments") :: Int

splitByComma :: String -> [String]
splitByComma s = case dropWhile (==',') s of
  "" -> []
  s' -> let (w, s'') = break (==',') s' in w : splitByComma s''

-- Cargar imágenes listadas en Rendering.usedImages
loadImages :: [FilePath] -> IO (Map.Map String Picture)
loadImages [] = return Map.empty
loadImages (p:ps) = do
  maybePic <- loadJuicy p
  rest <- loadImages ps
  case maybePic of
    Just pic -> return $ Map.insert p pic rest
    Nothing -> do
      putStrLn $ "[X] No se pudo cargar: " ++ p
      return rest

-- Construye un GameState inicial para un torneo
makeInitialState :: [(Int,String)] -> Size -> Double -> IO GameState
makeInitialState botConfigs stageSize seedBase = do
  -- Generar posiciones determinísticas para cada bot
  let ids = map fst botConfigs
      bounds = (fst stageSize / 2, snd stageSize / 2)
      positions = [ generatePositionFromSeed bounds seedBase rid | rid <- ids ]
      robots = Map.fromList [ (rid, createBasicRobot pos beh rid) | ((rid, beh), pos) <- zip botConfigs positions ]
      obstaclesList = generateNonOverlappingObstacles stageSize (realToFrac seedBase) (Map.elems robots)
      obstacles = Map.fromList [ (obstacleID o, o) | o <- obstaclesList ]
  imagesMap <- loadImages usedImages
  let gs = def { gameRobots = robots, gameObstacles = obstacles, gameStageSize = stageSize, gameImages = imagesMap, gameSeed = seedBase, gameBotConfigs = botConfigs, gameIsInMenu = False, gameStats = Map.fromList [ (i, emptyRobotStats) | i <- ids ] }
  return gs

-- Escritura de estadísticas (append)
appendTournamentStats :: Handle -> Int -> GameState -> IO ()
appendTournamentStats = writeTournamentStats

-- Función principal para lanzar torneos desde config.txt
runTournamentsFromConfig :: FilePath -> FilePath -> IO ()
runTournamentsFromConfig configPath outPath = do
  cfg <- readFile configPath
  let (botConfigs, stageSize, totalT) = parseConfig cfg
  baseTime <- getPOSIXTime
  let seedStart = realToFrac baseTime
  -- Limpiar archivo anterior (sobrescribir en lugar de agregar)
  writeFile outPath ""
  h <- openFile outPath AppendMode
  -- contador de torneos restantes y índice
  counterRef <- newIORef (1 :: Int)
  aggregateRef <- newIORef ([] :: [Map.Map Int RobotStats])

  let makeNextState :: Int -> IO GameState
      makeNextState k = makeInitialState botConfigs stageSize (seedStart + fromIntegral k)

  -- callback que se ejecuta al terminar cada partida
  let callback gs = do
        idx <- readIORef counterRef
        appendTournamentStats h idx gs
        modifyIORef' aggregateRef (gameStats gs :)
        if idx >= totalT
          then do
            -- escribir agregados
            aggs <- readIORef aggregateRef
            writeAggregates h aggs
            hClose h
            return Nothing
          else do
            writeIORef counterRef (idx + 1)
            nextState <- makeNextState (idx + 1)
            return (Just nextState)

  -- Estado inicial y lanzar playGameWithCallback
  initial <- makeNextState 1
  playGameWithCallback initial callback

writeAggregates :: Handle -> [Map.Map Int RobotStats] -> IO ()
writeAggregates = writeAggregateStats

-- Exported main helper
mainRunTournaments :: IO ()
mainRunTournaments = runTournamentsFromConfig "config.txt" "estadisticas.txt"
