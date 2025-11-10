module Torneos where

import System.IO
import Data.List (intercalate)
import Data.Char (isSpace, ord)
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

-- Simple hash function to generate seed from string
hashString :: String -> Double
hashString s = fromIntegral $ foldl (\acc c -> (acc * 31 + ord c) `mod` 1000000) 0 s

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
appendTournamentStats h idx gs = do
  hPutStrLn h $ "--- Torneo " ++ show idx ++ " ---"
  let t = gameTime gs
      statsMap = gameStats gs
      robots = Map.elems (gameRobots gs)
      robotIds = Map.keys statsMap
  mapM_ (writeRobotStat h t statsMap) robotIds
  let winner = case Map.keys (gameRobots gs) of
                 [rid] -> show rid
                 [] -> "None"
                 _ -> "Multiple"
  hPutStrLn h $ "Ganador: " ++ winner
  hPutStrLn h ""
  where
    writeRobotStat h t stmap rid = do
      let s = Map.findWithDefault emptyRobotStats rid stmap
          hits = hitsReceived s
          ta = timeAlive s
          pct = if t > 0 then (ta / t) * 100 else 0
      hPutStrLn h $ "Robot " ++ show rid ++ ": hits=" ++ show hits ++ ", tiempoVivo=" ++ show ta ++ ", pct=" ++ show pct ++ "%"

-- Función principal para lanzar torneos desde config.txt
runTournamentsFromConfig :: FilePath -> FilePath -> IO ()
runTournamentsFromConfig configPath outPath = do
  cfg <- readFile configPath
  let (botConfigs, stageSize, totalT) = parseConfig cfg
  let seedStart = hashString cfg + 12345.0
  h <- openFile outPath AppendMode
  -- contador de torneos restantes y índice
  counterRef <- newIORef (1 :: Int)
  aggregateRef <- newIORef ([] :: [Map.Map Int RobotStats])

  let makeNextState :: Int -> IO GameState
      makeNextState k = makeInitialState botConfigs stageSize (seedStart + fromIntegral k * 7919.0)

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
writeAggregates h aggs = do
  hPutStrLn h "--- Estadisticas agregadas ---"
  -- para cada robot calcular media de hits y maximo de hits y media tiempo vivo
  let allKeys = Map.keys $ head aggs
      byKey k = [ Map.findWithDefault emptyRobotStats k m | m <- aggs ]
      avg xs = fromIntegral (sum xs) / fromIntegral (length xs)
      maxVal xs = maximum xs
  mapM_ (writeFor allKeys aggs) allKeys
  where
    writeFor allKeys aggs k = do
      let lst = [ Map.findWithDefault emptyRobotStats k m | m <- aggs ]
          hitsList = map hitsReceived lst
          timeList = map timeAlive lst
          avgHits = if null hitsList then 0 else avgInt hitsList
          maxHits = if null hitsList then 0 else maximum hitsList
          avgTime = if null timeList then 0 else avgF timeList
      hPutStrLn h $ "Robot " ++ show k ++ ": avgHits=" ++ show avgHits ++ ", maxHits=" ++ show maxHits ++ ", avgTimeAlive=" ++ show avgTime
    avgInt xs = fromIntegral (sum xs) / fromIntegral (length xs :: Int)
    avgF xs = sum xs / fromIntegral (length xs)

-- Exported main helper
mainRunTournaments :: IO ()
mainRunTournaments = runTournamentsFromConfig "config.txt" "estadisticas.txt"
