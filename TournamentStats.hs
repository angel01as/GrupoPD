module TournamentStats (writeTournamentStats, writeAggregateStats) where

import System.IO
import qualified Data.Map as Map
import GameState

-- Escritura de estadísticas de un torneo individual
writeTournamentStats :: Handle -> Int -> GameState -> IO ()
writeTournamentStats h idx gs = do
  hPutStrLn h $ "--- Torneo " ++ show idx ++ " ---"
  let t = gameTime gs
      statsMap = gameStats gs
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
          hitsRec = hitsReceived s
          hitsLand = hitsLanded s
          shots = shotsFired s
          accuracy = if shots > 0 then (fromIntegral hitsLand / fromIntegral shots) * 100 else 0 :: Float
          ta = timeAlive s
          pct = if t > 0 then (ta / t) * 100 else 0
          robotKills = kills s
      hPutStrLn h $ "Robot " ++ show rid ++ ":"
      hPutStrLn h $ "  - Disparos realizados: " ++ show shots
      hPutStrLn h $ "  - Impactos logrados: " ++ show hitsLand ++ " (precisión: " ++ show accuracy ++ "%)"
      hPutStrLn h $ "  - Impactos recibidos: " ++ show hitsRec
      hPutStrLn h $ "  - Eliminaciones: " ++ show robotKills
      hPutStrLn h $ "  - Tiempo con vida: " ++ show ta ++ "s (" ++ show pct ++ "% del torneo)"

-- Escritura de estadísticas agregadas
writeAggregateStats :: Handle -> [Map.Map Int RobotStats] -> IO ()
writeAggregateStats h aggs = do
  hPutStrLn h "=== ESTADISTICAS AGREGADAS DE TODOS LOS TORNEOS ==="
  hPutStrLn h ""
  let allKeys = Map.keys $ head aggs
  mapM_ (writeFor aggs) allKeys
  where
    writeFor aggs k = do
      let lst = [ Map.findWithDefault emptyRobotStats k m | m <- aggs ]
          -- Estadísticas de disparos
          shotsList = map shotsFired lst
          hitsLandedList = map hitsLanded lst
          avgShots = if null shotsList then 0 else avgInt shotsList
          maxShots = if null shotsList then 0 else maximum shotsList
          totalShots = sum shotsList
          
          avgHitsLanded = if null hitsLandedList then 0 else avgInt hitsLandedList
          maxHitsLanded = if null hitsLandedList then 0 else maximum hitsLandedList
          totalHitsLanded = sum hitsLandedList
          
          avgAccuracy = if totalShots > 0 
                        then (fromIntegral totalHitsLanded / fromIntegral totalShots) * 100 
                        else 0 :: Float
          
          -- Estadísticas de impactos recibidos
          hitsRecList = map hitsReceived lst
          avgHitsRec = if null hitsRecList then 0 else avgInt hitsRecList
          maxHitsRec = if null hitsRecList then 0 else maximum hitsRecList
          
          -- Estadísticas de eliminaciones
          killsList = map kills lst
          avgKills = if null killsList then 0 else avgInt killsList
          maxKills = if null killsList then 0 else maximum killsList
          totalKills = sum killsList
          
          -- Estadísticas de tiempo vivo
          timeList = map timeAlive lst
          avgTime = if null timeList then 0 else avgF timeList
          maxTime = if null timeList then 0 else maximum timeList
          
      hPutStrLn h $ "Robot " ++ show k ++ ":"
      hPutStrLn h $ "  Disparos:"
      hPutStrLn h $ "    - Promedio: " ++ show avgShots
      hPutStrLn h $ "    - Máximo: " ++ show maxShots
      hPutStrLn h $ "    - Total: " ++ show totalShots
      hPutStrLn h $ "  Impactos logrados:"
      hPutStrLn h $ "    - Promedio: " ++ show avgHitsLanded
      hPutStrLn h $ "    - Máximo: " ++ show maxHitsLanded
      hPutStrLn h $ "    - Total: " ++ show totalHitsLanded
      hPutStrLn h $ "    - Precisión promedio: " ++ show avgAccuracy ++ "%"
      hPutStrLn h $ "  Impactos recibidos:"
      hPutStrLn h $ "    - Promedio: " ++ show avgHitsRec
      hPutStrLn h $ "    - Máximo: " ++ show maxHitsRec
      hPutStrLn h $ "  Eliminaciones:"
      hPutStrLn h $ "    - Promedio: " ++ show avgKills
      hPutStrLn h $ "    - Máximo: " ++ show maxKills
      hPutStrLn h $ "    - Total: " ++ show totalKills
      hPutStrLn h $ "  Tiempo con vida:"
      hPutStrLn h $ "    - Promedio: " ++ show avgTime ++ "s"
      hPutStrLn h $ "    - Máximo: " ++ show maxTime ++ "s"
      hPutStrLn h ""
    
    avgInt xs = fromIntegral (sum xs) / fromIntegral (length xs :: Int)
    avgF xs = sum xs / fromIntegral (length xs)

