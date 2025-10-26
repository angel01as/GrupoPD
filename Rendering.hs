module Rendering(drawGame, usedImages) where

import Graphics.Gloss hiding (Vector, Point)
import Entities
import GameState
import Robot(Robot(..), Turret(..), isRobotAlive)
import Geometry
import qualified Data.Map as Map
import UIButton
import WindowSizeState

import Numeric (showFFloat)
import Graphics.Gloss.Juicy (loadJuicy)

-- Rutas a imágenes
backgroundImagePath = "images/background.jpg"
robotImagePath = "images/tankbody2.png" -- Cambiar
projetileImagePath = "images/tankbullet2.png" -- Cambiar
explosionImagePath = "" -- Cambiar

usedImages = [backgroundImagePath, robotImagePath, projetileImagePath, explosionImagePath]

-- gmap es un fmap sobre un Map que devuelve el resultado como lista.
gmap :: (Ord k) => (v -> w) -> Map.Map k v -> [w]
gmap f m = Map.elems (fmap f m)

-- gfilter es un filter sobre un Map que devuelve el resultado como lista.
gfilter :: (Ord k) => (v -> Bool) -> Map.Map k v -> [v]
gfilter f m = Map.elems (Map.filter f m)

-- Dimensiones base del juego. Usar Scalar para cáculos internos y Pixel para la pantalla., incluso si ambos son Float.

type Pixel = Float

-- Factor de escalado dinámico basado en el tamaño de ventana
baseWindowSize :: (Int, Int)
baseWindowSize = (1000, 700)

-- Calcula el factor de escalado dinámico
getMeter2PixelFactor :: GameState -> Pixel
getMeter2PixelFactor gs = 
  min (fromIntegral windowWidth / sceneWidth)
      (fromIntegral windowHeight / sceneHeight)
      where
        (windowWidth, windowHeight) = gameWindowSize gs
        (sceneWidth, sceneHeight) = gameStageSize gs

getPixel2MeterFactor :: GameState -> Scalar
getPixel2MeterFactor gs = 1 / getMeter2PixelFactor gs

pixel2Meter :: GameState -> Pixel -> Scalar
pixel2Meter gs px = px * getPixel2MeterFactor gs

meter2Pixel :: GameState -> Scalar -> Pixel
meter2Pixel gs m = m * getMeter2PixelFactor gs

-- === DIBUJO ===
drawGame :: GameState -> Picture
drawGame gs
  | gameIsInMenu gs = drawMenu
  | gameDebugInfo gs = Pictures [regularGameInfo, debugGameInfo]
  | otherwise = regularGameInfo
  where
    regularGameInfo = Pictures [background, robotsPic, projectilesPic, explosionsPic, ui]
    debugGameInfo = Pictures [createBorder, drawAllVertices]
    windowSize' = gameWindowSize gs
    (windowWidth, windowHeight) = (fromIntegral (fst windowSize'), fromIntegral (snd windowSize'))
    (baseWindowWidth, baseWindowHeight) = (fromIntegral $ fst baseWindowSize, fromIntegral $ snd baseWindowSize)
    
    createBorder :: Picture
    createBorder = Color red $ rectangleWire (meter2Pixel gs sx) (meter2Pixel gs sy) where (sx, sy) = gameStageSize gs    

    -- Usar imagen de fondo si está disponible, sino usar color sólido
    background = case Map.lookup backgroundImagePath (gameImages gs) of
      Just bgImage -> Scale (windowWidth / baseWindowWidth) (windowHeight / baseWindowHeight) bgImage
      Nothing      -> Color (makeColor 0.1 0.1 0.1 1.0) $ rectangleSolid windowWidth windowHeight
    
    robotsPic = Pictures (gmap drawRobot (gameRobots gs))
    projectilesPic = Pictures (gmap drawProjectile (gameProjectiles gs))
    explosionsPic = Pictures (gmap drawExplosion (gameExplosions gs))
    ui = drawUI gs

    -- Convierte coordenadas del mundo a píxeles
    toPx :: (Scalar, Scalar) -> (Float, Float)
    toPx (x, y) = (meter2Pixel gs x, meter2Pixel gs y)

    -- Convierte ángulos de radianes a grados para Gloss
    radToDeg :: Angle -> Float
    radToDeg = rad2deg

    -- Renderiza un robot completo (tanque + cañón + barra de salud)
    drawRobot :: Robot -> Picture
    drawRobot r = Pictures [tank, turret, healthBar]
      where
        (x, y) = position r
        (sx, sy) = size r
        robotOrientation = -radToDeg (orientation r) -- Gloss usa rotación en sentido horario expresada en grados y nosotros rotación antihoraria expresada en radianes.
        turretAngle = -radToDeg (turretOrientation (robotTurret r))
        
        -- Tanque (cuerpo del robot)
        tank = case Map.lookup robotImagePath (gameImages gs) of
          Just img -> Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Rotate robotOrientation img
          Nothing  -> Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Rotate robotOrientation $
                      Color (greyN 0.5) $ rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)
        
        -- Cañón (torreta)
        turretLength = meter2Pixel gs sx * 1.5  -- Cañón más largo
        turretWidth = meter2Pixel gs sy * 0.3   -- Ancho más grueso
        turret = Color (makeColor 0.1 0.2 0.1 1.0) $
                 Translate (meter2Pixel gs x) (meter2Pixel gs y) $
                 Rotate turretAngle $
                 Translate (turretLength/4) 0 $
                 rectangleSolid (turretLength/2) (turretWidth/2)
        
        -- Barra de salud
        healthBar = drawHealthBar r

    -- Renderiza la barra de salud de un robot
    drawHealthBar :: Robot -> Picture
    drawHealthBar r = Pictures [background, health]
      where
        (x, y) = position r
        (sx, sy) = size r
        
        -- Posición de la barra (arriba del robot)
        barY = meter2Pixel gs y + meter2Pixel gs sy/2 + 15
        barWidth = meter2Pixel gs sx * 1.2
        barHeight = 6
        
        -- Porcentaje de salud
        healthPercent = robotEnergy r / robotMaxEnergy r
        
        -- Fondo de la barra (rojo)
        background = Color red $
                     Translate (meter2Pixel gs x) barY $
                     rectangleSolid (barWidth/2) (barHeight/2)
        
        -- Barra de salud (verde)
        health = Color green $
                 Translate (meter2Pixel gs x - barWidth/4 + (barWidth/2 * healthPercent)/2) barY $
                 rectangleSolid (barWidth/2 * healthPercent) (barHeight/2)

    -- Renderiza un proyectil (círculo)
    drawProjectile :: Projectile -> Picture
    drawProjectile p = Color orange $ 
      uncurry Translate (toPx (position p)) $ -- uncurry convierte tupla (x,y) en argumentos separados para Translate
      circleSolid (meter2Pixel gs 0.5) -- Radio de 0.5 metros

    drawExplosion :: Explosion -> Picture
    drawExplosion e = Color (withAlpha 0.7 red) $ 
        uncurry Translate (toPx (explosionPosition e)) $ 
        circleSolid (meter2Pixel gs (explosionRadius e))

    -- Renderiza la interfaz de usuario
    drawUI :: GameState -> Picture
    drawUI gs = Pictures [timeDisplay, robotCount, hotkeys]
      where

        -- Posiciones responsive basadas en el tamaño de ventana
        leftMargin = -windowWidth/2 + 20
        topMargin = windowHeight/2 - 20
        
        -- Escala responsive basada en el tamaño de ventana
        textScale = min (windowWidth / baseWindowWidth) (windowHeight / baseWindowHeight) * 0.2 -- min: toma el menor de los dos valores para mantener proporciones
        
        timeDisplay = Color white $ Translate leftMargin (topMargin - 30) $ Scale textScale textScale $ 
                      Text ("Tiempo: " ++ show (round (gameTime gs) :: Int) ++ "s" ++ pauseText)
                      where
                        pauseText
                          | gamePaused gs = " (En Pausa)"
                          | otherwise = ""
        
        robotCount = Color white $ Translate leftMargin (topMargin - 60) $ Scale textScale textScale $ 
                     Text ("Robots vivos: " ++ show (length (gameRobots gs)))
        hotkeys = Color white $ Translate leftMargin (topMargin - 90) $ Scale textScale textScale $
                      Text "Reset: R | Debug: D | Velocidad: 1..0 | Pausa: Espacio"
    
    drawAllVertices :: Picture
    drawAllVertices = Pictures vertsDrawings
      where
        allVertices = concatMap vertices (gameRobots gs) ++ concatMap vertices (gameProjectiles gs)
        vertsDrawings = drawVertices allVertices
          where
            drawVertices :: [Point] -> [Picture]
            drawVertices [] = []
            drawVertices (v:vs) = vertDraw:drawVertices vs
              where vertDraw = Color red $ uncurry Translate (toPx v) $ circleSolid (meter2Pixel gs 0.5)

    drawMenu :: Picture
    drawMenu = Pictures [robotNum, debugMode, simulationSpeed, drawButtons]
        where
            relative :: Scalar2D -> (Pixel, Pixel) -- Transforma coordenadas y tamaños relativos en píxeles.
            relative (x, y) = (windowWidth/2 * x, windowHeight/2 * y)
            -- Escala responsive basada en el tamaño de ventana
            textScale = min (windowWidth / baseWindowWidth) (windowHeight / baseWindowHeight) * 0.2 

            getRowTranslate :: Float -> Float
            getRowTranslate row = -row*0.25

            drawText :: String -> Float -> Picture
            drawText txt row = Color black $ (uncurry Translate) (relative (-0.75, getRowTranslate row)) $ Scale textScale textScale $ Text txt

            robotNum = drawText ("Numero de robots: " ++ show (gameTotalRobotCount gs)) (-1)
            debugMode = drawText ("Velocidad de simulacion: " ++ showFFloat (Just 1) (gameSimulationSpeed gs) "") 0 -- showFFloat sirve para mostrar un número determinado de decimales y opcionalmente un sufijo.
            simulationSpeed = drawText ("Activar modo debug: " ++  if gameDebugInfo gs then "Si" else "No") 1

            drawButtons :: Picture
            drawButtons = Pictures $ map drawButton (gameButtons gs)
            
            drawButton :: UIButton GameState -> Picture
            drawButton button = (uncurry Translate) (relative (buttonPosition button)) $ 
                Pictures [
                            Color (greyN 0.85) $ rectangleSolid rbw rbh,
                            Color black $ rectangleWire rbw rbh,
                            Translate (-rbw/5) (-rbh/5) $ Scale textScale textScale $ Text (buttonText button) -- No está perfectamente ajustado
                        ]
                where
                    (rbw, rbh) = relative (buttonSize button)


            