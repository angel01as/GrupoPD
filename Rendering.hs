module Rendering(drawGame, usedImages) where

import Graphics.Gloss hiding (Vector, Point)
import Entities
import GameState
import Robot(Robot(..), Turret(..), isRobotAlive)
import Geometry
import qualified Data.Map as Map
import UIButton
import WindowSizeState
import Collisions (willCollideNextFrame)
import Data.Char (toUpper)

import Numeric (showFFloat)
import Graphics.Gloss.Juicy (loadJuicy)

-- Rutas a imágenes
backgroundImagePath = "images/background.jpg"

aggressiveBotBodyPath = "images/aggressiveBot/robot_body.png"
defensiveBotBodyPath = "images/defensiveBot/robot_body.png"
sniperBotBodyPath = "images/sniperBot/robot_body.png"

aggressiveBotTurretPath = "images/aggressiveBot/robot_turret.png"
defensiveBotTurretPath = "images/defensiveBot/robot_turret.png"
sniperBotTurretPath = "images/sniperBot/robot_turret.png"

projectileImagePath = "images/projectile.png"

-- Sprites de explosión de proyectil
explosionSprite1Path = "images/explosion/a.png"
explosionSprite2Path = "images/explosion/aa.png"
explosionSprite3Path = "images/explosion/aaa.png"
explosionSprite4Path = "images/explosion/aaaa.png"

-- Sprites de colisión tanque-tanque
collisionSprite1Path = "images/collisionEffect/collision_effect1.png"
collisionSprite2Path = "images/collisionEffect/collision_effect2.png"
collisionSprite3Path = "images/collisionEffect/collision_effect3.png"

-- Sprites de muerte del robot
robotDeathSprite1Path = "images/deathEffect/death_effect1.png"
robotDeathSprite2Path = "images/deathEffect/death_effect2.png"
robotDeathSprite3Path = "images/deathEffect/death_effect3.png"
robotDeathSprite4Path = "images/deathEffect/death_effect4.png"

solidObstaclePath = "images/obstacles/ruin1.png"
hazardObstaclePath = "images/obstacles/hazard.png"
mineObstaclePath = "images/obstacles/mine.png"


usedImages = [backgroundImagePath, aggressiveBotBodyPath, defensiveBotBodyPath, sniperBotBodyPath,
              aggressiveBotTurretPath, defensiveBotTurretPath, sniperBotTurretPath, projectileImagePath,
              explosionSprite1Path, explosionSprite2Path, explosionSprite3Path, explosionSprite4Path,
              collisionSprite1Path, collisionSprite2Path, collisionSprite3Path,
              robotDeathSprite1Path, robotDeathSprite2Path, robotDeathSprite3Path, robotDeathSprite4Path,
              solidObstaclePath, hazardObstaclePath, mineObstaclePath]

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
    regularGameInfo =
      let base = Pictures [background, obstaclesPic, robotsPic, projectilesPic, explosionsPic, ui]
      in case Map.elems (gameRobots gs) of
           [winner] -> Pictures [base, drawWinnerScreen winner]
           _        -> base
    debugGameInfo = Pictures [createBorder, drawAllVertices, drawObstacleDebug, drawPredictedCollisions gs]
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
    obstaclesPic = Pictures (gmap drawObstacle (gameObstacles gs))
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
        
        -- Tanque (cuerpo del robot) - usando sprite

        tankSprite = case robotBehavior r of
          "aggressive" -> case Map.lookup aggressiveBotBodyPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.2 0.4 0.2 1.0) $ rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)
          "defensive" -> case Map.lookup defensiveBotBodyPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.2 0.4 0.2 1.0) $ rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)
          "sniper" -> case Map.lookup sniperBotBodyPath (gameImages gs) of
                          Just img -> img
                          Nothing -> Color (makeColor 0.2 0.4 0.2 1.0) $ rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)

        -- Cañón (torreta) - usando sprite
        turretSprite = case robotBehavior r of
          "aggressive" -> case Map.lookup aggressiveBotTurretPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.1 0.2 0.1 1.0) $ rectangleSolid (meter2Pixel gs sx * 1.5 / 2) (meter2Pixel gs sy * 0.3 / 2)
          "defensive" -> case Map.lookup defensiveBotTurretPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.1 0.2 0.1 1.0) $ rectangleSolid (meter2Pixel gs sx * 1.5 / 2) (meter2Pixel gs sy * 0.3 / 2)
          "sniper" -> case Map.lookup sniperBotTurretPath (gameImages gs) of
                          Just img -> img
                          Nothing -> Color (makeColor 0.1 0.2 0.1 1.0) $ rectangleSolid (meter2Pixel gs sx * 1.5 / 2) (meter2Pixel gs sy * 0.3 / 2)
        
        
        tank = Translate (meter2Pixel gs x) (meter2Pixel gs y) $
               Rotate robotOrientation $
               Scale (meter2Pixel gs sx / 100) (meter2Pixel gs sy / 100) $  -- Escalar para que coincida con el tamaño del robot
               (if isRobotAlive r then tankSprite else Color (greyN 0.5) tankSprite)

        turret = Translate (meter2Pixel gs x) (meter2Pixel gs y) $
                Rotate turretAngle $
                Translate (meter2Pixel gs sx * 0.25) 0 $  -- Offset hacia adelante
                Scale (meter2Pixel gs sx / 80) (meter2Pixel gs sy / 120) $
                turretSprite
        
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

    -- Renderiza un proyectil
    drawProjectile :: Projectile -> Picture
    drawProjectile p = 
      let (x, y) = position p
          angle = -radToDeg (projectileOrientation p)
          projectileSprite = case Map.lookup projectileImagePath (gameImages gs) of
            Just img -> Scale 0.5 0.5 img
            Nothing -> Color orange $ circleSolid (meter2Pixel gs 0.5)
      in Translate (meter2Pixel gs x) (meter2Pixel gs y) $ 
         Rotate angle $
         projectileSprite

    drawExplosion :: Explosion -> Picture
    -- Renderiza un obstáculo con estilo simple por tipo
    drawObstacle :: Obstacle -> Picture
    drawObstacle o =
      let (x,y) = obstaclePosition o
          (sx, sy) = obstacleSize o
          basePic colorPic = Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Color colorPic $ rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)
          t = gameTime gs
      in case obstacleType o of
           Solid -> case Map.lookup solidObstaclePath (gameImages gs) of 
                      Just img -> Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Scale 0.11 0.11 img
                      Nothing -> basePic (greyN 0.6)
           Hazard -> case Map.lookup hazardObstaclePath (gameImages gs) of
                      Just img -> Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Scale 0.07 0.07 img
                      Nothing -> let pulse = 0.5 + 0.5 * sin (t*4)
                                 in basePic (makeColor 1 0 0 (0.6 + 0.2*pulse))
           Bomb ->
             let blink = 0.5 + 0.5 * sin (t*8)
                 timerTxt = case obstacleTimer o of
                              Just tm -> Translate (-10) (-10) $ Scale 0.1 0.1 $ Color white $ Text (show (ceiling tm :: Int))
                              Nothing -> Blank
                 texture = case Map.lookup mineObstaclePath (gameImages gs) of
                            Just img -> Scale 0.05 0.05 img
                            Nothing -> Color (makeColor 1 1 0 (0.6 + 0.3*blink)) $ rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)
                 box = Translate (meter2Pixel gs x) (meter2Pixel gs y) $
                       Pictures [ texture, timerTxt]
             in box
           Special -> basePic (makeColor 0.2 0.6 1.0 0.5)

    -- Pantalla de ganador superpuesta
    drawWinnerScreen :: Robot -> Picture
    drawWinnerScreen r =
      let (winW, winH) = (fromIntegral (fst (gameWindowSize gs)), fromIntegral (snd (gameWindowSize gs)))
          overlay = Color (withAlpha 0.4 black) $ rectangleSolid winW winH
          title = Color white $ Translate (-200) 50 $ Scale 0.4 0.4 $ Text "\127942 ¡GANADOR!"
          subtitle = Color yellow $ Translate (-220) (-20) $ Scale 0.2 0.2 $ Text ("Tanque " ++ show (robotID r) ++ " - " ++ robotBehavior r)
      in Pictures [overlay, title, subtitle]
    drawExplosion e = 
        let (x, y) = explosionPosition e
            progress = explosionTime e / explosionMaxTime e
            
            -- Seleccionar sprite según el tipo y progreso de la animación
            spritePath = case explosionType e of
              ProjectileExplosion -> 
                if progress < 0.25 then explosionSprite1Path
                else if progress < 0.5 then explosionSprite2Path
                else if progress < 0.75 then explosionSprite3Path
                else explosionSprite4Path
              CollisionExplosion ->
                if progress < 0.33 then collisionSprite1Path
                else if progress < 0.66 then collisionSprite2Path
                else collisionSprite3Path
            
            explosionRadiusPx = meter2Pixel gs (explosionRadius e)
            
            -- Usar sprite si está disponible, sino círculo rojo
            explosionPic = case Map.lookup spritePath (gameImages gs) of
              Just img -> Scale (explosionRadiusPx / 50) (explosionRadiusPx / 50) img
              Nothing -> Color (withAlpha 0.7 red) $ circleSolid explosionRadiusPx
            
        in Translate (meter2Pixel gs x) (meter2Pixel gs y) explosionPic

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
        allVertices = concatMap vertices (gameRobots gs) ++ concatMap vertices (gameProjectiles gs) ++ concatMap vertices (gameObstacles gs)
        vertsDrawings = drawVertices allVertices
          where
            drawVertices :: [Point] -> [Picture]
            drawVertices [] = []
            drawVertices (v:vs) = vertDraw:drawVertices vs
              where vertDraw = Color red $ uncurry Translate (toPx v) $ circleSolid (meter2Pixel gs 0.5)

    drawObstacleDebug :: Picture
    drawObstacleDebug = Pictures (concatMap labelAndWire (Map.elems (gameObstacles gs)))
      where
        labelAndWire o =
          let (ox, oy) = obstaclePosition o
              (sx, sy) = obstacleSize o
              label = Translate (meter2Pixel gs ox) (meter2Pixel gs (oy + 2)) $ Scale 0.1 0.1 $ Color white $ Text ("OBST: " ++ show (obstacleID o) ++ " (" ++ show (obstacleType o) ++ ")")
              wire = Color green $ Translate (meter2Pixel gs ox) (meter2Pixel gs oy) $ rectangleWire (meter2Pixel gs sx) (meter2Pixel gs sy)
          in [label, wire]

    drawMenu :: Picture
    drawMenu = Pictures [panel, title, rowsText, hints, drawButtons]
      where
        relative :: Scalar2D -> (Pixel, Pixel)
        relative (x, y) = (windowWidth/2 * x, windowHeight/2 * y)
        textScale = min (windowWidth / 1000) (windowHeight / 700) * 0.25

        panel = Pictures [ Color (withAlpha 0.6 (greyN 0.2)) $ uncurry Translate (relative (0,0)) $ rectangleSolid (windowWidth*0.75) (windowHeight*0.75)
                         , Color (withAlpha 0.9 white) $ uncurry Translate (relative (0,0)) $ rectangleWire (windowWidth*0.75) (windowHeight*0.75)
                         ]
        title = Color white $ (uncurry Translate) (relative (-0.25, 0.32)) $ Scale (textScale*1.1) (textScale*1.1) $ Text "CONFIGURACION DE BATALLA"

        drawButtons :: Picture
        drawButtons = Pictures $ map drawButton (gameButtons gs)
          where
            drawButton :: UIButton GameState -> Picture
            drawButton button = (uncurry Translate) (relative (buttonPosition button)) $ 
              Pictures [ Color (greyN 0.85) $ rectangleSolid rbw rbh
                      , Color black $ rectangleWire rbw rbh
                      , Translate (-rbw*0.12) (-rbh*0.15) $ Scale textScale textScale $ Text (buttonText button)
                      ]
              where
                (rbw, rbh) = relative (buttonSize button)

        -- Pista inferior - movida más abajo y a la izquierda para que salga del panel
        hints = (uncurry Translate) (relative (-0.38, -0.40)) $ Scale (textScale*0.65) (textScale*0.65) $ Color (greyN 0.75) $ Text "Usa < y > para cambiar tipo. Click JUGAR para iniciar."

        -- Texto de filas: "Tanque i: <tipo>" - mejor espaciado y posiciones
        rowsText = Pictures $ zipWith drawRow [0..] (gameBotConfigs gs)
          where
            drawRow :: Int -> (Int, String) -> Picture
            drawRow idx (rid, beh) =
              let n = max 1 (length (gameBotConfigs gs))
                  -- Aumentar espaciado: usar más espacio vertical y mínimo entre filas
                  maxSpacing = 0.50 / fromIntegral (max 3 n)  -- Espaciado máximo entre filas
                  minSpacing = 0.12  -- Espaciado mínimo para evitar solapamiento
                  spacing = max minSpacing maxSpacing
                  yTop = 0.12  -- Empezar más abajo para dar más espacio
                  y = yTop - fromIntegral idx * spacing
                  labelTxt = "Tanque " ++ show rid ++ ":"
                  behTxt = case beh of
                            "aggressive" -> "Agresivo"
                            "defensive"  -> "Defensivo"
                            "sniper"     -> "Francotirador"
                            _             -> capitalize beh
                  -- Calcular offset vertical para centrar texto con los botones
                  -- Los botones tienen altura 0.10 (relativo), el texto se renderiza desde su línea base
                  -- Necesitamos subir el texto aproximadamente la mitad de su altura de línea para centrarlo
                  buttonHeightPx = windowHeight/2 * 0.10  -- Altura del botón en píxeles
                  textLineHeight = textScale * 0.85 * 25  -- Altura estimada de una línea de texto
                  textOffsetY = textLineHeight * 0.4  -- Subir texto para alinearlo con centro del botón
                  -- Layout: "Tanque X: Tipo" juntos a la izquierda, botones completamente a la derecha
                  leftCol = Translate (fst (relative (-0.40, y))) (snd (relative (-0.40, y)) + textOffsetY) $ 
                            Scale (textScale*0.85) (textScale*0.85) $ Color white $ Text labelTxt
                  centerCol = Translate (fst (relative (-0.10, y))) (snd (relative (-0.10, y)) + textOffsetY) $ 
                              Scale (textScale*0.9) (textScale*0.9) $ Color white $ Text behTxt
              in Pictures [leftCol, centerCol]

            capitalize [] = []
            capitalize (c:cs) = toEnum (fromEnum (toUpper c)) : cs

    

    -- Líneas azules semitransparentes entre robots que colisionarán y el obstáculo implicado
    drawPredictedCollisions :: GameState -> Picture
    drawPredictedCollisions s =
      let pairs = [ (r, o)
                  | r <- Map.elems (gameRobots s)
                  , o <- Map.elems (gameObstacles s)
                  , willCollideNextFrame r o 0.3
                  ]
          toPx (x,y) = (meter2Pixel s x, meter2Pixel s y)
          linesPics = [ Color (makeColor 0 0 1 0.3) $ Line [toPx (position r), toPx (obstaclePosition o)]
                      | (r,o) <- pairs]
      in Pictures linesPics


            