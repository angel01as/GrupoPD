module Rendering(drawGame, usedImages) where

import Graphics.Gloss hiding (Vector, Point)
import Entities
import GameState
import Robot(Robot(..), Turret(..), isRobotAlive, RobotSpriteProfile(..), spriteProfileFor)
import Geometry
import qualified Data.Map as Map
import UIButton
import WindowSizeState
import Collisions (willCollideNextFrame)
import Data.Char (toUpper)
import Data.List (find)

import Numeric (showFFloat)
import Graphics.Gloss.Juicy (loadJuicy)

-- Rutas a imágenes
backgroundImagePath = "images/background2.jpg"

aggressiveBotBodyPath = "images/aggressiveBot/robot_body.png"
defensiveBotBodyPath = "images/defensiveBot/robot_body.png"
sniperBotBodyPath = "images/sniperBot/robot_body.png"

aggressiveBotTurretPath = "images/aggressiveBot/robot_turret.png"
defensiveBotTurretPath = "images/defensiveBot/robot_turret.png"
sniperBotTurretPath = "images/sniperBot/robot_turret.png"

projectileImagePath = "images/projectile.png"
projectileSpriteWidthPx :: Float
projectileSpriteWidthPx = 887
projectileSpriteHeightPx :: Float
projectileSpriteHeightPx = 236

missileSpritePath = "images/bombas/bomba.png"
missileSpriteWidthPx :: Float
missileSpriteWidthPx = 512
missileSpriteHeightPx :: Float
missileSpriteHeightPx = 512

airplaneSpritePath = "images/avioneta/avioneta.png"
airplaneSpriteWidthPx :: Float
airplaneSpriteWidthPx = 1024
airplaneSpriteHeightPx :: Float
airplaneSpriteHeightPx = 512

menuBackgroundPath = "images/interfaz_inicial/nombre.jpg"

-- Sprites de explosión de proyectil
explosionSprite1Path = "images/explosion/1.png"
explosionSprite2Path = "images/explosion/2.png"
explosionSprite3Path = "images/explosion/3.png"
explosionSprite4Path = "images/explosion/4.png"
explosionSprite5Path = "images/explosion/5.png"
explosionSprite6Path = "images/explosion/6.png"

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
solidObstacleDamaged1Path = "images/obstacles/ruin1_.png"
solidObstacleDamaged2Path = "images/obstacles/ruin1_1.png"
hazardObstaclePath = "images/obstacles/hazard.png"
mineObstaclePath = "images/obstacles/mine.png"
hazardSpriteFPath = "images/hazard_animation/F.png"
hazardSpriteUPath = "images/hazard_animation/U.png"
hazardSpriteEPath = "images/hazard_animation/E.png"
hazardSpriteGPath = "images/hazard_animation/G.png"
hazardSpriteOPath = "images/hazard_animation/O.png"
hazardAnimationPaths = [hazardSpriteFPath, hazardSpriteUPath, hazardSpriteEPath, hazardSpriteGPath, hazardSpriteOPath]
solidObstacleSpriteWidthPx :: Float
solidObstacleSpriteWidthPx = 682
solidObstacleSpriteHeightPx :: Float
solidObstacleSpriteHeightPx = 703


usedImages = [backgroundImagePath, aggressiveBotBodyPath, defensiveBotBodyPath, sniperBotBodyPath,
              aggressiveBotTurretPath, defensiveBotTurretPath, sniperBotTurretPath, projectileImagePath,
              explosionSprite1Path, explosionSprite2Path, explosionSprite3Path, explosionSprite4Path,
              explosionSprite5Path, explosionSprite6Path,
              collisionSprite1Path, collisionSprite2Path, collisionSprite3Path,
              robotDeathSprite1Path, robotDeathSprite2Path, robotDeathSprite3Path, robotDeathSprite4Path,
              solidObstaclePath, solidObstacleDamaged1Path, solidObstacleDamaged2Path,
              hazardObstaclePath, mineObstaclePath,
              hazardSpriteFPath, hazardSpriteUPath, hazardSpriteEPath, hazardSpriteGPath, hazardSpriteOPath,
              missileSpritePath,
              menuBackgroundPath,
              airplaneSpritePath]

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
      let base = Pictures [background, airplanePic, obstaclesPic, robotsPic, missilesPic, projectilesPic, explosionsPic, ui]
          withWinner = case Map.elems (gameRobots gs) of
                          [winner] -> Pictures [base, drawWinnerScreen winner]
                          _        -> base
          withCountdown = if gameTournamentCountdown gs > 0 && gameTournamentActive gs
                            then Pictures [withWinner, drawCountdownOverlay (gameTournamentCountdown gs)]
                            else withWinner
      in withCountdown
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
    missilesPic = Pictures (gmap drawMissile (gameMissiles gs))
    airplanePic = maybe Blank drawAirplane (gameAirplane gs)
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
        profile = spriteProfileFor (robotBehavior r)
        (bodySpriteW, bodySpriteH) = rspBodySpritePixels profile
        (turretSpriteW, turretSpriteH) = rspTurretSpritePixels profile
        turretOffsetRatio = rspTurretForwardOffsetRatio profile
        tankScaleX = meter2Pixel gs sx / bodySpriteW
        tankScaleY = meter2Pixel gs sy / bodySpriteH
        turretScale = rspTurretScale profile
        turretWidthMeters = sx * (turretSpriteW / bodySpriteW) * turretScale
        turretHeightMeters = sy * (turretSpriteH / bodySpriteH) * turretScale
        turretScaleX = meter2Pixel gs turretWidthMeters / turretSpriteW
        turretScaleY = meter2Pixel gs turretHeightMeters / turretSpriteH
        
        -- Tanque (cuerpo del robot) - usando sprite

        tankSprite = case robotBehavior r of
          "aggressive" -> case Map.lookup aggressiveBotBodyPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.2 0.4 0.2 1.0) $ rectangleSolid bodySpriteW bodySpriteH
          "defensive" -> case Map.lookup defensiveBotBodyPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.2 0.4 0.2 1.0) $ rectangleSolid bodySpriteW bodySpriteH
          "sniper" -> case Map.lookup sniperBotBodyPath (gameImages gs) of
                          Just img -> img
                          Nothing -> Color (makeColor 0.2 0.4 0.2 1.0) $ rectangleSolid bodySpriteW bodySpriteH
          _ -> Color (makeColor 0.2 0.4 0.2 1.0) $ rectangleSolid bodySpriteW bodySpriteH

        -- Cañón (torreta) - usando sprite
        turretSprite = case robotBehavior r of
          "aggressive" -> case Map.lookup aggressiveBotTurretPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.1 0.2 0.1 1.0) $ rectangleSolid turretSpriteW turretSpriteH
          "defensive" -> case Map.lookup defensiveBotTurretPath (gameImages gs) of
                              Just img -> img
                              Nothing -> Color (makeColor 0.1 0.2 0.1 1.0) $ rectangleSolid turretSpriteW turretSpriteH
          "sniper" -> case Map.lookup sniperBotTurretPath (gameImages gs) of
                          Just img -> img
                          Nothing -> Color (makeColor 0.1 0.2 0.1 1.0) $ rectangleSolid turretSpriteW turretSpriteH
          _ -> Color (makeColor 0.1 0.2 0.1 1.0) $ rectangleSolid turretSpriteW turretSpriteH
        
        
        tank = Translate (meter2Pixel gs x) (meter2Pixel gs y) $
               Rotate robotOrientation $
               Scale tankScaleX tankScaleY $  -- Escalar al tamaño real del robot
               (if isRobotAlive r then tankSprite else Color (greyN 0.5) tankSprite)

        turret = Translate (meter2Pixel gs x) (meter2Pixel gs y) $
                Rotate turretAngle $
                Translate (meter2Pixel gs sx * turretOffsetRatio) 0 $  -- Offset proporcional hacia adelante
                Scale turretScaleX turretScaleY $
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
          (projW, projH) = size p
          scaleX = meter2Pixel gs projW / projectileSpriteWidthPx
          scaleY = meter2Pixel gs projH / projectileSpriteHeightPx
          projectileSprite = case Map.lookup projectileImagePath (gameImages gs) of
            Just img -> Scale scaleX scaleY img
            Nothing -> Color orange $ rectangleSolid (meter2Pixel gs projW) (meter2Pixel gs projH)
      in Translate (meter2Pixel gs x) (meter2Pixel gs y) $ 
         Rotate angle $
         projectileSprite

    drawMissile :: Missile -> Picture
    drawMissile m =
      let (x, y) = missilePosition m
          missileWidthMeters = 4.0
          missileHeightMeters = 8.0
          scaleX = meter2Pixel gs missileWidthMeters / missileSpriteWidthPx
          scaleY = meter2Pixel gs missileHeightMeters / missileSpriteHeightPx
          baseSprite = case Map.lookup missileSpritePath (gameImages gs) of
            Just img -> Scale scaleX scaleY img
            Nothing -> Color (makeColor 0.9 0.9 0.1 1.0) $ rectangleSolid (meter2Pixel gs missileWidthMeters) (meter2Pixel gs missileHeightMeters)
      in Translate (meter2Pixel gs x) (meter2Pixel gs y) baseSprite

    drawAirplane :: AirplaneState -> Picture
    drawAirplane plane =
      let widthMeters = 32.0
          heightMeters = 12.0
          scaleX = meter2Pixel gs widthMeters / airplaneSpriteWidthPx
          scaleY = meter2Pixel gs heightMeters / airplaneSpriteHeightPx
          sprite = case Map.lookup airplaneSpritePath (gameImages gs) of
            Just img -> Scale scaleX scaleY img
            Nothing -> Color (makeColor 0.6 0.6 0.7 1.0) $ rectangleSolid (meter2Pixel gs widthMeters) (meter2Pixel gs heightMeters)
      in Translate (meter2Pixel gs (airplaneX plane)) (meter2Pixel gs (airplaneY plane)) sprite

    drawExplosion :: Explosion -> Picture
    -- Renderiza un obstáculo con estilo simple por tipo
    drawObstacle :: Obstacle -> Picture
    drawObstacle o =
      let (x,y) = obstaclePosition o
          (sx, sy) = obstacleSize o
          basePic colorPic = Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Color colorPic $ rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)
          t = gameTime gs
          in case obstacleType o of
           Solid ->
             let hits = obstacleHitCount o
                 spritePath
                   | hits >= 3 = solidObstacleDamaged2Path
                   | hits >= 2 = solidObstacleDamaged1Path
                   | otherwise = solidObstaclePath
             in case Map.lookup spritePath (gameImages gs) of 
                  Just img ->
                    let scaleX = meter2Pixel gs sx / solidObstacleSpriteWidthPx
                        scaleY = meter2Pixel gs sy / solidObstacleSpriteHeightPx
                    in Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Scale scaleX scaleY img
                  Nothing -> basePic (greyN 0.6)
           Hazard ->
             let spritePath = case Map.lookup (obstacleID o) (gameHazardAnimations gs) of
                                Just anim | hazardAnimPlaying anim ->
                                  let idx = min (length hazardAnimationPaths - 1) (hazardAnimFrame anim)
                                  in hazardAnimationPaths !! idx
                                _ -> hazardObstaclePath
                 hazardScale = 0.07
             in case Map.lookup spritePath (gameImages gs) of
                      Just img -> Translate (meter2Pixel gs x) (meter2Pixel gs y) $ Scale hazardScale hazardScale img
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

    -- Overlay de cuenta atrás entre torneos
    drawCountdownOverlay :: Scalar -> Picture
    drawCountdownOverlay t =
      let (winW, winH) = (fromIntegral (fst (gameWindowSize gs)), fromIntegral (snd (gameWindowSize gs)))
          overlay = Color (withAlpha 0.65 black) $ rectangleSolid winW winH
          secondsInt = max 1 (ceiling t :: Int) -- aseguramos mostrar 1..3
          txt = show secondsInt
          title = Color white $ Translate (-150) 40 $ Scale 0.35 0.35 $ Text "Siguiente torneo en"
          numberPic = Color yellow $ Translate (-60) (-40) $ Scale 0.8 0.8 $ Text txt
      in Pictures [overlay, title, numberPic]

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
              ProjectileExplosion -> selectProjectileSprite progress
              CollisionExplosion ->
                if progress < 0.33 then collisionSprite1Path
                else if progress < 0.66 then collisionSprite2Path
                else collisionSprite3Path
            
            explosionRadiusPx = meter2Pixel gs (explosionRadius e)
            
            -- Usar sprite si está disponible, sino círculo rojo
            explosionPic = case Map.lookup spritePath (gameImages gs) of
              Just img ->
                let scaleFactor = case explosionType e of
                      ProjectileExplosion -> explosionRadiusPx / 260
                      CollisionExplosion  -> explosionRadiusPx / 180
                in Scale scaleFactor scaleFactor img
              Nothing -> Color (withAlpha 0.7 red) $ circleSolid explosionRadiusPx
            
        in Translate (meter2Pixel gs x) (meter2Pixel gs y) explosionPic

    selectProjectileSprite :: Scalar -> String
    selectProjectileSprite progress
      | progress < (1/6) = explosionSprite1Path
      | progress < (2/6) = explosionSprite2Path
      | progress < (3/6) = explosionSprite3Path
      | progress < (4/6) = explosionSprite4Path
      | progress < (5/6) = explosionSprite5Path
      | otherwise        = explosionSprite6Path

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
    drawMenu = Pictures
      [ menuBackground
      , vignette
      , glassPanel
      , titleBlock
      , subtitleBlock
      , botCards
      , drawButtons
      ]
      where
        relative :: Scalar2D -> (Pixel, Pixel)
        relative (x, y) = (windowWidth/2 * x, windowHeight/2 * y)
        baseScale = min (windowWidth / 1000) (windowHeight / 700)
        headingScale = baseScale * 0.45
        textScale = baseScale * 0.25

        menuBackground = case Map.lookup menuBackgroundPath (gameImages gs) of
          Just img ->
            let scaleX = windowWidth / baseWindowWidth
                scaleY = windowHeight / baseWindowHeight
            in Scale scaleX scaleY img
          Nothing -> Color (makeColor 0.05 0.08 0.12 1) $ rectangleSolid windowWidth windowHeight

        vignette = Color (withAlpha 0.45 black) $ rectangleSolid windowWidth windowHeight

        glassPanel = Pictures
          [ Color (withAlpha 0.35 (makeColor 0.1 0.15 0.25 1)) $ rectangleSolid (windowWidth*0.82) (windowHeight*0.78)
          , Color (withAlpha 0.6 (makeColor 0.8 0.9 1 1)) $ rectangleWire (windowWidth*0.82) (windowHeight*0.78)
          ]

        titleBlock = Translate (-windowWidth*0.34) (windowHeight*0.30) $
          Scale headingScale headingScale $
            Color (makeColor 0.92 0.97 1 1) $
              Text "CENTRO DE OPERACIONES"

        subtitleBlock = Translate (-windowWidth*0.34) (windowHeight*0.22) $
          Scale (headingScale*0.65) (headingScale*0.65) $
            Color (withAlpha 0.8 (makeColor 0.8 0.9 1 1)) $
              Text "Configura el escuadrón antes de desplegar"

        cardWidth = windowWidth * 0.7
        cardHeight = windowHeight * 0.08
        cardSpacing = windowHeight * 0.012
        botCount = max 1 (length (gameBotConfigs gs))
        maxSpacing = 0.50 / fromIntegral (max 3 botCount)
        minSpacing = 0.12
        spacing = max minSpacing maxSpacing
        yTopRel = 0.12
        rowRelPositions = [ (idx, yTopRel - fromIntegral idx * spacing) | idx <- [0 .. botCount - 1] ]
        botCards = Pictures $ zipWith drawCard [0..] (gameBotConfigs gs)

        drawCard idx (rid, beh) =
          let yRel = snd (rowRelPositions !! idx)
              y = snd (relative (0, yRel))
              baseColor = makeColor 0.15 0.22 0.3 0.78
              accentColor = case beh of
                "aggressive" -> makeColor 0.95 0.33 0.33 0.9
                "defensive"  -> makeColor 0.35 0.75 0.95 0.9
                "sniper"     -> makeColor 0.6 0.95 0.55 0.9
                _             -> makeColor 0.8 0.8 0.8 0.9
              behaviorText = case beh of
                "aggressive" -> "Agresivo"
                "defensive"  -> "Defensivo"
                "sniper"     -> "Francotirador"
                _             -> capitalize beh
              cardBase = Translate 0 y $ Pictures
                [ Color baseColor $ rectangleSolid cardWidth cardHeight
                , Color (withAlpha 0.7 accentColor) $ Translate (-cardWidth/2 + 12) 0 $ rectangleSolid 12 (cardHeight - 12)
                , Color (withAlpha 0.35 white) $ rectangleWire (cardWidth) (cardHeight)
                ]
              label = Translate (-cardWidth/2 + 30) (y - cardHeight*0.18) $
                        Scale (textScale*1.1) (textScale*1.1) $
                        Color white $
                        Text ("Tanque " ++ show rid)
              behavior = Translate (-cardWidth/2 + 220) (y - cardHeight*0.18) $
                          Scale (textScale*1.1) (textScale*1.1) $
                          Color (makeColor 0.9 0.95 1 1) $
                          Text behaviorText
          in Pictures [cardBase, label, behavior]

        drawButtons = Pictures $ map drawButton buttonInfo
          where
            buttonInfo = gameButtons gs
            findRowIndex yRel = fmap fst $ find (\(_, ry) -> abs (ry - yRel) < 1e-5) rowRelPositions
            drawButton button =
              let (bxNorm, byNorm) = buttonPosition button
                  (bw, bh) = relative (buttonSize button)
                  rowIdx = if buttonText button `elem` ["<", ">"]
                            then findRowIndex byNorm
                            else Nothing
                  by = case rowIdx of
                         Just idx -> snd (relative (0, snd (rowRelPositions !! idx)))
                         Nothing  -> snd (relative (0, byNorm))
                  bx = fst (relative (bxNorm, 0))
                  buttonColor = makeColor 0.1 0.6 0.85 0.95
                  shadow = Color (withAlpha 0.4 black) $ Translate bx (by - 6) $ rectangleSolid bw bh
                  body = Color buttonColor $ Translate bx by $ rectangleSolid bw bh
                  border = Color (makeColor 0.9 0.97 1 0.9) $ Translate bx by $ rectangleWire bw bh
                  label = Translate (bx - bw*0.18) (by - bh*0.2) $ Scale (textScale*0.9) (textScale*0.9) $ Color white $ Text (buttonText button)
              in Pictures [shadow, body, border, label]

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


            