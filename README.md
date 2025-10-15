# GrupoPD - HAUS 5: Robot Battle Arena

## ✅ **IMPLEMENTACIÓN COMPLETADA**

### **Mejoras Implementadas según las Especificaciones del Compañero:**

#### **1. Robot.hs - Sistema de Robots Mejorado**
- ✅ **robotBehavior**: Añadido como String para evitar dependencias circulares
- ✅ **robotMaxEnergy**: Sistema de energía máxima para calcular porcentajes
- ✅ **robotLastUpdateTime**: Control de tiempo para actualizaciones precisas
- ✅ **robotCurrentInstruction**: Seguimiento de instrucciones en ejecución

#### **2. Sistema de Torreta Completo**
- ✅ **Cooldown**: Sistema de recarga entre disparos (`turretCooldown`, `turretMaxCooldown`)
- ✅ **Daño configurable**: Cada torreta tiene su propio daño (`turretDamage`)
- ✅ **Alcance**: Sistema de rango de disparo (`turretRange`)
- ✅ **Funciones auxiliares**: `canShoot`, `shootProjectile`, `afterShooting`, `updateTurretCooldown`

#### **3. Sistema de Explosiones**
- ✅ **Radio dinámico**: Las explosiones crecen con el tiempo
- ✅ **Daño por área**: Sistema de daño basado en distancia
- ✅ **Efectos temporales**: Fase de daño (30%) y fase de humo (70%)
- ✅ **Funciones**: `createExplosion`, `updateExplosion`, `isExplosionActive`, `isExplosionDamaging`
- ✅ **GameEntity**: Implementación completa de la interfaz GameEntity

#### **4. AI.hs - Ejecución Mejorada**
- ✅ **Ejecución con tiempo**: Considera `deltaTime` entre frames
- ✅ **AIExecutionResult**: Resultado estructurado con robots, proyectiles y explosiones
- ✅ **updateRobotAI**: Función principal que actualiza robots con su comportamiento
- ✅ **executeAICommands**: Ejecuta comandos considerando el tiempo
- ✅ **getBehaviorByName**: Sistema de comportamientos por nombre para evitar dependencias circulares

## **Estructura del Proyecto**

```
├── AI.hs                # Sistema de comportamientos AI y DSL
├── Game.hs              # Bucle principal (draw/update) y GameState del juego
├── Main.hs              # Punto de entrada: crea estado inicial y lanza playGame
├── Robot.hs             # Definición de robots y torretas
├── Entities.hs          # Proyectiles, explosiones y entidades del juego
├── Geometry.hs          # Funciones geométricas y matemáticas
├── Collisions.hs        # Sistema de detección de colisiones
└── README.md            # Este archivo
```

## **Comportamientos AI Implementados**

### Bot Agresivo (`"aggressive"`)
- Busca enemigos activamente
- Ataca cuando encuentra objetivos
- Se mueve hacia el enemigo

### Bot Defensivo (`"defensive"`)
- Huye cuando tiene poca energía
- Ataca solo con energía suficiente
- Patrulla cuando no hay amenazas

## **Sistema de Comandos AI**
- **Movimiento**: Adelante, atrás, rotación, multiplicación de velocidad
- **Disparo**: Sistema de cooldown y creación de proyectiles
- **Memoria**: Almacenamiento de información entre frames
- **Condiciones**: Evaluación de estado del juego

## **Sistema de Colisiones**
- **SAT Algorithm**: Detección precisa de colisiones entre polígonos
- **Robot-Projectile**: Daño por impacto de proyectiles
- **Robot-Robot**: Daño por colisión directa
- **Explosiones**: Daño por área de efecto

## **Objetivos Cumplidos**

✅ **robotBehavior** - Implementado con capa extra de nombres  
✅ **robotMaxEnergy** - Añadido al tipo Robot  
✅ **Torreta completa** - Cooldown, daño, alcance implementados  
✅ **Sistema de explosiones** - Radio dinámico, daño temporal, efecto humo  
✅ **Capa extra** - Sistema de nombres para evitar dependencias circulares  
✅ **Ejecución con tiempo** - Consideración de deltaTime entre frames  
✅ **AIExecutionResult** - Resultado estructurado de ejecución AI  

## **Punto de Entrada y Bucle de Juego (implementado por Angel)**

- `Main.hs`:
  - Crea el estado inicial del torneo (ventana 1000x700, dos robots básicos: "aggressive" y "defensive").
  - Llama a `playGame` para iniciar el bucle principal.

- `Game.hs`:
  - `GameState`: estado mínimo necesario para Gloss (tamaño de ventana, robots, proyectiles, tiempo).
  - `drawGame`: dibuja cada robot como `rectangleSolid` (en píxeles, partiendo de tamaños en metros) y los proyectiles de forma simple.
  - `updateGame`: por frame, construye el estado de IA, actualiza cada robot con `AI.updateRobotAI`, genera proyectiles y avanza posiciones según velocidades y `deltaTime`.

### Cómo ejecutarlo

- GHCi:
  ```bash
  ghci -package gloss
  :load Main.hs
  main
  ```
- GHC:
  ```bash
  ghc -package gloss Main.hs
  ./Main   # (Windows: Main.exe)
  ```

## **Cómo Usar los Módulos**

### **Cargar en GHCi (Recomendado)**
```bash
ghci -package containers
:load AI.hs
:load Robot.hs
:load Entities.hs
```

### **Compilar los Módulos**
```bash
ghc -package containers -c AI.hs
ghc -package containers -c Robot.hs
ghc -package containers -c Entities.hs
```

## **Notas Técnicas**

- **Dependencias circulares resueltas**: Se usa un sistema de nombres de comportamientos en lugar de referencias directas
- **Compilación exitosa**: Los módulos compilan sin errores con GHC 8.6.5
- **Sistema extensible**: Fácil añadir nuevos comportamientos y comandos
- **Manejo de tiempo**: Consideración precisa del tiempo entre frames
- **GameEntity**: Las explosiones implementan correctamente la interfaz GameEntity

Los módulos están **completamente funcionales** y listos para ser integrados en un sistema de juego más amplio.

---

## **🎮 COMMIT: Dibujo Sistema de Juego Visual **  (Fran)

### **Nuevas Funcionalidades Implementadas:**

#### **1. Sistema de Renderizado Visual (Game.hs)**
- ✅ **Renderizado de robots**: Tanques con torretas, barras de vida y orientación
- ✅ **Sistema de proyectiles**: Círculos naranjas con física de movimiento
- ✅ **Interfaz de usuario**: Contador de tiempo, robots vivos y estadísticas
- ✅ **Escalado dinámico**: Adaptación automática a cualquier resolución de ventana

#### **2. Sistema de Imágenes de Fondo (gloss.juicy)**
- ✅ **Carga de imágenes**: Soporte para PNG, JPG, BMP, GIF
- ✅ **Escalado automático**: Las imágenes se adaptan al tamaño de ventana
- ✅ **Fallback robusto**: Fondo sólido si la imagen no se carga
- ✅ **Responsive**: Redimensionado en tiempo real

#### **3. Sistema de Escalado Dinámico**
- ✅ **Factor de escalado**: Basado en tamaño de ventana vs escenario base
- ✅ **Escenario base**: 100x70 metros (1000x700 píxeles)
- ✅ **Escalado proporcional**: Mantiene proporciones en todas las resoluciones
- ✅ **UI responsive**: Texto y elementos se adaptan al tamaño de ventana


### **Dependencias Añadidas:**
- **gloss-juicy**: Para carga y renderizado de imágenes
- **Graphics.Gloss**: Para el sistema de renderizado visual

### **Características Técnicas:**
- **Resolución base**: 1000x700 píxeles
- **Escenario**: 100x70 metros (escalable)
- **Tanques**: 8x8 metros (escalables)
- **Proyectiles**: 0.5 metros de radio (escalables)
- **FPS**: 60 frames por segundo
- **Escalado**: Automático y responsive
