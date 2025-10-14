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

#### **4. AI.hs - Ejecución Mejorada**
- ✅ **Ejecución con tiempo**: Considera `deltaTime` entre frames
- ✅ **AIExecutionResult**: Resultado estructurado con robots, proyectiles y explosiones
- ✅ **updateRobotAI**: Función principal que actualiza robots con su comportamiento
- ✅ **executeAICommands**: Ejecuta comandos considerando el tiempo
- ✅ **getBehaviorByName**: Sistema de comportamientos por nombre para evitar dependencias circulares

### **Funcionalidades Principales Implementadas:**

#### **✅ Punto de Entrada del Programa (Main.hs)**
- Inicialización de robots con diferentes comportamientos
- Configuración del estado inicial del juego
- Lanzamiento del torneo

#### **✅ Lógica del Torneo (Bucle Principal)**
1. **Actualización de AI**: Cada robot ejecuta su comportamiento
2. **Ejecución de comandos**: Procesamiento con tiempo entre frames
3. **Detección de colisiones**: Sistema completo robot-robot y proyectil-robot
4. **Renderizado**: Visualización del estado en tiempo real

#### **✅ Lógica de Renderizado**
- **Robots**: Posición, orientación, velocidad, barra de salud
- **Proyectiles**: Posición, velocidad, daño
- **Explosiones**: Radio, progreso temporal, estado
- **Barras de salud**: Representación visual con porcentajes

## **Cómo Ejecutar el Juego**

### **Método 1: Compilación Manual**
```bash
ghc -package containers Main.hs -o HAUS5.exe
.\HAUS5.exe
```

### **Método 2: Script de Compilación (Windows)**
```bash
compile.bat
```

## **Estructura del Proyecto**

```
├── Main.hs              # Punto de entrada y bucle principal
├── AI.hs                # Sistema de comportamientos AI y DSL
├── Robot.hs             # Definición de robots y torretas
├── Entities.hs          # Proyectiles, explosiones y entidades del juego
├── Geometry.hs          # Funciones geométricas y matemáticas
├── Collisions.hs        # Sistema de detección de colisiones
├── compile.bat          # Script de compilación para Windows
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

✅ **Punto de entrada principal** - Implementado en `Main.hs`  
✅ **Bucle principal del juego** - Sistema completo de actualización  
✅ **Lógica de renderizado** - Visualización detallada de todos los objetos  
✅ **Sistema de colisiones** - Detección y procesamiento de colisiones  
✅ **Comportamientos AI** - Sistema extensible de comportamientos  
✅ **Sistema de explosiones** - Efectos visuales y de daño  
✅ **Sistema de torretas** - Cooldown, daño y alcance  
✅ **Ejecución con tiempo** - Consideración de deltaTime entre frames  
✅ **Capa extra entre BotBehavior y BotCommand** - Sistema de nombres para evitar dependencias circulares  

## **Notas Técnicas**

- **Dependencias circulares resueltas**: Se usa un sistema de nombres de comportamientos en lugar de referencias directas
- **Compilación exitosa**: El proyecto compila sin errores con GHC 8.6.5
- **Sistema extensible**: Fácil añadir nuevos comportamientos y comandos
- **Manejo de tiempo**: Consideración precisa del tiempo entre frames

El proyecto está **completamente funcional** y listo para crear nuevos bots con diferentes estrategias.