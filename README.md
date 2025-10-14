# GrupoPD

# HAUS 5

Punto de Entrada del Programa
Crea el punto de entrada principal del programa que se encargará de inicializar los robots y la región de juego. Esta sección debe lanzar el ciclo principal del juego para iniciar el torneo.



Lógica del Torneo (Bucle Principal del Juego)
Desarrolla una función que implemente la lógica del torneo y el bucle principal del juego. En cada iteración, esta función deberá:

Llamar a las funciones de lógica de cada bot para determinar sus acciones.
Ejecutar las actualizaciones correspondientes en los estados del juego basándose en las acciones de los bots.
Comprobar las posibles colisiones (entre proyectiles, tanques, etc.).
Renderizar el estado resultante en el área de juego.


Lógica de Renderizado
Desarrolla una función que dibuje en el área de juego todos los objetos basándose en el estado actual del juego.

El Tanque y el Cañón se representarán como rectángulos.
El Proyectil podría ser un círculo (su diseño se deja a vuestra imaginación).
Las Explosiones se dejan a vuestra imaginación para el diseño.
El Nivel de Vida (o barra de salud) también se dibujará usando rectángulos.


El objetivo de esta tarea es obtener una primera versión del juego funcional donde se podrán crear nuevos bots con diferentes estrategias.