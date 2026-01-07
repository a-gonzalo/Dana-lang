# Arquitectura de Runtime: EPOS (Ephemeral Pulse Orchestration System)
## 1. El Concepto Fundamental
La arquitectura EPOS abandona el modelo de "entidades vivas" (como los Actores) en favor de un modelo de "Flujo sobre Infraestructura Estática". En EPOS, los nodos no son procesos, sino estaciones de paso inmutables. La energía del sistema reside en los Pulsos, que son unidades mínimas de trabajo que recorren el grafo.

## 2. Los Componentes del Sistema
### 2.1 El Grafo Estático (The Map)
El programa parseado reside en memoria como una estructura de solo lectura (Arc<Graph>). Contiene la topología y la lógica de los nodos, pero no contiene datos de ejecución. Es un "mapa" que todos los hilos consultan pero ninguno modifica.

### 2.2 Los Trabajadores (The Workers)
Un pool de hilos de ejecución fijos (basados en la capacidad de la CPU). Su única función es extraer un Pulso de la cola global, ejecutar la lógica del nodo destino y colocar los pulsos resultantes de nuevo en la cola.

### 2.3 El Pulso (The Pulse)
Es la unidad atómica de ejecución. Un pulso es un paquete que contiene:

Payload: El dato real (Token).

TraceID: La identidad del flujo original.

Address: El nodo y puerto de destino hacia donde se dirige.

### 2.4 El Almacén de Contexto (The Trace-State Store)
Un registro de memoria volátil y fragmentado. En lugar de que cada nodo guarde su estado, existe un almacén donde el estado se guarda bajo la clave combinada (TraceID + NodeID).

## 3. Dinámica de Ejecución
### 3.1 El Ciclo de Vida de un Pulso
Cuando un Worker procesa un pulso:

Identifica el nodo destino en el Mapa Estático.

Si el nodo requiere estado (como un Collector o un Join), lo solicita al Almacén de Contexto usando su TraceID.

Ejecuta la función process del nodo.

Al finalizar, si el nodo emite datos, el Worker crea nuevos pulsos para cada arista conectada y los envía a la cola global.

El Worker queda libre inmediatamente para el siguiente pulso, sin importar de qué flujo o nodo sea.

### 3.2 Paralelismo Implícito
El paralelismo ocurre de forma natural cuando un nodo emite múltiples pulsos hacia distintas aristas. Estos pulsos entran en la cola global y son capturados por diferentes Workers casi instantáneamente, ejecutando ramas enteras del grafo en paralelo sin que el programador gestione hilos.

### 3.3 Gestión de la "Muerte" del Flujo
Cada TraceID mantiene un contador atómico de pulsos activos. Cuando un pulso se procesa y no genera nuevos pulsos (hoja del grafo o filtrado por Guard), el contador decrece. Cuando llega a cero, el sistema dispara una rutina de limpieza que elimina todas las entradas del Almacén de Contexto asociadas a ese TraceID.

## 4. Ventajas sobre el Modelo de Actores
Bajo Consumo de Memoria: No hay miles de pilas (stacks) de hilos ni buzones (mailboxes) esperando. Solo hay memoria ocupada por los datos que realmente se están moviendo.

Cero Bloqueos: Al ser los nodos estáticos y el estado estar segmentado por TraceID, no hay contención de bloqueos (locks) entre diferentes ejecuciones.

Localidad de Caché: Los Workers pueden optimizarse para procesar pulsos del mismo grafo rápidamente, manteniendo el código de los nodos caliente en la caché de la CPU.

## 5. Resumen para la Implementación
Para construir EPOS en Rust, necesitamos:

Un Scheduler basado en una cola de mensajes asíncrona (como un canal MPMC).

Un ContextManager que utilice una estructura de datos concurrente (como un Hashmap particionado) para los estados de los TraceIDs.

Un Evaluador de Aristas que se ejecute en el Worker para decidir, según los Guards, si un pulso debe generarse o no.