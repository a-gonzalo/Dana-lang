# Manifiesto de Implementación: Dana v0.2~

## I. Filosofía de Diseño: "La Red es el Programa"
Dana no es una lista de instrucciones; es una topología de transformación.

- Homoiconicidad Topológica: Todo elemento del lenguaje (configuración, lógica, sub-pipelines) es un Nodo. No existen las variables, solo los pulsos de datos.

- Determinismo por TraceID: El estado no pertenece a los nodos, sino al flujo. Cada evento raíz genera un TraceID que garantiza el aislamiento total y la trazabilidad.

- Control en la Arista: La lógica de decisión reside en la conexión (Edge), no en el interior del nodo. Las aristas son inteligentes (Guards); los nodos son puros.

## II. El Runtime: Arquitectura EPOS (Ephemeral Process Orchestration System)
El motor de ejecución debe ser un sistema de orquestación de pulsos efímeros, no un sistema de actores vivos.

- Nodos Stateless: Los nodos en Rust son inmutables y de solo lectura.

- Pool de Workers: Un número fijo de hilos procesa una cola global de pulsos.

- Contexto Volátil: El estado necesario para nodos como Join o Collector se almacena en un mapa concurrente indexado por TraceID y se destruye inmediatamente al finalizar el flujo.

## III. Estructura del Lenguaje

- node (Atom): Definición de puertos y bloque process. Se prohíbe la palabra clave let y cualquier estado local.

- graph (Molecule): Definición de topología. Un grafo es una "caja negra" con puertos in y out.

- graph Main: Único punto de entrada. Inicia la propagación de los primeros pulsos.

- Nodos Estáticos (Configuración): Nodos sin entradas y con valores por defecto en sus salidas (convención MAYÚSCULAS). Son las fuentes de verdad del sistema.

## IV. Conectividad e Interacción

- Pulsos: * -> Sincrónico (inmediato).

- ~> Asincrónico (planificado).

- Guards Explícitos: Las aristas filtran datos mediante expresiones booleanas "[condicion]". Si no hay coincidencia, el flujo muere. No existe el else implícito.

- Homoiconicidad de Configuración: Los parámetros de un nodo entran por puertos in. La configuración es simplemente un pulso que llega desde un nodo estático o un literal.

## V. Axiomas de la StdLib (Kernel de Rust)
El motor de Rust solo necesita implementar cuatro tipos de nodos nativos para que el resto del lenguaje sea posible:

- Slicer: Para deconstruir List y String (Iteración).

- Collector: Para persistir estado por TraceID (Acumulación).

- Join: Para sincronizar múltiples ramas paralelas.

- NativeIO: Nodos raíz para interactuar con el mundo real (HTTP, FS, Process).

## VI. Reglas de Oro para el Desarrollador

- Si necesitas una variable, crea un puerto.

- Si necesitas un bucle, crea una conexión recursiva.

- Si necesitas una constante, crea un nodo estático.

Si el código es difícil de visualizar, la topología está mal diseñada.