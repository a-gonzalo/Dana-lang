# Dana Language Specification v1.0
**Paradigm:** Data-Flow Oriented, Graph-Based, Strongly Typed.
**Primary Structure:** Directed Acyclic/Cyclic Graph (DCG) of Nodes.
**Implementation Target:** Rust (Tokio for Runtime, Nom/Pest for Parsing).

---

## 1. Core Philosophy
Dana elimina la distinción entre código y datos. No existe la ejecución secuencial imperativa; el programa es una red de nodos que reaccionan a estímulos.
- **No hay variables locales:** Solo hay estados dentro de nodos.
- **No hay puntero de instrucción:** La ejecución es una reacción en cadena (Propagación de Pulsos).
- **Todo es un Grafo:** Desde la lógica de negocio hasta la interfaz de usuario.

---

## 2. Type System (Structural & Static)
Dana utiliza un sistema de tipos estricto para asegurar la integridad del flujo en tiempo de compilación.

* **Atomics:** `Int`, `Float`, `String`, `Bool`, `Byte`.
* **Shapes (Complex):** Definidos por la estructura interna de un nodo.
* **Streams:** Representados como `Stream<T>`, una secuencia de datos fluyendo por una arista.
* **Type Safety:** Una arista `NodeA.out -> NodeB.in` es rechazada si los tipos de los puertos no coinciden.

---

## 3. Syntax Grammar (EBNF-like)

### 3.1 Node Declaration
Un nodo es un contenedor de estado (properties), entradas (in) y salidas (out).

```dana
node [Name] {
    // State properties
    [Property]: [Type] = [DefaultValue]

    // Communication Ports
    in [PortName]: [Type]
    out [PortName]: [Type]
    
    // Logic Block
    process: (input) { 
        // Logic uses 'emit' to send data to output ports
        emit [PortName]([Value])
    }
}
```


### 3.2 Edges and Flow (The Connectivity)

    Sync Edge (->): El nodo destino se procesa inmediatamente.

    Async Edge (~>): El mensaje se encola; el runtime lo procesa de forma asíncrona.

    Guards ([condition]): Actúan como filtros lógicos en la arista.

```dana
// Example of guarded flow
Source.out -> [val > 0] -> Target.in
```

### 3.3 Lambda Nodes (Inline Processing)

Para transformaciones rápidas sin declarar un nodo completo:

```dana
Source.out -> (x => x * 2) -> Target.in
```

## 4. Runtime Architecture: The Pulse Engine

El runtime de Dana, implementado en Rust, funciona mediante un planificador de tareas basado en grafos.
### 4.1 The Pulse Scheduler

    Injection: Un evento externo (I/O, MouseClick, Timer) inyecta un dato en un puerto.

    Activation: El nodo se marca como Hot.

    Execution: El Scheduler (Pool de hilos de Tokio) ejecuta el bloque process.

    Propagation: El resultado viaja por las aristas, activando los siguientes nodos.

### 4.2 Memory Management: Connectivity Ownership

Dana no utiliza Garbage Collector tradicional.

    Graph-Based RC: Un nodo vive mientras sea alcanzable desde el nodo Root o un evento activo.

    Isolation: Cada nodo es dueño de sus datos internos; el paso de mensajes entre nodos implica movimiento (move) o clonación profunda de datos para evitar condiciones de carrera.

5. Standard Library (System Nodes)

Nodos reservados para interactuar con el mundo exterior:

    System.IO.stdout: Puerto de salida estándar.

    System.IO.stdin: Emite pulsos de tipo String basados en la entrada del usuario.

    System.Net.Http: Gestiona peticiones de red como nodos de proceso.

    System.Time.Tick(ms): Un generador de pulsos temporal.

## 6. Comprehensive Examples
### 6.1 Hello World
```dana
node Main {
    content: String = "Hello Dana!"
}

Main.content -> System.IO.stdout
```

### 6.2 Reactive Counter (UI Model)
```dana
node Counter {
    current: Int = 0
    in increment: Trigger
    
    process: (increment) => {
        emit current(current + 1)
    }
}

// UI Interaction
UI.Button.click -> Counter.increment
Counter.current -> (val => "Count: " + val) -> UI.Label.text
```

### 6.3 HL7 Parser (Data Processing)
```dana
node HL7Parser {
    in raw: String
    out patientId: String

    process: (raw) => {
        raw -> String.split("\n") 
            -> [line | line.startsWith("PID")]
            -> (pidLine => pidLine.split("|")[3])
            -> emit patientId
    }
}
```

## 7. Implementation Strategy (Rust)

    Parser: Utilizar nom para procesar la sintaxis de Dana y generar un GraphAST.

    Graph Representation: Utilizar la estructura de datos Petgraph para gestionar nodos y aristas.

    Node Wrapping: Cada nodo en el runtime debe estar envuelto en un Arc<Mutex<Node>> o usar un modelo de actores.

    Concurrency: Utilizar canales tokio::sync::mpsc para representar las aristas (edges) y permitir que los pulsos fluyan entre hilos de forma segura.