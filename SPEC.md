# Dana Language Specification v0.1
**Status:** MVP / Experimental
**Paradigm:** Data-Flow Oriented, Graph-Based, Strongly Typed.

---

## 1. Core Philosophy
Dana models programs as directed graphs where nodes represent processing units and edges represent data flow.
- **Graph-First**: Logic is built by connecting nodes with edges (`->`).
- **Reactive**: Execution is driven by events (pulses) injected into the graph.
- **Strongly Typed**: Ports and connections are validated at compile time.

## 2. Type System (v0.1)
Dana uses a strict type system to ensure graph integrity.

### Primitives
* `Int`: 64-bit signed integer.
* `Float`: 64-bit floating point.
* `String`: UTF-8 String.
* `Bool`: Boolean (`true`, `false`).
* `Byte`: 8-bit unsigned integer.
* `Unit` / `Trigger`: Signal type (no payload).

### Complex Types
* `Stream<T>`: (Reserved) Represents a sequence of data.
* `List<T>`: (Planned) Homogeneous collection.

## 3. Syntax Grammar

### 3.1 Node Declaration
A node defines input/output ports and internal process logic.

```dana
node Calculator {
    // 1. Properties (Configuration)
    multiplier: Int = 2

    // 2. Ports (Interface)
    in input: Int
    out result: Int
    
    // 3. Logic Block
    process: (input) => { 
        // Immutable binding (Allowed in v0.1)
        let val = input * multiplier
        
        // Emit to output port
        emit result(val)
    }
}
```

### 3.2 Edges and Flow
Connections between ports determine data flow.

*   **Sync Edge (`->`)**: Immediate execution.
*   **Async Edge (`~>`)**: (Experimental) Deferred execution.

```dana
// Basic Connection
Source.out -> Target.in

// Lambda (Inline) Node
Source.out -> (x => x + 1) -> Target.in
```

### 3.3 Guards (v0.1)
Simple boolean conditions on edges. Data only flows if the condition is true.

```dana
Sensor.val -> [val > 100] -> Alarm.trigger
```

## 4. Execution Model (v0.1)
The runtime executes synchronously in breadth-first order (BFS) for the MVP.
1.  **Injection**: External event triggers a port.
2.  **Process**: Node logic runs, emitting new values.
3.  **Propagation**: Values flow to connected nodes immediately.

---

# Roadmap: Towards v0.2

This section outlines proposed features for the next version.

## 1. Pattern Matching on Edges (Edge Matching)
**Goal**: Replace imperative `if/else` logic with declarative routing.
**Proposal**: Enhance Guards `[...]` to support pattern matching syntax.

### Syntax Proposal
```dana
// 1. Literal Match
Status.code -> ["OK"]    -> UI.Success
Status.code -> ["ERROR"] -> UI.Error

// 2. Structural Match (Destructuring)
Api.response -> [{ type: "User", data: d }] -> UserProcessor.in

// 3. Wildcard (The "Else" case)
// Matches anything not caught by specific patterns
Status.code -> [_] -> Logger.log
```

## 2. Modularity & Structure
**Goal**: Enable scalable architecture through composition and strict entry points.

### The `graph` Keyword
- **Concept**: A `graph` is a composite node defined by its topology.
- **Usage**: Can be instantiated inside other nodes/graphs.
- **Encapsulation**: Defines strict `in`/`out` interfaces, hiding internal complexity.

### The `graph Main` Entrypoint
- **Constraint**: The top-level execution block must be defined as `graph Main`.
- **Role**: Instantiates top-level nodes and defines the initial flow.

## 3. Structural Types (`type`)
**Goal**: Define complex data structures for rich domain modeling.
**Syntax**:
```dana
type User {
    id: Int
    name: String
    active: Bool
}
```

## 4. Execution Logic: TraceID & Recursion
**Goal**: Safe recursion and async context tracking.
- **TraceID**: A unique, deterministic ID generated at the entry of the graph.
- **Propagation**: Flows with every pulse value.
- **Recursion Safety**: The runtime uses TraceID to detect and manage re-entrant loops (stack depth control).

## 5. Standard Library Expansion
- **Collections**: `List<T>`, `Map<K,V>`.
- **Flow Control**: `Split`, `Join` nodes.