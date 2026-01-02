# Dana Language Developer Guide for AI Agents

**Version:** 0.1.0 (MVP)
**Paradigm:** Graph-Oriented, Data-Flow, Synchronous Execution

## 1. Core Syntax

Dana programs define **Nodes** (logic) and **Edges** (connections).

### Node Definition
Nodes are the building blocks. They must be declared with properties *first*, then ports, then the process block.

```dana
node ComponentName {
    // 1. Properties (Configuration/State)
    // MUST appear before ports.
    // Currently read-only configuration in v0.1.0.
    prefix: String = "Item: "
    threshold: Int = 10

    // 2. Input Ports (Data In)
    in data: Int
    in trigger: Unit

    // 3. Output Ports (Data Out)
    out result: String
    out alert: Bool

    // 4. Process Logic
    // Triggered when data arrives on specific ports (e.g. 'data')
    process: (data) => {
        // Local variables
        let double_val = data * 2
        
        // Expressions: +, -, *, /, &&, ||, ==, !=, >, <
        // String concatenation supported with +
        
        // Emit data to output ports
        emit result(prefix + double_val)
        
        // Control flow is currently linear (no if/else in MVP, distinct process blocks per trigger planned)
    }
}
```

### Edge Definition
Connects an Output Port to an Input Port.
`SourceNode.output_port -> TargetNode.input_port`

### Comments
- Line: `// comment`
- Block: `/* comment */`

## 2. Type System
Strongly typed.
- `Int`: 64-bit signed integer.
- `Float`: 64-bit float.
- `String`: UTF-8 String (double quoted).
- `Bool`: `true` or `false`.
- `Unit`: Signal type. Used for triggers.
- `Byte`: 8-bit unsigned (less common).

## 3. Standard Library (v0.1.0)
Native nodes available globally.

### System.IO
- **Node**: `System.IO`
- **Input Port**: `stdout` (Accepts `String`). Prints value to console.
- **Usage**: `MyNode.msg -> System.IO.stdout`

## 4. Execution Model
1.  **Event Injection**: CLI triggers a node port (e.g., `--trigger Node.port --value val`).
2.  **Propagation**: The node process runs. `emit` pushes data to output ports.
3.  **Synchronous Flow**: Data travels instantly across `->` edges to connected inputs.
4.  **Chain Reaction**: Connected nodes execute immediately (Breadth-First propagation).

## 5. Limitations (v0.1.0)
- **Properties**: Read-only during execution. State is not persisted between triggers yet.
- **Control Flow**: No `if/else` inside process blocks (use separate nodes or guards in future).
- **Edges**: Async edges (`~>`) are parsed but execute synchronously.
