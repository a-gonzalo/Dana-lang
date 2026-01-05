# Dana Language (v0.1.0 MVP)

Dana is a **Graph-Oriented Programming Language** designed for high-performance, predictable data-flow applications. Programs in Dana are defined as directed graphs where nodes represent processing units and edges represent data flow.

## Key Features

- **Graph-First Architecture**: Logic is built by connecting nodes with edges (`->`).
- **Hybrid Execution Runtime**: 
  - **Dynamic Nodes**: Define custom logic using high-level Dana syntax (`process` blocks).
  - **Native Nodes (FFI)**: Drop-in high-performance Rust components for system-level operations (e.g., `System.IO`).
- **Strong Typing**: Ports and properties are strictly typed (`Int`, `String`, `Float`, etc.) to ensure graph integrity.
- **Explicit Data Flow**: No hidden state mutations; data propagates explicitly through ports.

## Current Status (v0.1.0)

The project is currently in **MVP (Minimum Viable Product)** stage.

### Implemented Features
- [x] **Compiler Frontend**: Fully functional PEG parser using `pest`.
- [x] **Graph Builder**: Validates topology, port existence, and type compatibility.
- [x] **Runtime Engine**: Synchronous event-driven scheduler.
- [x] **Native FFI**: Interface for integrating Rust code as graph nodes (`stdlib`).
- [x] **Standard Library**: 
  - `System.IO.stdout`: Native interface to console output.
- [x] **CLI Tool**: `dana` command for running and checking programs.

### Limitations (Future Roadmap)
- Asynchronous edges (`~>`) are parsed but currently execute synchronously.
- `Stream<T>` type is defined but not yet distinct from standard types in behavior.
- Advanced pattern matching and error handling are planned for v0.2.

## 📦 Installation & Usage

### Prerequisites
- Rust (latest stable)
- Cargo

### Building
```bash
cargo build --release
```

### Development with WSL
If you are developing on Windows but using the Rust toolchain in WSL:
1. **VS Code Tasks**: Use the provided tasks (`Ctrl+Shift+B` for build) which run `cargo` inside WSL via `dana.ps1`.
2. **PowerShell Script**: You can also use `.\dana.ps1 build`, `.\dana.ps1 test`, or `.\dana.ps1 run "..."`.
3. **Performance**: The build uses a target directory inside WSL (`~/.cargo-target/dana-lang`) to avoid the slow performance of building on `/mnt/c`.

### Running Examples

Two example programs are provided in the `examples/` directory.

**1. Hello World** (`examples/hello.dana`)
sends a string from a user node to the system console.
```bash
cargo run -- run examples/hello.dana --trigger HelloWorld.trigger
```

**2. Math Calculation** (`examples/math.dana`)
Demonstrates arithmetic operations and variable scope.
```bash
cargo run -- run examples/math.dana --trigger Calculator.val --value 10
```

**3. String Operations** (`examples/strings.dana`)
Demonstrates properties and string concatenation.
```bash
cargo run -- run examples/strings.dana --trigger Greeter.name --value Alice
```

## 📄 Language Quick Start

A Dana program consists of **Nodes** and **Edges**.

```dana
// 1. Define a Node
node Greeter {
    // Properties (State/Configuration)
    prefix: String = "Hello, "

    // Ports (Interface)
    in name: String
    out message: String

    // Logic (Process)
    process: (name) => {
        emit message(prefix + name)
    }
}

// 2. Connect the Graph
// Connect Greeter to System Output
Greeter.message -> System.IO.stdout
```

## 🤝 Contributing

This is an experimental language. Contributions to the standard library or runtime engine are welcome!
