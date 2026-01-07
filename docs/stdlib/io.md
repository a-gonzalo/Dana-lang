# System.IO Namespace

Root nodes for interacting with the external environment.

## System.IO.stdout
Prints the string representation of any data type to the standard output.

### Ports
- **Input `stdout`**: (`Any`) The data to be printed. Supports all primitive and complex types.

### Behavior
The output is printed immediately. This node does not emit any further pulses. It is thread-safe and can handle concurrent pulses from multiple workers.
