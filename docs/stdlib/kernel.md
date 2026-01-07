# System.Kernel Namespace

Fundamental nodes for pulse orchestration and flow control in the EPOS runtime.

## System.Kernel.Collector
Accumulates incoming pulses for a given `TraceID` and emits them as a List when requested.

### Ports
- **Input `in`**: (Any) The value to append to the collection.
- **Input `reset`**: (`Impulse`) When triggered, emits the accumulated list and clears the state for this `TraceID`.
- **Output `out`**: (`List<Any>`) The list of all values received through `in` since the trace began or the last `reset`.

### Behavior
The `Collector` utilizes the `TraceStateStore` to persist values. It is highly efficient for batch processing and windowing operations.

---

## System.Kernel.Join
Synchronizes two parallel execution branches, waiting for data from both before proceeding.

### Ports
- **Input `a`**: (Any) First synchronization value.
- **Input `b`**: (Any) Second synchronization value.
- **Output `out`**: (`List<Any>`) Emits a pair `[a, b]` only after both inputs have been received for the same `TraceID`.

### Behavior
Once a pulse arrives at port `a` or `b`, the `Join` node caches it in the flow context. When the corresponding pulse for the sibling port arrives, both are packed into a list and emitted together. The internal cache for this operation is cleared immediately after emission.
