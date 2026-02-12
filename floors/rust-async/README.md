# Floor 28 - Rust-Async Jurisdiction

**Language:** Rust-Async (Async Rust with Tokio)  
**Floor Number:** 28  
**Domain:** Asynchronous Rust, async/await patterns, concurrent task execution

## Domain and Jurisdiction

Floor 28 governs the realm of asynchronous Rust - modern concurrent programming using async/await patterns with the Tokio runtime. This jurisdiction specializes in:

- **Asynchronous Programming**: Non-blocking I/O and concurrent task execution
- **Async/Await Patterns**: Modern, ergonomic asynchronous code
- **Concurrent Task Management**: Spawn, join, and manage async tasks
- **Stream Processing**: Asynchronous data streams and iterators
- **Synchronization Primitives**: Mutex, RwLock, channels for concurrent access

## Architectural Laws

The Rust-Async jurisdiction operates under strict architectural principles:

1. **Async Safety Enforced**: All async operations are cancellation-safe
2. **Cancellation Safety Guaranteed**: Cleanup code always executes on task cancellation
3. **Deadlock Detection Active**: Lock ordering and deadlock prevention enforced
4. **Send+Sync Bounds Verified**: All shared data properly bound for thread safety
5. **Future Composition**: Proper future chaining and error propagation

## Security Doctrine

Security in the Rust-Async jurisdiction follows rigorous safety principles:

- **Panic Safety Maintained**: Panics don't corrupt shared state
- **Resource Cleanup Guaranteed**: Drop implementations and RAII ensure cleanup
- **Unwind Safety Enforced**: Catch_unwind for critical sections
- **Concurrent Access Controlled**: RwLock and Mutex prevent data races
- **Timeout Protection**: All external operations have timeout guards

## Office Structure

### Architecture Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Async system design and future composition
- Stream and iterator architecture
- Concurrency patterns and executor selection
- Backpressure and flow control design

**Key Agents:**
- Async Architect - Overall async system design
- Concurrency Architect - Concurrency patterns and lock-free design
- Stream Architect - Stream processing and backpressure handling

### Implementation Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Async function implementation with async/await
- Tokio runtime configuration and tuning
- Channel and synchronization primitive usage
- Error propagation and Result handling

**Key Agents:**
- Async Engineer - Async function implementation
- Runtime Engineer - Tokio runtime configuration and tuning
- Channel Engineer - Inter-task communication primitives

### Review Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Cancellation safety review and validation
- Deadlock detection and prevention
- Send+Sync trait bound verification
- Async code best practices enforcement

**Key Agents:**
- Safety Reviewer - Cancellation and panic safety
- Deadlock Detector - Lock ordering and deadlock analysis
- Bounds Auditor - Send+Sync and lifetime verification

### Test Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Async test execution with #[tokio::test]
- Concurrency testing and race detection
- Loom-based model checking
- Performance benchmarking

**Key Agents:**
- Async Tester - Async test implementation
- Concurrency Tester - Race condition detection with Loom
- Performance Tester - Async benchmarks and profiling

### Security Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Panic safety analysis and unwind safety
- Resource leak detection
- Cancellation handling verification
- Async injection prevention

**Key Agents:**
- Panic Guardian - Panic and unwind safety
- Resource Guardian - Resource leak detection
- Cancellation Guardian - Proper cancellation handling

### Manager Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Runtime monitoring and task scheduling
- Resource allocation and backpressure management
- Performance optimization and latency tracking
- Capacity planning

**Key Agents:**
- Runtime Manager - Runtime monitoring and task scheduling
- Resource Manager - Resource allocation and backpressure
- Performance Manager - Optimization and latency tracking

## Specialized Agent Modules

### Service Agent
**Role:** Async I/O and concurrent request processing  
**Capabilities:**
- Concurrent request handling with semaphore limits
- Async I/O multiplexing
- Task spawning and lifecycle management
- Backpressure and capacity management

**State Tracking:**
- Active task count and concurrency limits
- Request utilization percentage
- Connection lifecycle management

### Data Agent
**Role:** Concurrent data access with synchronization  
**Capabilities:**
- RwLock-based concurrent data access
- Async caching and state management
- Stream processing for data flows
- Read/write statistics tracking

**State Tracking:**
- Cache size and memory usage
- Read and write operation counts
- Read/write ratio analysis

### Operations Agent
**Role:** Async task execution and monitoring  
**Capabilities:**
- Async task execution with timeout protection
- Future composition and error handling
- Stream transformation and filtering
- Performance metrics collection

**State Tracking:**
- Tasks completed and failed counts
- Average task duration
- Success rate percentage
- Timeout detection

## API Documentation

The floor communicates via JSON-RPC over stdin/stdout with full async processing.

### get_info
Returns comprehensive floor information with runtime statistics.

```json
{"method": "get_info"}
```

Response includes:
- Floor metadata (number, language, domain)
- All 6 offices with responsibilities
- Specialized agent runtime statistics
- Architectural laws and security doctrine
- Active task and agent counts

### add_agent
Asynchronously add a new agent.

```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "async-001",
    "name": "Async Worker",
    "role": "Worker",
    "capabilities": ["Async processing", "Stream handling"],
    "office": "Implementation"
  }
}
```

### create_task
Create an async task with automatic tracking.

```json
{
  "method": "create_task",
  "params": {
    "task_id": "task-001",
    "title": "Process async stream",
    "assigned_to": "impl-001",
    "office": "Implementation"
  }
}
```

### process_code
Process async Rust code with various operations.

**Operations:**
- `analyze` - Analyze async code patterns
- `check_safety` - Check cancellation and panic safety
- `optimize` - Get optimization suggestions
- `test` - Analyze test coverage

```json
{
  "method": "process_code",
  "params": {
    "code": "async fn process() { tokio::spawn(async {}).await }",
    "operation": "analyze"
  }
}
```

**analyze** returns:
- Line and async function counts
- Await point analysis
- Spawn point detection
- Mutex, RwLock, channel usage
- Stream processing patterns
- Select block usage
- Potential deadlock warnings
- Cancellation safety assessment

**check_safety** returns:
- Cancellation safety status
- Deadlock risk assessment
- Panic safety evaluation
- Sync primitive usage

**optimize** returns:
- Optimization suggestions
- Async score (0-100)
- Specific improvements
- Best practice recommendations

**test** returns:
- Test count and framework detection
- Async test presence
- Testing recommendations

## Building and Running

### Prerequisites

- Rust 1.70+ with cargo
- Tokio runtime (included in dependencies)

### Building

```bash
cd floors/rust-async
cargo build --release
```

### Running

Execute the floor:
```bash
./target/release/department_floor
```

Or run directly:
```bash
cargo run --release
```

### Interactive Testing

```bash
echo '{"method": "get_info"}' | cargo run --release
```

Multiple commands:
```bash
cargo run --release << EOF
{"method": "get_info"}
{"method": "process_code", "params": {"code": "async fn test() { tokio::spawn(async {}).await }", "operation": "analyze"}}
EOF
```

## Testing

Run all tests (async tests execute in Tokio runtime):
```bash
cargo test
```

Run with output:
```bash
cargo test -- --nocapture
```

Run specific test:
```bash
cargo test test_async_analysis
```

Run concurrency tests:
```bash
cargo test test_concurrent_access
```

## Linting and Code Quality

```bash
# Format check
cargo fmt --check

# Clippy with all warnings
cargo clippy -- -D warnings

# Check async-specific lints
cargo clippy -- -W clippy::await_holding_lock -W clippy::async_yields_async
```

## Performance Optimization

### Runtime Configuration

The Tokio runtime is configured with:
- Multi-threaded work-stealing scheduler
- Full feature set (I/O, timers, sync, etc.)
- Tracing support for debugging

### Concurrency Control

- Request semaphore limits concurrent processing
- Service agent tracks active task count
- Backpressure prevents resource exhaustion

### Async Best Practices

1. **Avoid Blocking**: Never block the async executor
   ```rust
   // Bad
   std::thread::sleep(duration);
   
   // Good
   tokio::time::sleep(duration).await;
   ```

2. **Minimize Lock Hold Time**: Release locks before await points
   ```rust
   // Bad
   let guard = mutex.lock().await;
   expensive_async_work().await;
   
   // Good
   let data = {
       let guard = mutex.lock().await;
       guard.clone()
   };
   expensive_async_work().await;
   ```

3. **Use Timeouts**: Protect against hanging operations
   ```rust
   tokio::time::timeout(Duration::from_secs(30), operation).await
   ```

4. **Cancellation Safety**: Ensure cleanup on cancellation
   ```rust
   tokio::select! {
       result = operation() => result,
       _ = cancellation_token.cancelled() => {
           // Cleanup code
       }
   }
   ```

## Async Patterns

### Future Composition

```rust
async fn example() {
    let (a, b, c) = tokio::join!(
        fetch_a(),
        fetch_b(),
        fetch_c()
    );
}
```

### Stream Processing

```rust
use futures::stream::{self, StreamExt};

async fn process_stream() {
    stream::iter(vec![1, 2, 3])
        .map(|x| x * 2)
        .filter(|x| x > &2)
        .collect::<Vec<_>>()
        .await;
}
```

### Select Pattern

```rust
tokio::select! {
    result = operation1() => handle_op1(result),
    result = operation2() => handle_op2(result),
    _ = timeout => handle_timeout(),
}
```

### Spawning Tasks

```rust
let handle = tokio::spawn(async move {
    // Work
});
handle.await.unwrap();
```

## Synchronization Primitives

### Mutex (Exclusive Access)
```rust
let mutex = Arc::new(Mutex::new(data));
let guard = mutex.lock().await;
```

### RwLock (Read-Write Access)
```rust
let rwlock = Arc::new(RwLock::new(data));
let read_guard = rwlock.read().await;
let write_guard = rwlock.write().await;
```

### Channels
```rust
let (tx, mut rx) = tokio::sync::mpsc::channel(100);
tokio::spawn(async move {
    while let Some(msg) = rx.recv().await {
        process(msg);
    }
});
```

### Semaphore
```rust
let sem = Arc::new(Semaphore::new(10));
let permit = sem.acquire().await.unwrap();
```

## Deadlock Prevention

1. **Lock Ordering**: Always acquire locks in the same order
2. **Minimize Lock Scope**: Hold locks for minimal time
3. **Avoid Nested Locks**: Use RwLock when possible
4. **Timeout Guards**: Use try_lock or timeouts

## Cancellation Safety

An operation is cancellation-safe if dropping its future mid-execution is safe:

**Cancellation-Safe:**
- tokio::time::sleep
- tokio::net::TcpStream::connect
- Most tokio I/O operations

**NOT Cancellation-Safe:**
- tokio::sync::mpsc::Receiver::recv (use recv_many)
- tokio::io::AsyncReadExt::read_exact (may lose data)

**Making Operations Cancellation-Safe:**
```rust
let mut buffer = Vec::new();
tokio::select! {
    result = read_operation(&mut buffer) => {
        // Process result
    }
    _ = cancel_token.cancelled() => {
        // buffer is in consistent state
    }
}
```

## Common Issues and Solutions

**Issue: Deadlock**
- Use lock ordering
- Profile with `--cfg tokio_unstable` and console-subscriber
- Review with deadlock detector agent

**Issue: High Latency**
- Check for blocking operations
- Use timeouts
- Profile with tokio-console

**Issue: Resource Leaks**
- Ensure Drop implementations
- Use RAII patterns
- Test cancellation scenarios

**Issue: Panic in Async**
- Use catch_unwind for critical sections
- Implement proper error propagation
- Test panic scenarios

## Monitoring and Debugging

### Tokio Console
```bash
cargo install tokio-console
RUSTFLAGS="--cfg tokio_unstable" cargo run --features tokio/tracing
tokio-console
```

### Tracing
```rust
use tracing::{info, instrument};

#[instrument]
async fn traced_function() {
    info!("Function called");
}
```

## Dependencies

- **tokio**: Async runtime with work-stealing scheduler
- **futures**: Future and stream utilities
- **async-trait**: Async trait support
- **tokio-util**: Additional Tokio utilities
- **serde/serde_json**: JSON serialization

## Contributing

When extending Floor 28:
- Use #[tokio::test] for async tests
- Ensure cancellation safety
- Document lock ordering
- Add timeout protection
- Test concurrent scenarios
- Update performance metrics

## Advanced Topics

### Custom Runtime
```rust
let rt = tokio::runtime::Builder::new_multi_thread()
    .worker_threads(4)
    .build()
    .unwrap();
```

### Async Traits
```rust
#[async_trait]
trait AsyncProcessor {
    async fn process(&self, data: Data) -> Result<(), Error>;
}
```

### Pin and Unpin
Understanding pinning for custom futures and async streams.

## License

See repository LICENSE file.
