# Floor 26 - CUDA/GPU Jurisdiction

**Language:** CUDA C++  
**Floor Number:** 26  
**Domain:** Parallel compute kernels, GPU acceleration, Massively parallel workloads

## Domain and Jurisdiction

Floor 26 is the CUDA/GPU jurisdiction, responsible for all GPU-accelerated parallel computing within the Miniature Office. This floor handles:

- **Parallel Compute Kernels**: CUDA kernel development for massively parallel operations
- **GPU Acceleration**: Offloading compute-intensive tasks to GPU
- **Massively Parallel Workloads**: SIMD/SIMT processing, data parallelism
- **Memory Management**: Device memory allocation, host-device transfers, unified memory
- **Performance Optimization**: Occupancy maximization, memory coalescing, warp efficiency

## Architectural Laws

The CUDA/GPU jurisdiction operates under strict architectural laws for efficient GPU computing:

### 1. Memory Coalescing Over Compute Complexity
Optimize memory access patterns for coalesced reads/writes. Memory bandwidth is the bottleneck. Prefer simpler algorithms with better memory access patterns over complex algorithms with poor memory patterns.

### 2. Thread Safety Paramount
Ensure thread-safe operations. Avoid race conditions. Use atomic operations when necessary. Understand warp-level synchronization. Proper use of `__syncthreads()`.

### 3. Minimize Host-Device Transfers
Keep data on device as long as possible. Batch transfers. Use pinned memory for faster transfers. Consider unified memory for appropriate workloads. PCIe bandwidth is expensive.

### 4. Maximize Occupancy
Optimize block size and register usage. Target high occupancy (ratio of active warps to maximum warps). Balance shared memory usage. Use occupancy calculator. More occupancy = better latency hiding.

### 5. Bounds Checking in Kernels
All array accesses must include bounds checks. Thread index calculations must handle edge cases. Validate grid/block dimensions. Prevent out-of-bounds memory access.

## Security Doctrine

Security in CUDA/GPU computing focuses on memory safety and kernel correctness:

### Memory Bounds Checking
- Validate all array indices in kernels (check `threadIdx`, `blockIdx`)
- Ensure grid dimensions cover full data range
- Check allocation success for all `cudaMalloc` calls
- Validate memory copy sizes against buffer sizes
- Use bounds-checked kernels during development

### Race Condition Prevention
- Identify and eliminate race conditions in shared memory access
- Use atomic operations for concurrent updates
- Proper barrier synchronization with `__syncthreads()`
- Avoid write-after-write and read-after-write hazards
- Document thread dependencies

### Kernel Launch Validation
- Check `cudaGetLastError()` after kernel launches
- Validate grid and block dimensions before launch
- Ensure sufficient device memory available
- Check for kernel timeout (TDR on Windows)
- Use `cudaDeviceSynchronize()` and check for errors

### Device Memory Leak Prevention
- Every `cudaMalloc` must have corresponding `cudaFree`
- Use RAII patterns or cleanup blocks
- Check allocation failures and handle gracefully
- Profile memory usage with CUDA tools
- Zero sensitive data before deallocation

## Office Structure

All six offices are staffed and operational:

### 1. Architecture Office
**Role:** Kernel design, memory hierarchy planning, optimization strategy  
**Minimum Staff:** 3 agents  
**Agents:**
- Kernel Architect: Parallel algorithm design and decomposition
- Memory Architect: Memory hierarchy and access pattern optimization
- Performance Architect: Occupancy and throughput optimization

**Capabilities:**
- Parallel algorithm design
- Memory access pattern optimization
- Occupancy analysis
- Performance modeling

### 2. Implementation Office
**Role:** Kernel implementation, host code, memory management  
**Minimum Staff:** 3 agents  
**Agents:**
- Kernel Service Agent: CUDA kernel development
- Memory Data Agent: Memory management and validation
- Operations Agent: Parallel operations implementation

**Capabilities:**
- CUDA kernel coding
- Host-device memory management
- Parallel algorithm implementation
- Error handling

### 3. Review Office
**Role:** Code review, performance analysis, memory pattern validation  
**Minimum Staff:** 2 agents  
**Agents:**
- Code Review Specialist: CUDA best practices enforcement
- Performance Analyst: Kernel performance validation

**Capabilities:**
- Code quality assessment
- Memory pattern review
- Occupancy optimization review
- Race condition detection

### 4. Test Office
**Role:** Kernel testing, correctness validation, performance benchmarking  
**Minimum Staff:** 2 agents  
**Agents:**
- Kernel Test Engineer: Kernel correctness testing
- Performance Tester: Benchmark and profiling

**Capabilities:**
- Kernel correctness testing
- Performance benchmarking
- Memory validation
- Edge case testing

### 5. Security Office
**Role:** Memory safety, bounds checking, race condition detection  
**Minimum Staff:** 2 agents  
**Agents:**
- Memory Safety Officer: Bounds checking and leak detection
- Concurrency Analyst: Race condition prevention

**Capabilities:**
- Memory bounds validation
- Race condition analysis
- Memory leak detection
- Atomic operation verification

### 6. Manager Office
**Role:** GPU resource allocation, kernel scheduling, coordination  
**Minimum Staff:** 1 agent  
**Agent:**
- Floor Manager: GPU resource coordination

**Capabilities:**
- GPU resource management
- Kernel launch coordination
- Stream management
- Task scheduling

## Agent Staffing

**Minimum Total:** 13 agents across all offices  
**Current Deployment:** 3 primary agents (Service, Data, Operations)

### Primary Agents

#### 1. Kernel Service Agent (`cuda-service-1`)
**Role:** Service  
**Capabilities:**
- Kernel analysis and pattern recognition
- Thread organization validation
- Performance profiling
- Code structure analysis

**Responsibilities:**
- Analyze CUDA kernel code
- Count global/shared memory operations
- Identify optimization opportunities
- Validate kernel launch configurations

#### 2. Memory Data Agent (`cuda-data-1`)
**Role:** Data/Model  
**Capabilities:**
- Memory validation and bounds checking
- Coalescing pattern analysis
- Memory hierarchy optimization
- Allocation tracking

**Responsibilities:**
- Validate memory bounds in kernels
- Check memory allocation success
- Ensure proper memory deallocation
- Analyze memory access patterns

#### 3. Operations Agent (`cuda-ops-1`)
**Role:** Operations  
**Capabilities:**
- Parallel operations (reduction, scan, etc.)
- Vector/matrix operations
- Atomic operations
- Warp-level primitives

**Responsibilities:**
- Implement parallel algorithms
- Execute vector/matrix operations
- Perform reduction operations
- Handle synchronization

## API Documentation

### JSON-RPC Interface

The floor communicates via JSON-RPC 2.0 over stdin/stdout.

#### Methods

##### `initialize`
Initialize the floor and return GPU capabilities.

**Request:**
```json
{"jsonrpc":"2.0","method":"initialize","id":1}
```

**Response:**
```json
{
  "jsonrpc":"2.0",
  "id":1,
  "result":{
    "status":"initialized",
    "floor":26,
    "language":"cuda",
    "capabilities":["gpu_compute","parallel_kernels","memory_management"]
  }
}
```

##### `get_floor_info`
Get comprehensive floor information.

**Request:**
```json
{"jsonrpc":"2.0","method":"get_floor_info","id":2}
```

**Response:**
```json
{
  "jsonrpc":"2.0",
  "id":2,
  "result":{
    "floor_number":26,
    "language":"cuda",
    "domain":"Parallel compute kernels, GPU acceleration, Massively parallel workloads",
    "offices":["Architecture Office",...],
    "agents":[...],
    "architectural_laws":[...],
    "security_doctrine":[...]
  }
}
```

##### `analyze_code`
Analyze CUDA kernel code.

**Request:**
```json
{
  "jsonrpc":"2.0",
  "method":"analyze_code",
  "params":{"code":"__global__ void kernel() { }"},
  "id":3
}
```

**Response:**
```json
{
  "jsonrpc":"2.0",
  "id":3,
  "result":{
    "language":"cuda",
    "lines":1,
    "kernels":1,
    "global_memory_ops":0,
    "shared_memory_ops":0,
    "agent":"Kernel Service Agent"
  }
}
```

##### Other Methods
- `add_agent`: Add new agent
- `create_task`: Create task
- `list_agents`: List all agents
- `list_tasks`: List all tasks
- `shutdown`: Shutdown floor

## Building and Running

### Prerequisites

- NVIDIA CUDA Toolkit 10.0 or later
- NVIDIA GPU with compute capability 3.5 or higher
- GCC/G++ compatible with CUDA version
- Linux, Windows, or macOS with CUDA support

### Building

#### Using Make (Linux/macOS):
```bash
make
```

#### Manual compilation:
```bash
nvcc -std=c++11 -O2 -arch=sm_35 -o department_floor department_floor.cu
```

#### For specific GPU architecture:
```bash
# Maxwell (GTX 900 series)
make sm_50

# Pascal (GTX 10xx series)
make sm_60

# Volta/Turing (GTX 16xx, RTX 20xx)
make sm_70

# Ampere (RTX 30xx)
make sm_80
```

### Running

```bash
echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | ./department_floor
```

### Testing

```bash
make test
```

### Checking CUDA Installation

```bash
make info
```

### Compiler Flags

- `-std=c++11`: Use C++11 standard
- `-O2`: Optimization level 2
- `-arch=sm_XX`: Target GPU compute capability
- `-Xcompiler -Wall`: Pass warnings flag to host compiler

For debugging:
```bash
make debug
```

This adds:
- `-g`: Generate debug info
- `-G`: Enable device code debugging
- `-lineinfo`: Include line info for profiling

## Code Examples

### Safe Vector Addition Kernel
```cuda
__global__ void vector_add_kernel(const float *a, const float *b, float *c, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    
    // Bounds check - critical for safety
    if (idx < n) {
        c[idx] = a[idx] + b[idx];
    }
}
```

### Matrix Multiplication with Tiling
```cuda
__global__ void matrix_multiply_kernel(const float *A, const float *B, float *C,
                                       int M, int N, int K) {
    __shared__ float shared_A[16][16];
    __shared__ float shared_B[16][16];
    
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;
    
    float sum = 0.0f;
    
    // Tiled multiplication for memory coalescing
    for (int tile = 0; tile < (K + 15) / 16; tile++) {
        // Load with bounds checking
        if (row < M && (tile * 16 + threadIdx.x) < K) {
            shared_A[threadIdx.y][threadIdx.x] = A[row * K + tile * 16 + threadIdx.x];
        } else {
            shared_A[threadIdx.y][threadIdx.x] = 0.0f;
        }
        
        if ((tile * 16 + threadIdx.y) < K && col < N) {
            shared_B[threadIdx.y][threadIdx.x] = B[(tile * 16 + threadIdx.y) * N + col];
        } else {
            shared_B[threadIdx.y][threadIdx.x] = 0.0f;
        }
        
        __syncthreads();
        
        // Compute partial dot product
        for (int k = 0; k < 16; k++) {
            sum += shared_A[threadIdx.y][k] * shared_B[k][threadIdx.x];
        }
        
        __syncthreads();
    }
    
    // Write result with bounds checking
    if (row < M && col < N) {
        C[row * N + col] = sum;
    }
}
```

### Safe Memory Management
```cuda
cudaError_t safe_vector_add(const float *h_a, const float *h_b, float *h_c, int n) {
    float *d_a = NULL, *d_b = NULL, *d_c = NULL;
    cudaError_t err = cudaSuccess;
    
    // Validate input
    if (h_a == NULL || h_b == NULL || h_c == NULL || n <= 0) {
        return cudaErrorInvalidValue;
    }
    
    // Allocate device memory
    err = cudaMalloc(&d_a, n * sizeof(float));
    if (err != cudaSuccess) goto cleanup;
    
    // ... allocate d_b, d_c ...
    
    // Copy to device
    err = cudaMemcpy(d_a, h_a, n * sizeof(float), cudaMemcpyHostToDevice);
    if (err != cudaSuccess) goto cleanup;
    
    // Launch kernel
    int threads = 256;
    int blocks = (n + threads - 1) / threads;
    vector_add_kernel<<<blocks, threads>>>(d_a, d_b, d_c, n);
    
    // Check for launch errors
    err = cudaGetLastError();
    if (err != cudaSuccess) goto cleanup;
    
    // Wait for completion
    err = cudaDeviceSynchronize();
    if (err != cudaSuccess) goto cleanup;
    
    // Copy result back
    err = cudaMemcpy(h_c, d_c, n * sizeof(float), cudaMemcpyDeviceToHost);
    
cleanup:
    // Always free device memory
    if (d_a) cudaFree(d_a);
    if (d_b) cudaFree(d_b);
    if (d_c) cudaFree(d_c);
    
    return err;
}
```

## Performance Optimization

### Memory Optimization
1. **Coalesced Access**: Align memory access for warp-level coalescing
2. **Shared Memory**: Use shared memory for data reuse within block
3. **Texture Memory**: Consider texture cache for read-only data
4. **Unified Memory**: Use for prototyping, optimize later

### Execution Optimization
1. **Occupancy**: Target 50%+ occupancy using occupancy calculator
2. **Warp Divergence**: Minimize branching within warps
3. **Register Usage**: Balance register usage vs occupancy
4. **Thread Block Size**: Multiples of warp size (32), typically 128-512

### Launch Configuration
```cuda
// Calculate optimal launch configuration
int threads = 256;  // Multiple of 32, typically 128-512
int blocks = (n + threads - 1) / threads;

// Check maximum grid dimension
if (blocks > 65535) {
    // Need 2D or 3D grid
    int blocks_x = (int)ceil(sqrt((double)blocks));
    int blocks_y = (blocks + blocks_x - 1) / blocks_x;
    dim3 grid(blocks_x, blocks_y);
    kernel<<<grid, threads>>>(...);
}
```

## Profiling and Debugging

### NVIDIA Tools
- **nvprof**: Command-line profiler (legacy)
- **Nsight Compute**: Kernel-level profiler
- **Nsight Systems**: System-level profiler
- **cuda-memcheck**: Memory checker (like Valgrind for CUDA)

### Usage Examples
```bash
# Profile application
nvprof ./department_floor

# Check for memory errors
cuda-memcheck ./department_floor

# Detailed kernel analysis
ncu --set full ./department_floor
```

## Integration

This floor integrates with the Miniature Office:

- Receives compute tasks via JSON-RPC
- Offloads parallel work to GPU
- Reports performance metrics
- Maintains GPU resource pools
- Escalates GPU errors appropriately

## Hardware Requirements

**Minimum:**
- NVIDIA GPU with Compute Capability 3.5+
- 2GB GPU memory
- PCIe x8 or better

**Recommended:**
- Modern NVIDIA GPU (Pascal/Volta/Turing/Ampere)
- 8GB+ GPU memory
- PCIe 3.0 x16 or better
- NVLink for multi-GPU

## References

- CUDA C Programming Guide
- CUDA C Best Practices Guide
- NVIDIA GPU Architecture Whitepapers
- Professional CUDA C Programming
