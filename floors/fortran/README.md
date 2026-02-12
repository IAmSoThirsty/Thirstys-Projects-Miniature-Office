# Floor 24 - Fortran Jurisdiction

**Language:** Fortran (2008+)  
**Floor Number:** 24  
**Domain:** Scientific computation, Numerical analysis, HPC simulations, Array processing

## Domain and Jurisdiction

Floor 24 is the Fortran jurisdiction, responsible for all scientific computing and numerical analysis within the Miniature Office. This floor handles:

- **Scientific Computation**: Numerical methods, differential equations, linear algebra
- **Numerical Analysis**: Precision analysis, stability validation, error propagation
- **HPC Simulations**: High-performance computing, parallel array operations
- **Array Processing**: Multi-dimensional arrays, matrix operations, column-major ordering
- **Legacy Code Integration**: Interfacing with existing Fortran scientific libraries

## Architectural Laws

The Fortran jurisdiction operates under strict architectural laws that reflect the language's scientific computing heritage:

### 1. Numerical Precision Over Convenience
All numeric operations must preserve maximum precision. Use explicit KIND parameters for floating-point types. Document precision requirements. No implicit type conversions that lose precision.

### 2. Mandatory Array Bounds Checking
All array accesses must be bounds-checked. Use compiler flags for runtime bounds validation. Explicit array dimensions required. Off-by-one errors are considered critical failures.

### 3. Explicit Array Dimensions
Array dimensions must be explicitly declared or passed as parameters. No assumed-size arrays in new code. Use assumed-shape arrays with explicit interfaces for modern code.

### 4. Column-Major Ordering
Arrays are stored in column-major order (Fortran standard). Optimize loop ordering for cache efficiency. Inner loops iterate over first dimension.

### 5. Modern Fortran Standards
Use Fortran 2008 or later standards. Prefer modules over COMMON blocks. Use explicit interfaces. Employ intent declarations for all arguments.

## Security Doctrine

Security in Fortran scientific computing focuses on numerical stability and data integrity:

### Array Bounds Validation
- Enable runtime bounds checking during development (`-fcheck=bounds`)
- Validate all array indices before access
- Use assumed-shape arrays with explicit interfaces
- Document array dimension requirements

### Numerical Stability
- Validate for NaN and Infinity in results
- Check condition numbers for matrix operations
- Detect overflow and underflow conditions
- Use appropriate precision for problem domain

### Memory Safety
- Initialize all variables before use
- Avoid implicit SAVE attribute issues
- Properly allocate/deallocate dynamic arrays
- Check allocation status

### Input Validation
- Validate array dimensions before operations
- Check for divide-by-zero conditions
- Validate mathematical operation preconditions
- Bounds-check file I/O operations

## Office Structure

All six offices are staffed and operational:

### 1. Architecture Office
**Role:** Algorithm design, numerical method selection, data structure planning  
**Minimum Staff:** 3 agents  
**Agents:**
- Algorithm Architect: Numerical method selection and optimization
- Array Structure Designer: Multi-dimensional array layout planning
- Precision Analyst: Floating-point precision requirements analysis

**Capabilities:**
- Numerical algorithm design
- Array structure optimization
- Precision requirement specification
- Performance profiling

### 2. Implementation Office
**Role:** Code implementation, subroutine development, module creation  
**Minimum Staff:** 3 agents  
**Agents:**
- Numerical Service Agent: Core computational routines
- Array Data Agent: Array validation and management
- Operations Agent: Matrix operations and linear algebra

**Capabilities:**
- Subroutine and function implementation
- Module development
- Array operation implementation
- I/O routine development

### 3. Review Office
**Role:** Code review, numerical validation, precision analysis  
**Minimum Staff:** 2 agents  
**Agents:**
- Code Review Specialist: Fortran idiom compliance
- Numerical Validator: Result accuracy verification

**Capabilities:**
- Code quality assessment
- Numerical accuracy validation
- Performance review
- Standards compliance checking

### 4. Test Office
**Role:** Unit testing, numerical validation, regression testing  
**Minimum Staff:** 2 agents  
**Agents:**
- Unit Test Engineer: Individual subroutine testing
- Integration Tester: Full program validation

**Capabilities:**
- Test case development
- Numerical result validation
- Regression testing
- Performance benchmarking

### 5. Security Office
**Role:** Bounds checking, numerical stability, overflow detection  
**Minimum Staff:** 2 agents  
**Agents:**
- Bounds Checker: Array bounds validation
- Stability Analyzer: Numerical stability assessment

**Capabilities:**
- Array bounds analysis
- Numerical stability checking
- Overflow/underflow detection
- Precision loss detection

### 6. Manager Office
**Role:** Project coordination, resource allocation, deadline management  
**Minimum Staff:** 1 agent  
**Agent:**
- Floor Manager: Overall floor coordination

**Capabilities:**
- Task assignment and tracking
- Agent coordination
- Deadline management
- Escalation handling

## Agent Staffing

**Minimum Total:** 13 agents across all offices  
**Current Deployment:** 3 primary agents (Service, Data, Operations)

### Primary Agents

#### 1. Numerical Service Agent (`fortran-service-1`)
**Role:** Service  
**Capabilities:**
- Precision control and analysis
- Rounding mode management
- Code pattern analysis
- Numerical method implementation

**Responsibilities:**
- Analyze Fortran code structure
- Count subroutines, functions, and DO loops
- Identify array usage patterns
- Validate numerical precision requirements

#### 2. Array Data Agent (`fortran-data-1`)
**Role:** Data/Model  
**Capabilities:**
- Array bounds validation
- Dimension checking
- Memory layout optimization
- Data structure management

**Responsibilities:**
- Validate array bounds before access
- Ensure proper array dimensioning
- Detect out-of-bounds access attempts
- Optimize array memory layout

#### 3. Operations Agent (`fortran-ops-1`)
**Role:** Operations  
**Capabilities:**
- Matrix operations
- Linear algebra routines
- Numerical methods
- Vector operations

**Responsibilities:**
- Implement matrix multiplication with bounds checking
- Perform vector operations safely
- Execute numerical algorithms
- Validate operation preconditions

## API Documentation

### JSON-RPC Interface

The floor communicates via JSON-RPC 2.0 over stdin/stdout.

#### Methods

##### `initialize`
Initialize the floor and return capabilities.

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
    "floor":24,
    "language":"fortran",
    "capabilities":["scientific_compute","numerical_analysis","array_operations"]
  }
}
```

##### `get_floor_info`
Get comprehensive floor information including agents and offices.

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
    "floor_number":24,
    "language":"fortran",
    "domain":"Scientific computation, Numerical analysis, HPC simulations",
    "offices":["Architecture Office","Implementation Office","Review Office","Test Office","Security Office","Manager Office"],
    "agents":[...],
    "architectural_laws":[...],
    "security_doctrine":[...]
  }
}
```

##### `analyze_code`
Analyze Fortran code and return metrics.

**Request:**
```json
{
  "jsonrpc":"2.0",
  "method":"analyze_code",
  "params":{"code":"program test\n  real :: x\nend program"},
  "id":3
}
```

**Response:**
```json
{
  "jsonrpc":"2.0",
  "id":3,
  "result":{
    "language":"fortran",
    "lines":3,
    "subroutines":0,
    "arrays":0,
    "do_loops":0,
    "agent":"Numerical Service Agent"
  }
}
```

##### `add_agent`
Add a new agent to the floor.

**Request:**
```json
{
  "jsonrpc":"2.0",
  "method":"add_agent",
  "params":{"agent_id":"agent-123","name":"Test Agent","role":"Service"},
  "id":4
}
```

##### `create_task`
Create a new task on the floor.

**Request:**
```json
{
  "jsonrpc":"2.0",
  "method":"create_task",
  "params":{"task_id":"task-123","title":"Implement solver","assigned_to":"agent-123"},
  "id":5
}
```

##### `list_agents`
List all agents on the floor.

##### `list_tasks`
List all tasks on the floor.

##### `shutdown`
Gracefully shutdown the floor.

## Building and Running

### Prerequisites

- GNU Fortran (gfortran) 8.0 or later
- Make (optional, for using Makefile)

### Building

#### Using Make:
```bash
make
```

#### Manual compilation:
```bash
gfortran -std=f2008 -Wall -Wextra -fcheck=all -fbounds-check -O2 -o department_floor department_floor.f90
```

### Running

The floor reads JSON-RPC requests from stdin and writes responses to stdout:

```bash
echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | ./department_floor
```

### Testing

Run basic tests:
```bash
make test
```

### Compiler Flags

- `-std=f2008`: Enforce Fortran 2008 standard
- `-Wall`: Enable all warnings
- `-Wextra`: Enable extra warnings
- `-fcheck=all`: Enable all runtime checks
- `-fbounds-check`: Enable array bounds checking
- `-O2`: Optimization level 2 (production)

For debugging, use:
```bash
gfortran -std=f2008 -g -fbacktrace -fcheck=all -fbounds-check -o department_floor department_floor.f90
```

## Code Examples

### Array Bounds Validation
```fortran
subroutine validate_array_bounds(array_size, index, is_valid)
    implicit none
    integer, intent(in) :: array_size, index
    logical, intent(out) :: is_valid
    
    is_valid = .false.
    if (index >= 1 .and. index <= array_size) then
        is_valid = .true.
    end if
end subroutine
```

### Safe Matrix Multiplication
```fortran
subroutine safe_matrix_multiply(a, b, c, n, m, p, status)
    implicit none
    integer, intent(in) :: n, m, p
    real(8), intent(in) :: a(n,m), b(m,p)
    real(8), intent(out) :: c(n,p)
    integer, intent(out) :: status
    integer :: i, j, k
    
    status = 0
    if (n <= 0 .or. m <= 0 .or. p <= 0) then
        status = -1
        return
    end if
    
    c = 0.0d0
    do j = 1, p
        do i = 1, n
            do k = 1, m
                c(i,j) = c(i,j) + a(i,k) * b(k,j)
            end do
        end do
    end do
end subroutine
```

## Performance Considerations

1. **Column-Major Ordering**: Optimize loop nesting for Fortran's column-major array storage
2. **Array Contiguity**: Use contiguous arrays for better cache performance
3. **Compiler Optimization**: Use `-O2` or `-O3` for production builds
4. **BLAS/LAPACK**: Consider linking to optimized linear algebra libraries
5. **Parallelization**: Use OpenMP or Coarray Fortran for parallel processing

## Integration

This floor integrates with the Miniature Office ecosystem:

- Receives tasks via JSON-RPC
- Reports status and results via JSON-RPC
- Escalates failures to Manager Office
- Coordinates with other floors for multi-language projects
- Maintains language sovereignty while cooperating

## References

- Fortran 2008 Standard (ISO/IEC 1539-1:2010)
- Modern Fortran Style Guide
- Numerical Recipes in Fortran
- LAPACK Users' Guide
