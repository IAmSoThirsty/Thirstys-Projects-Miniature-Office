# Floor 25 - MATLAB/Octave Jurisdiction

**Language:** MATLAB / GNU Octave  
**Floor Number:** 25  
**Domain:** Numerical modeling, Matrix operations, Signal processing, Visualization

## Domain and Jurisdiction

Floor 25 is the MATLAB/Octave jurisdiction, responsible for all numerical modeling and matrix-based computation within the Miniature Office. This floor handles:

- **Numerical Modeling**: System simulation, mathematical modeling, differential equations
- **Matrix Operations**: Linear algebra, eigenvalue problems, matrix decompositions
- **Signal Processing**: Filtering, FFT, spectral analysis, time-series processing
- **Visualization**: Plotting, 3D graphics, data visualization
- **Data Analysis**: Statistical analysis, curve fitting, optimization

## Architectural Laws

The MATLAB/Octave jurisdiction operates under architectural laws that reflect efficient numerical computing:

### 1. Vectorization Over Loops
Prefer vectorized operations over explicit loops. Use element-wise operators (`.* ./ .^`). Exploit MATLAB's optimized BLAS/LAPACK routines. Loops are acceptable only when vectorization is impossible.

### 2. Explicit Matrix Dimensions
Always validate matrix dimensions before operations. Document expected dimensions. Check compatibility for multiplication, addition, etc. Dimension mismatches are critical errors.

### 3. Preallocate Arrays
Preallocate arrays before loops using `zeros()`, `ones()`, `NaN()`. Never grow arrays dynamically in loops. Memory reallocation kills performance.

### 4. Avoid Dynamic Resizing
Fixed-size arrays whenever possible. If dynamic sizing needed, grow in chunks. Use cell arrays for heterogeneous collections.

### 5. Clear Variable Scope
Use functions over scripts. Avoid global variables. Clear function semantics with input validation. Document all function signatures.

## Security Doctrine

Security in MATLAB/Octave focuses on numerical correctness and data integrity:

### Dimension Validation
- Check matrix dimensions before operations
- Validate vector lengths for element-wise operations
- Ensure conformability for matrix multiplication
- Document dimension requirements in function headers

### NaN and Infinity Checking
- Check for NaN in results (division by zero, sqrt of negative)
- Detect Inf values (overflow conditions)
- Use `isnan()` and `isinf()` for validation
- Handle special cases explicitly

### Index Bounds Verification
- Validate array indices before access
- Check for positive, integer indices
- Ensure indices within array bounds
- Use logical indexing for safety

### Numerical Stability
- Check condition numbers for ill-conditioned matrices
- Detect singular matrices before inversion
- Use appropriate solvers for different problem types
- Validate eigenvalue computations

## Office Structure

All six offices are staffed and operational:

### 1. Architecture Office
**Role:** Algorithm design, data structure planning, performance optimization  
**Minimum Staff:** 3 agents  
**Agents:**
- Algorithm Architect: Numerical algorithm selection
- Performance Optimizer: Vectorization and optimization strategies
- API Designer: Function interface design

**Capabilities:**
- Algorithm selection and design
- Performance profiling and optimization
- Memory layout planning
- API specification

### 2. Implementation Office
**Role:** Function implementation, vectorization, module development  
**Minimum Staff:** 3 agents  
**Agents:**
- Matrix Service Agent: Core matrix operations
- Data Model Agent: Data validation and type checking
- Operations Agent: Numerical method implementation

**Capabilities:**
- Function and script development
- Vectorization implementation
- Numerical method coding
- Integration with toolboxes

### 3. Review Office
**Role:** Code review, performance analysis, correctness verification  
**Minimum Staff:** 2 agents  
**Agents:**
- Code Review Specialist: MATLAB idiom compliance
- Performance Analyst: Efficiency validation

**Capabilities:**
- Code quality assessment
- Performance review
- Vectorization opportunities identification
- Best practices enforcement

### 4. Test Office
**Role:** Unit testing, numerical validation, regression testing  
**Minimum Staff:** 2 agents  
**Agents:**
- Unit Test Engineer: Function testing
- Numerical Validator: Result accuracy verification

**Capabilities:**
- Test case development
- Numerical accuracy validation
- Regression testing
- Performance benchmarking

### 5. Security Office
**Role:** Dimension checking, NaN/Inf detection, bounds validation  
**Minimum Staff:** 2 agents  
**Agents:**
- Dimension Validator: Matrix dimension checking
- Numerical Safety Officer: NaN/Inf detection

**Capabilities:**
- Dimension validation
- Special value detection
- Index bounds checking
- Stability analysis

### 6. Manager Office
**Role:** Project coordination, task management, resource allocation  
**Minimum Staff:** 1 agent  
**Agent:**
- Floor Manager: Overall coordination

**Capabilities:**
- Task assignment and tracking
- Agent coordination
- Deadline management
- Escalation handling

## Agent Staffing

**Minimum Total:** 13 agents across all offices  
**Current Deployment:** 3 primary agents (Service, Data, Operations)

### Primary Agents

#### 1. Matrix Service Agent (`matlab-service-1`)
**Role:** Service  
**Capabilities:**
- Vectorization and optimization
- Matrix analysis
- Performance profiling
- Code pattern recognition

**Responsibilities:**
- Analyze MATLAB code structure
- Count functions and matrix operations
- Identify vectorization opportunities
- Optimize computational patterns

#### 2. Data Model Agent (`matlab-data-1`)
**Role:** Data/Model  
**Capabilities:**
- Dimension validation
- Type checking
- Data structure management
- Input validation

**Responsibilities:**
- Validate matrix dimensions before operations
- Check data types and structures
- Ensure index bounds safety
- Manage data integrity

#### 3. Operations Agent (`matlab-ops-1`)
**Role:** Operations  
**Capabilities:**
- Linear algebra operations
- Signal processing algorithms
- Numerical methods
- Matrix computations

**Responsibilities:**
- Implement safe matrix multiplication
- Execute numerical algorithms
- Perform signal processing operations
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
    "floor":25,
    "language":"matlab",
    "capabilities":["matrix_operations","numerical_modeling","signal_processing"]
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
    "floor_number":25,
    "language":"matlab",
    "domain":"Numerical modeling, Matrix operations, Signal processing, Visualization",
    "offices":["Architecture Office",...],
    "agents":[...],
    "architectural_laws":[...],
    "security_doctrine":[...]
  }
}
```

##### `analyze_code`
Analyze MATLAB/Octave code.

**Request:**
```json
{
  "jsonrpc":"2.0",
  "method":"analyze_code",
  "params":{"code":"A = zeros(10,10);\nB = A * A';"},
  "id":3
}
```

**Response:**
```json
{
  "jsonrpc":"2.0",
  "id":3,
  "result":{
    "language":"matlab",
    "lines":2,
    "functions":0,
    "matrices":1,
    "vectorized_ops":1,
    "agent":"Matrix Service Agent"
  }
}
```

##### `add_agent`
Add a new agent to the floor.

##### `create_task`
Create a new task.

##### `list_agents`
List all agents.

##### `list_tasks`
List all tasks.

##### `shutdown`
Shutdown the floor.

## Building and Running

### Prerequisites

**Option 1: MATLAB**
- MATLAB R2016b or later (for JSON support)
- Core MATLAB license

**Option 2: GNU Octave**
- GNU Octave 4.0 or later
- Free and open-source alternative to MATLAB

### Running with MATLAB

```bash
matlab -nodisplay -nosplash -r "department_floor"
```

Or with piped input:
```bash
echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | matlab -nodisplay -nosplash -r "department_floor"
```

### Running with Octave

```bash
octave --silent --eval "department_floor"
```

Or with piped input:
```bash
echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | octave --silent --eval "department_floor"
```

### Testing

Create a test script `test_floor.m`:
```matlab
% Test the department floor
fprintf('Testing Floor 25...\n');

% Test 1: Matrix multiplication validation
A = [1 2; 3 4];
B = [5 6; 7 8];
[C, status] = safe_matrix_multiply(A, B);
assert(status == 0, 'Matrix multiplication failed');

% Test 2: Dimension validation
is_valid = validate_matrix_dimensions(A, B, 'multiply');
assert(is_valid, 'Dimension validation failed');

% Test 3: Vector access
v = [1 2 3 4 5];
[val, status] = safe_vector_access(v, 3);
assert(status == 0 && val == 3, 'Vector access failed');

fprintf('All tests passed!\n');
```

### Interactive Testing

```matlab
>> department_floor
{"jsonrpc":"2.0","method":"initialize","id":1}
{"jsonrpc":"2.0","id":1,"result":{"status":"initialized","floor":25,"language":"matlab","capabilities":["matrix_operations","numerical_modeling","signal_processing"]}}

{"jsonrpc":"2.0","method":"get_floor_info","id":2}
...
```

## Code Examples

### Dimension Validation
```matlab
function is_valid = validate_matrix_dimensions(A, B, operation)
    is_valid = false;
    
    if isempty(A) || isempty(B)
        return;
    end
    
    [m1, n1] = size(A);
    [m2, n2] = size(B);
    
    switch operation
        case 'multiply'
            is_valid = (n1 == m2);
        case 'add'
            is_valid = (m1 == m2) && (n1 == n2);
    end
end
```

### Safe Matrix Multiplication
```matlab
function [C, status] = safe_matrix_multiply(A, B)
    status = 0;
    C = [];
    
    if ~validate_matrix_dimensions(A, B, 'multiply')
        status = -2;
        return;
    end
    
    C = A * B;
    
    if any(isnan(C(:))) || any(isinf(C(:)))
        status = -3;
    end
end
```

### Vectorized Operations
```matlab
% Bad: Loop-based
for i = 1:length(x)
    y(i) = x(i)^2 + 2*x(i) + 1;
end

% Good: Vectorized
y = x.^2 + 2*x + 1;
```

## Performance Optimization

1. **Vectorization**: Replace loops with vectorized operations
2. **Preallocation**: Allocate arrays before loops
3. **Built-in Functions**: Use optimized built-ins (sum, mean, etc.)
4. **JIT Compilation**: Let MATLAB/Octave JIT optimize code
5. **Profiling**: Use `profile` to identify bottlenecks

## Compatibility Notes

The floor is designed to work with both MATLAB and GNU Octave:

- Uses conditional checks for MATLAB vs Octave
- Provides fallback for missing JSON functions
- Compatible with Octave 4.0+ and MATLAB R2016b+
- Avoids MATLAB-only or Octave-only features when possible

## Integration

This floor integrates with the Miniature Office:

- Receives tasks via JSON-RPC
- Reports results via JSON-RPC
- Maintains language sovereignty
- Coordinates with other floors
- Escalates failures appropriately

## References

- MATLAB Programming Style Guidelines
- GNU Octave Manual
- Numerical Computing with MATLAB
- Matrix Computations (Golub & Van Loan)
