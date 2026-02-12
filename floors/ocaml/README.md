# Floor 21 - OCaml Jurisdiction

**Language:** OCaml  
**Floor Number:** 21  
**Domain:** Functional systems, Compilers, Type-safe systems

## Architectural Law

**Functional Programming Principles**
- **Immutability by default**: Prefer immutable data structures; avoid refs and mutable fields
- **Pure functions**: Functions should be side-effect free when possible
- **Type-driven development**: Let the type system guide design
- **Pattern matching**: Use exhaustive pattern matching for control flow
- **Higher-order functions**: Embrace map, fold, filter, and custom combinators
- **Algebraic data types**: Use variants and records for domain modeling
- **Module abstraction**: Use module signatures to hide implementation details
- **Tail recursion**: Ensure recursive functions are tail-recursive for performance

**Code Quality Standards**
- Strong static typing with type inference
- Explicit type annotations for public APIs
- Module organization with .mli interface files
- Descriptive variant constructor names (CamelCase)
- Function names in snake_case
- Comprehensive pattern matching (avoid incomplete matches)
- Use Result type for error handling instead of exceptions
- Documentation comments for modules and public functions

## Security Doctrine

**Critical Security Requirements**
- **Type Safety**: Never use `Obj.magic` to bypass the type system
- **Memory Safety**: OCaml provides automatic memory management; no manual pointers
- **No Null References**: Use `option` type instead of null
- **Pattern Matching Exhaustiveness**: Compiler enforces exhaustive pattern matching
- **Immutability**: Default immutability prevents many concurrent access bugs
- **Exception Safety**: Prefer Result type over exceptions for expected errors
- **Bounds Checking**: All array/string accesses are bounds-checked at runtime
- **Safe FFI**: Validate all data crossing C FFI boundaries

**Functional Safety Patterns**
- Use `option` type for values that might not exist
- Use `result` type for operations that can fail
- Avoid global mutable state
- Use proper exception handling with try/with for unexpected errors
- Validate external input before processing
- Use phantom types for compile-time safety guarantees
- Leverage GADTs for type-level constraints
- Avoid `Obj.magic`, `Marshal.from_string` on untrusted data

**Thread Safety**
- OCaml runtime has a global lock (GIL-like) preventing most race conditions
- Use immutable data structures for shared state
- If using mutable refs with threading, protect with Mutex
- Prefer message passing (Lwt, Async) over shared memory

## Offices and Agent Staffing

### Architecture Office
**Staff:** Data Model Agent, Type System Agent, Module Design Agent  
**Responsibilities:**
- Algebraic data type design (variants, records)
- Type system design and inference
- Module signature definition
- Phantom type and GADT design
- Functor and first-class module design

**Key Agent: Data Model Agent**
- Validates OCaml data structures (lists, options, records, variants)
- Infers variant types from data
- Transforms data to/from algebraic types
- Type inference and checking
- Handles JSON serialization/deserialization

### Implementation Office
**Staff:** Service Agent, Functional Patterns Agent, Concurrency Agent  
**Responsibilities:**
- Functional programming pattern implementation
- Higher-order function design
- Pattern matching implementation
- Module system usage (functors, first-class modules)
- Concurrency with Lwt/Async
- External C binding implementation

**Key Agent: Service Agent**
- Validates functional programming style
- Checks for proper use of pattern matching
- Analyzes type usage and polymorphism
- Detects improper mutation (refs, mutable fields)
- Enforces immutability best practices
- Module signature compliance

### Review Office
**Staff:** Operations Agent, Type Checker Agent, Quality Metrics Agent  
**Responsibilities:**
- Code structure analysis (modules, functions, types)
- Type correctness verification
- Pattern matching exhaustiveness checking
- Quality scoring based on functional principles
- Purity analysis (side-effect detection)
- Performance profiling suggestions

**Key Agent: Operations Agent**
- Analyzes code structure (modules, functions, types, variants)
- Counts pattern matches and algebraic types
- Quality checks (type annotations, exhaustiveness, immutability)
- Detects mutation and side effects
- Suggests functional idioms
- Performance analysis (tail recursion, memory allocation)

### Test Office
**Staff:** Test Agent, Alcotest Agent, Property Testing Agent  
**Responsibilities:**
- Alcotest test framework management
- OUnit test support
- Property-based testing with QCheck
- Test coverage analysis
- Inline test execution (ppx_inline_test)
- Expect test support

**Key Agent: Test Agent**
- Detects test framework (Alcotest, OUnit)
- Counts test cases and assertions
- Analyzes test organization
- Validates test coverage
- Reports test metrics
- Integration with dune test runner

### Security Office
**Staff:** Security Agent, Type Safety Agent, Purity Checker Agent  
**Responsibilities:**
- Type safety bypass detection (Obj.magic)
- Unsafe operation identification
- Mutation detection in critical code
- Exception handling validation
- FFI security (external C functions)
- Format string safety

**Key Agent: Security Agent**
- Scans for `Obj.magic` (type system bypass - CRITICAL)
- Detects unsafe string operations
- Identifies mutable refs with threading (race conditions)
- Checks for unchecked exceptions
- Validates external C bindings
- Reports vulnerability severity
- Format string validation

### Manager Office
**Staff:** Manager Agent, Build System Agent, Dependency Manager Agent  
**Responsibilities:**
- Task orchestration across agents
- Dune build system management
- OPAM dependency resolution
- Workflow coordination
- Resource allocation

**Key Agent: Manager Agent**
- Delegates tasks to appropriate agents
- Manages workflow execution
- Coordinates build and test processes
- Monitors resource utilization
- Reports workflow status

## API Documentation

### JSON-RPC Methods

#### `get_info`
Get floor information including agents, tasks, and offices.

**Request:**
```json
{"method": "get_info", "params": {}}
```

**Response:**
```json
{
  "floorNumber": 21,
  "language": "ocaml",
  "domain": "Functional systems, Compilers, Type-safe systems",
  "offices": ["Architecture Office", "Implementation Office", "Review Office", "Test Office", "Security Office", "Manager Office"],
  "agentCount": 6,
  "taskCount": 0,
  "agents": [...],
  "tasks": [...]
}
```

#### `process_code`
Process OCaml code with various operations.

**Operations:**
- `analyze`: Analyze code structure (modules, functions, types, variants)
- `quality`: Check code quality and functional programming practices
- `security`: Scan for security vulnerabilities (Obj.magic, unsafe operations)
- `test_analysis`: Analyze test structure (Alcotest, OUnit)

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "type color = Red | Green | Blue\nlet to_string = function | Red -> \"red\" | Green -> \"green\" | Blue -> \"blue\"",
    "operation": "analyze"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "analysis": {
    "lines": 2,
    "modules": 0,
    "functions": 1,
    "types": 1,
    "variants": 3,
    "language": "ocaml"
  }
}
```

#### `execute_service`
Execute service agent operations.

**Services:**
- `validate_style`: Validate functional programming style
- `check_functional_patterns`: Analyze functional patterns (map, fold, pattern matching)
- `analyze_types`: Analyze type system usage

**Request:**
```json
{
  "method": "execute_service",
  "params": {
    "serviceName": "check_functional_patterns",
    "code": "List.map (fun x -> x * 2) [1; 2; 3]"
  }
}
```

#### `process_data`
Process data with data model agent.

**Operations:**
- `validate`: Validate data structure
- `infer_variant`: Infer variant type from data
- `transform_to_variant`: Transform data to variant representation

**Request:**
```json
{
  "method": "process_data",
  "params": {
    "operation": "validate",
    "data": {"key": "value"}
  }
}
```

#### `add_agent`
Add a new agent to the floor.

**Request:**
```json
{
  "method": "add_agent",
  "params": {
    "agentId": "custom-001",
    "name": "CustomAgent",
    "role": "Custom Role",
    "office": "Implementation Office",
    "capabilities": ["capability1", "capability2"]
  }
}
```

#### `create_task`
Create a new task on the floor.

**Request:**
```json
{
  "method": "create_task",
  "params": {
    "taskId": "task-001",
    "title": "Implement feature X",
    "assignedTo": "service-001"
  }
}
```

## Installation and Running

### Requirements
- **OCaml**: 4.14.0 or later
- **Dune**: Build system (3.0+)
- **OPAM**: Package manager
- **Dependencies**: yojson (JSON library), str (regex)

### Installation

**Using OPAM (recommended):**
```bash
# Install OPAM if not already installed
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

# Initialize OPAM
opam init -y
eval $(opam env)

# Install OCaml compiler
opam switch create 4.14.0
eval $(opam env)

# Install dependencies
opam install -y dune yojson
```

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install -y opam
opam init -y
eval $(opam env)
opam install -y ocaml-base-compiler dune yojson
```

**macOS:**
```bash
brew install opam
opam init -y
eval $(opam env)
opam install -y dune yojson
```

### Building

**Using Dune:**
```bash
cd floors/ocaml
dune build
```

The executable will be at `_build/default/department_floor.exe`

**Running:**
```bash
dune exec department_floor
```

**Manual Compilation:**
```bash
ocamlfind ocamlopt -package yojson,str,unix -linkpkg -o department_floor department_floor.ml
```

### Running the Floor

**Interactive Mode:**
```bash
dune exec department_floor
# or
./_build/default/department_floor.exe
```

**JSON-RPC Communication:**
```bash
echo '{"method":"get_info","params":{}}' | dune exec department_floor
```

**From Pipeline:**
```bash
cat request.json | dune exec department_floor
```

### Testing

```bash
# Test basic functionality
echo '{"method":"get_info","params":{}}' | dune exec department_floor

# Test code analysis
echo '{"method":"process_code","params":{"code":"type t = A | B","operation":"analyze"}}' | dune exec department_floor

# Run dune tests (if test suite exists)
dune test
```

## Security Considerations

### Type Safety

**GOOD: Using option type:**
```ocaml
let find_user id users =
  List.find_opt (fun u -> u.id = id) users

match find_user 123 users with
| Some user -> process user
| None -> handle_not_found ()
```

**BAD: Never bypass type system:**
```ocaml
(* NEVER DO THIS *)
let unsafe_cast x = Obj.magic x  (* CRITICAL VULNERABILITY *)
```

### Immutability

**GOOD: Immutable data structures:**
```ocaml
type user = { id: int; name: string }

let update_name user new_name =
  { user with name = new_name }  (* Creates new record *)
```

**BAD: Mutable references:**
```ocaml
(* Avoid unless necessary *)
let counter = ref 0
let increment () = counter := !counter + 1  (* Mutation *)
```

### Error Handling

**GOOD: Using Result type:**
```ocaml
type ('a, 'e) result = Ok of 'a | Error of 'e

let divide x y =
  if y = 0 then
    Error "Division by zero"
  else
    Ok (x / y)

match divide 10 2 with
| Ok result -> Printf.printf "Result: %d\n" result
| Error msg -> Printf.eprintf "Error: %s\n" msg
```

**GOOD: Using try/with for unexpected errors:**
```ocaml
let read_file filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Ok content
  with
  | Sys_error msg -> Error msg
```

### Pattern Matching Exhaustiveness

**GOOD: Exhaustive matching:**
```ocaml
type color = Red | Green | Blue

let to_string = function
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"
  (* Compiler ensures all cases covered *)
```

## Design Patterns

### Algebraic Data Types
```ocaml
type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr

let rec eval = function
  | Const n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
```

### Option and Result Types
```ocaml
type 'a option = None | Some of 'a
type ('a, 'e) result = Ok of 'a | Error of 'e

let bind opt f =
  match opt with
  | None -> None
  | Some x -> f x

let ( >>= ) = bind
```

### Module Signatures
```ocaml
module type STACK = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
end

module Stack : STACK = struct
  type 'a t = 'a list
  let empty = []
  let push x s = x :: s
  let pop = function
    | [] -> None
    | x :: xs -> Some (x, xs)
end
```

### Functors
```ocaml
module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Make_Set (Ord : ORDERED) = struct
  type elt = Ord.t
  type t = elt list
  
  let empty = []
  let rec add x = function
    | [] -> [x]
    | y :: ys ->
        match Ord.compare x y with
        | 0 -> y :: ys
        | n when n < 0 -> x :: y :: ys
        | _ -> y :: add x ys
end
```

## Performance Considerations

- Use tail recursion to avoid stack overflows
- Prefer `List.rev_map` + `List.rev` over `List.map` for large lists
- Use `Seq` for lazy evaluation of large collections
- Cache expensive computations with memoization
- Use `Bytes` instead of `String` for mutable buffers
- Profile with `perf`, `ocamlprof`, or landmarks
- Use flambda compiler for aggressive inlining
- Consider `Jane Street Base` library for optimized collections

## Integration Examples

### CI/CD Integration
```yaml
# GitHub Actions
steps:
  - name: Setup OCaml
    uses: ocaml/setup-ocaml@v2
    with:
      ocaml-compiler: 4.14.0
  - name: Install dependencies
    run: opam install -y dune yojson
  - name: Build
    run: dune build
  - name: Test
    run: echo '{"method":"get_info","params":{}}' | dune exec department_floor
```

### Docker Support
```dockerfile
FROM ocaml/opam:debian-ocaml-4.14
WORKDIR /app
COPY . .
RUN opam install -y dune yojson
RUN eval $(opam env) && dune build
CMD ["dune", "exec", "department_floor"]
```

## Troubleshooting

### Common Issues

**Issue: dune: command not found**
```
Solution: opam install dune
```

**Issue: Library yojson not found**
```
Solution: opam install yojson
```

**Issue: OCaml version mismatch**
```
Solution: opam switch create 4.14.0 && eval $(opam env)
```

**Issue: Pattern matching not exhaustive**
```
Solution: Add all missing cases or use wildcard pattern (_)
```

## References

- [OCaml Manual](https://ocaml.org/manual/)
- [Real World OCaml](https://dev.realworldocaml.org/)
- [Dune Documentation](https://dune.readthedocs.io/)
- [OPAM Package Manager](https://opam.ocaml.org/)
- [Yojson Library](https://github.com/ocaml-community/yojson)
- [OCaml.org](https://ocaml.org/)
