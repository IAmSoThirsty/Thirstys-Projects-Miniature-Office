# Floor 20 - Haskell Jurisdiction

**Language:** Haskell  
**Floor Number:** 20  
**Domain:** Pure functional programming, Type-safe systems, Compilers

## Architectural Laws

This floor operates under strict Haskell principles:
- **Type Safety First**: Type safety > runtime checks
- **No Partial Functions**: All functions are total; use Maybe/Either for failure cases
- **Monadic Error Handling**: Explicit error propagation through monads
- **Purity by Default**: IO operations explicitly tracked in type system
- **Immutability**: All data structures are immutable

## Security Doctrine

- **Type-Level Security**: Security properties enforced at compile time
- **Safe API Design**: No unsafe operations without explicit justification
- **Parametricity**: Free theorems guarantee behavior
- **No Undefined Behavior**: Compiler prevents entire classes of bugs

## Agent Staffing

The floor employs specialized agents across 6 offices:

### Architecture Office
- Type-safe system design
- Monad transformer stacks
- Domain modeling with types

### Implementation Office
- Pure functional implementation
- Monadic effects management
- Performance optimization

### Review Office
- Code review with property-based testing mindset
- Type-safety verification
- API design review

### Test Office
- QuickCheck property testing
- HUnit unit testing
- Type-driven testing

### Security Office
- Type-level security analysis
- Safe API audit
- Dependency security

### Manager Office
- Project coordination
- Dependency management (Cabal/Stack)
- Build optimization

## Prerequisites

- GHC 9.2+ (Glasgow Haskell Compiler)
- Cabal 3.6+ or Stack 2.9+

Install via GHCup (recommended):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## Building

### Using Cabal:
```bash
cabal update
cabal build
```

For optimized release build:
```bash
cabal build --enable-optimization=2
```

### Using Stack:
```bash
stack setup
stack build
```

## Running

After building with Cabal:
```bash
cabal run department-floor
```

Or with Stack:
```bash
stack exec department-floor
```

Run directly from binary:
```bash
./dist-newstyle/build/*/ghc-*/department-floor-0.1.0.0/x/department-floor/build/department-floor/department-floor
```

## API

The floor implements JSON-RPC over stdin/stdout. All operations are pure and type-safe.

### Get Floor Info
```json
{"method": "get_info"}
```

Response includes floor metadata, agents, tasks, and architectural laws.

### Add Agent
```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "agent_1",
    "name": "Alice",
    "role": "Architect",
    "capabilities": ["design"]
  }
}
```

Roles: `Architect`, `Implementer`, `Reviewer`, `Tester`, `SecurityExpert`, `Manager`

### Create Task
```json
{
  "method": "create_task",
  "params": {
    "task_id": "task_1",
    "title": "Implement type-safe parser",
    "assigned_to": "agent_1"
  }
}
```

### Process Code
```json
{
  "method": "process_code",
  "params": {
    "code": "module Main where\nmain :: IO ()\nmain = putStrLn \"Hello\"",
    "operation": "analyze"
  }
}
```

Operations:
- `analyze`: Code analysis (lines, functions, types, purity score)
- `format`: Code formatting (would use stylish-haskell in production)
- `lint`: Linting (would use HLint in production)

## Testing

Test the floor interactively:
```bash
echo '{"method": "get_info"}' | cabal run department-floor
```

Test code analysis:
```bash
echo '{"method": "process_code", "params": {"code": "factorial n = product [1..n]", "operation": "analyze"}}' | cabal run department-floor
```

## Development

Format code:
```bash
stylish-haskell -i *.hs
```

Lint code:
```bash
hlint .
```

Type check:
```bash
cabal build --ghc-options="-fno-code"
```

## Jurisdiction

This floor has exclusive authority over:
- Pure functional programming paradigms
- Type-safe system design
- Compiler and interpreter implementation
- Domain-specific language design
- Monadic effect systems
- Property-based testing

## Inter-Floor Communication

Communicates with other floors via JSON-RPC protocol, maintaining type safety at boundaries through Aeson serialization.
