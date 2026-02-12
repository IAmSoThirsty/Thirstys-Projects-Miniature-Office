# Floor 7 - Shell/Bash Jurisdiction

**Language:** Bash  
**Floor Number:** 7  
**Domain:** System automation, Orchestration

## Architecture

This floor implements department logic in Bash, demonstrating:
- **Language Sovereignty**: All code written in Bash
- **Identical Topology**: 6 offices (Architecture, Implementation, Review, Test, Security, Manager)
- **Explicit Error Handling**: set -e for fail-fast
- **No Silent Failures**: All errors are reported
- **ShellCheck Compliance**: Bash best practices

## Prerequisites

- Bash 4.0+
- jq (for JSON parsing)

## Running

```bash
./department_floor.sh
```

Or:
```bash
bash department_floor.sh
```

## API

Same JSON-RPC API as other floors. See Python floor README for examples.

## Testing

Test the floor interactively:
```bash
echo '{"method": "get_info"}' | bash department_floor.sh
```

## Linting

```bash
shellcheck department_floor.sh
```
