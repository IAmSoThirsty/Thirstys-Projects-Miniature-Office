# Floor 5 - Go Jurisdiction

**Language:** Go  
**Floor Number:** 5  
**Domain:** Network services, Concurrency-heavy systems, Infrastructure logic

## Architecture

This floor implements department logic in Go, demonstrating:
- **Language Sovereignty**: All code written in Go
- **Identical Topology**: 6 offices (Architecture, Implementation, Review, Test, Security, Manager)
- **Explicit Concurrency**: Clear goroutine and channel usage
- **No Goroutine Leaks**: Proper cleanup and cancellation
- **go fmt Compliance**: Standard Go formatting

## Prerequisites

- Go 1.21+

## Building

```bash
go build -o department_floor department_floor.go
```

## Running

Run directly:
```bash
go run department_floor.go
```

Or run compiled binary:
```bash
./department_floor
```

## API

Same JSON-RPC API as other floors. See Python floor README for examples.

## Testing

Test the floor interactively:
```bash
echo '{"method": "get_info"}' | go run department_floor.go
```

## Linting

```bash
go fmt department_floor.go
go vet department_floor.go
```
