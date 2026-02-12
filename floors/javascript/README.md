# Floor 4 - JavaScript/TypeScript Jurisdiction

**Language:** JavaScript  
**Floor Number:** 4  
**Domain:** Frontend logic, Tooling, Node services

## Architecture

This floor implements department logic in JavaScript/Node.js, demonstrating:
- **Language Sovereignty**: All code written in JavaScript
- **Identical Topology**: 6 offices (Architecture, Implementation, Review, Test, Security, Manager)
- **Contract-Bound Operation**: All operations via JSON-RPC protocol
- **Async Correctness**: Proper async/await patterns
- **ESLint Compliance**: Standard JavaScript practices

## Prerequisites

- Node.js 14+ or later

## Running

```bash
node department_floor.js
```

The floor listens on stdin for JSON-RPC requests and writes responses to stdout.

## API

Same JSON-RPC API as other floors. See Python floor README for examples.

## Testing

Test the floor interactively:
```bash
echo '{"method": "get_info"}' | node department_floor.js
```
