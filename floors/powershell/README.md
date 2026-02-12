# Floor 18 - PowerShell Jurisdiction

**Language:** PowerShell  
**Floor Number:** 18  
**Domain:** Windows automation, System administration, DevOps scripting

## Architectural Law

**PowerShell Best Practices**
- Verb-Noun naming convention for all functions and cmdlets
- Use approved PowerShell verbs (Get, Set, New, Remove, Test, Invoke, etc.)
- Follow pipeline-first design principles
- Implement [CmdletBinding()] for advanced functions
- Support pipeline input with ValueFromPipeline parameter attribute
- Use StrictMode for type safety
- Comprehensive error handling with Try/Catch
- Comment-based help (.SYNOPSIS, .DESCRIPTION, .PARAMETER, .EXAMPLE)
- Type declarations for parameters and return values

**Code Quality Standards**
- PowerShell 7+ modern syntax
- Explicit typing with `[type]` annotations
- Use of PowerShell classes for object-oriented design
- Strong parameter validation ([ValidateNotNull], [ValidateScript], etc.)
- Proper use of scopes (Script, Global, Local)
- ErrorActionPreference management
- Output streams (Success, Error, Warning, Verbose, Debug)

## Security Doctrine

**Critical Security Requirements**
- **Command Injection Prevention**: Never use `Invoke-Expression` or `iex` with user input
- **Credential Handling**: Use SecureString and PSCredential objects; never hardcode passwords
- **Execution Policy**: Respect and document required execution policies
- **Input Validation**: Validate all user input and external data
- **Safe File Operations**: Use `-Confirm` and `-WhatIf` for destructive operations
- **Web Security**: Prefer HTTPS; validate certificates; avoid insecure connections
- **Deserialization**: Validate XML/JSON before import; avoid untrusted CliXml files
- **Registry Operations**: Validate registry paths; check permissions before modifications
- **Process Execution**: Sanitize arguments to `Start-Process`; avoid shell injection

**Security Best Practices**
- Use constrained language mode for untrusted scripts
- Implement Just Enough Administration (JEA) for privilege separation
- Log security-relevant operations
- Use Windows Event Log for audit trails
- Implement proper error handling to avoid information disclosure
- Validate file paths to prevent directory traversal
- Use `-ErrorAction Stop` to catch all errors

## Offices and Agent Staffing

### Architecture Office
**Staff:** Data Model Agent, Schema Agent, Object Design Agent  
**Responsibilities:**
- PSCustomObject design and implementation
- Hashtable and dictionary management
- XML/JSON schema design
- Type system design with PowerShell classes
- Data serialization and deserialization

**Key Agent: Data Model Agent**
- Validates PowerShell data structures (Hashtable, PSCustomObject, Arrays)
- Transforms data between formats (hashtable ↔ PSCustomObject)
- Infers types from runtime objects
- Implements type-safe conversions
- Manages object property validation

### Implementation Office
**Staff:** Service Agent, Automation Agent, API Integration Agent  
**Responsibilities:**
- Windows service management and monitoring
- Scheduled task creation and management
- REST API integration and consumption
- Registry operations and management
- WMI/CIM object manipulation
- Remote PowerShell (PSRemoting) implementation

**Key Agent: Service Agent**
- Validates PowerShell style conventions (Verb-Noun, CmdletBinding)
- Checks cmdlet usage and pipeline patterns
- Analyzes security vulnerabilities (Invoke-Expression, hardcoded credentials)
- Service orchestration and workflow automation
- Windows automation task execution

### Review Office
**Staff:** Operations Agent, Quality Metrics Agent, Best Practices Agent  
**Responsibilities:**
- Code analysis and metrics collection
- PowerShell best practices enforcement
- Quality scoring based on PowerShell conventions
- Performance profiling and optimization suggestions
- Script Analyzer (PSScriptAnalyzer) integration

**Key Agent: Operations Agent**
- Analyzes PowerShell code structure (functions, classes, parameters)
- Counts CmdletBinding attributes and parameter declarations
- Quality checks (error handling, comment-based help, StrictMode)
- Identifies anti-patterns and code smells
- Suggests PowerShell idioms and improvements
- Performance analysis

### Test Office
**Staff:** Test Agent, Pester Agent, Coverage Agent  
**Responsibilities:**
- Pester test framework management
- Unit test and integration test execution
- Code coverage analysis with Pester
- Mock object creation and management
- Test automation and CI/CD integration

**Key Agent: Test Agent**
- Detects Pester test structure (Describe, Context, It blocks)
- Analyzes test coverage and assertion counts
- Counts Should assertions and Mock usage
- Validates test organization and naming
- Reports test metrics and statistics
- Integration with CI/CD pipelines

### Security Office
**Staff:** Security Agent, Vulnerability Scanner Agent, Compliance Agent  
**Responsibilities:**
- Security vulnerability scanning
- Command injection detection
- Credential exposure detection
- Unsafe operation identification (Remove-Item -Recurse -Force)
- Execution policy compliance
- PowerShell security best practices enforcement

**Key Agent: Security Agent**
- Scans for `Invoke-Expression` with variables (command injection)
- Detects hardcoded credentials and insecure SecureString usage
- Identifies unsafe file operations without confirmation
- Checks for insecure web requests (HTTP vs HTTPS)
- Validates deserialization security (Import-Clixml, ConvertFrom-Json)
- Reports vulnerability severity (CRITICAL, HIGH, MEDIUM, LOW)
- Execution policy verification

### Manager Office
**Staff:** Manager Agent, Workflow Orchestration Agent, Resource Management Agent  
**Responsibilities:**
- Task delegation and priority management
- Workflow orchestration across agents
- Resource utilization optimization
- Job scheduling and queue management
- Agent coordination and communication

**Key Agent: Manager Agent**
- Delegates tasks to appropriate agents based on office
- Manages workflow execution and dependencies
- Monitors resource utilization
- Implements priority scheduling
- Coordinates cross-office operations
- Reports workflow status and metrics

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
  "floorNumber": 18,
  "language": "powershell",
  "domain": "Windows automation, System administration, DevOps scripting",
  "offices": ["Architecture Office", "Implementation Office", "Review Office", "Test Office", "Security Office", "Manager Office"],
  "agentCount": 6,
  "taskCount": 0,
  "agents": [...],
  "tasks": [...]
}
```

#### `process_code`
Process PowerShell code with various operations.

**Operations:**
- `analyze`: Analyze code structure (functions, classes, parameters)
- `quality`: Check code quality and best practices
- `security`: Scan for security vulnerabilities
- `test_analysis`: Analyze Pester test structure

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "function Get-User { [CmdletBinding()] param([string]$Name) }",
    "operation": "analyze"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "analysis": {
    "lines": 1,
    "functions": 1,
    "classes": 0,
    "cmdletBindings": 1,
    "parameters": 0,
    "language": "powershell"
  }
}
```

#### `execute_service`
Execute service agent operations.

**Services:**
- `validate_style`: Validate PowerShell style conventions
- `check_cmdlets`: Analyze cmdlet usage
- `analyze_security`: Security vulnerability scan

**Request:**
```json
{
  "method": "execute_service",
  "params": {
    "serviceName": "validate_style",
    "code": "function Get-Data { }"
  }
}
```

#### `process_data`
Process data with data model agent.

**Operations:**
- `validate`: Validate data structure
- `transform`: Transform data format (hashtable to PascalCase)
- `infer_type`: Infer .NET type information

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
- **PowerShell 7.0+** (PowerShell Core)
- Windows, Linux, or macOS

### Installation

**Windows:**
```powershell
# Install PowerShell 7+ if not already installed
winget install Microsoft.PowerShell

# Verify installation
pwsh --version
```

**Linux/macOS:**
```bash
# Install PowerShell 7+ via package manager
# Ubuntu/Debian
sudo apt install powershell

# macOS
brew install powershell/tap/powershell

# Verify installation
pwsh --version
```

### Running the Floor

**Interactive Mode:**
```powershell
pwsh -File floors/powershell/department_floor.ps1
```

**JSON-RPC Communication:**
```bash
echo '{"method":"get_info","params":{}}' | pwsh -File floors/powershell/department_floor.ps1
```

**From Pipeline:**
```powershell
# PowerShell pipeline example
@'
{"method":"get_info","params":{}}
'@ | pwsh -File floors/powershell/department_floor.ps1
```

### Testing

```powershell
# Test basic functionality
$request = @{method='get_info'; params=@{}} | ConvertTo-Json -Compress
$request | pwsh -File floors/powershell/department_floor.ps1

# Test code analysis
$request = @{
    method='process_code'
    params=@{
        code='function Get-User { [CmdletBinding()] param([string]$Name) }'
        operation='analyze'
    }
} | ConvertTo-Json -Compress
$request | pwsh -File floors/powershell/department_floor.ps1
```

## Security Considerations

### Execution Policy
This script requires appropriate execution policy settings:

```powershell
# Check current policy
Get-ExecutionPolicy

# Set policy for current user (recommended for development)
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# For production, use signed scripts
Set-ExecutionPolicy -ExecutionPolicy AllSigned -Scope LocalMachine
```

### Running in Production

1. **Code Signing**: Sign the script with a trusted certificate
2. **Constrained Language Mode**: Run untrusted scripts in constrained mode
3. **JEA Configuration**: Use Just Enough Administration for privilege separation
4. **Audit Logging**: Enable PowerShell transcript logging
5. **Module Whitelisting**: Use AppLocker or WDAC to control script execution

### Security Scanning

The Security Agent performs comprehensive security scanning:

```powershell
$request = @{
    method='process_code'
    params=@{
        code='Invoke-Expression $userInput'
        operation='security'
    }
} | ConvertTo-Json -Compress
$request | pwsh -File floors/powershell/department_floor.ps1

# Output includes:
# - Command injection vulnerabilities
# - Credential exposure risks
# - Unsafe operations
# - Insecure connections
# - Unsafe deserialization
```

## Design Patterns

### Pipeline Support
```powershell
function Get-ProcessedData {
    [CmdletBinding()]
    param(
        [Parameter(ValueFromPipeline=$true)]
        [string]$InputObject
    )
    
    process {
        # Process each pipeline object
        $InputObject.ToUpper()
    }
}
```

### Error Handling
```powershell
function Invoke-SafeOperation {
    [CmdletBinding()]
    param([string]$Path)
    
    try {
        $ErrorActionPreference = 'Stop'
        # Risky operation
        Get-Content -Path $Path
    }
    catch [System.IO.FileNotFoundException] {
        Write-Error "File not found: $Path"
        throw
    }
    catch {
        Write-Error "Unexpected error: $_"
        throw
    }
}
```

### Credential Handling
```powershell
# GOOD: Using SecureString
$securePassword = Read-Host -AsSecureString -Prompt "Password"
$credential = New-Object System.Management.Automation.PSCredential("username", $securePassword)

# BAD: Never hardcode credentials
# $password = "hardcoded123"  # NEVER DO THIS
```

## Performance Considerations

- Use pipeline processing for large datasets
- Prefer `ForEach-Object` over foreach loops for pipeline efficiency
- Use `StringBuilder` for string concatenation in loops
- Cache expensive operations (WMI queries, Active Directory lookups)
- Use `-Filter` parameter instead of `Where-Object` for Active Directory cmdlets
- Dispose of objects that implement IDisposable
- Use `Measure-Command` for performance profiling

## Integration Examples

### CI/CD Integration
```yaml
# Azure DevOps Pipeline
steps:
  - task: PowerShell@2
    inputs:
      filePath: 'floors/powershell/department_floor.ps1'
      arguments: ''
      pwsh: true
```

### Docker Support
```dockerfile
FROM mcr.microsoft.com/powershell:7.4-ubuntu-22.04
WORKDIR /app
COPY floors/powershell/department_floor.ps1 .
CMD ["pwsh", "-File", "department_floor.ps1"]
```

## Troubleshooting

### Common Issues

**Issue: Execution Policy Error**
```
Solution: Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

**Issue: JSON Parsing Error**
```
Solution: Ensure JSON is valid and properly escaped
```

**Issue: Missing PowerShell 7**
```
Solution: Install PowerShell 7+ from https://github.com/PowerShell/PowerShell
```

## References

- [PowerShell Documentation](https://docs.microsoft.com/powershell/)
- [PowerShell Security Best Practices](https://docs.microsoft.com/powershell/scripting/security/security-overview)
- [Pester Testing Framework](https://pester.dev/)
- [PSScriptAnalyzer](https://github.com/PowerShell/PSScriptAnalyzer)
- [PowerShell Approved Verbs](https://docs.microsoft.com/powershell/scripting/developer/cmdlet/approved-verbs-for-windows-powershell-commands)
