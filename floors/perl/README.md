# Floor 17 - Perl Jurisdiction

**Language:** Perl 5  
**Floor Number:** 17  
**Domain:** System scripting, Text processing, Legacy systems  
**Architectural Law:** TIMTOWTDI with best practices, Strict mode mandatory, Use warnings  
**Security Doctrine:** Taint mode, Command injection prevention, Input validation

## Overview

The Perl Department Floor specializes in system scripting, text processing, and legacy system integration. This floor embodies Perl's philosophy of "There's More Than One Way To Do It" (TIMTOWTDI) while enforcing modern best practices, security, and maintainability.

## Architecture

This floor implements department logic following core principles:

- **Language Sovereignty**: Pure Perl 5 implementation with modern practices
- **Identical Topology**: 6 offices with specialized roles
- **Contract-Bound Operation**: JSON-RPC protocol via stdin/stdout
- **Non-Creative Mandate**: Strict adherence to Perl best practices
- **Failure Escalation**: Explicit error reporting and security warnings

## Domain & Jurisdiction

The Perl floor has exclusive authority over:

1. **System Scripting**
   - Process management
   - File operations
   - System integration
   - Shell interaction

2. **Text Processing**
   - Regular expression analysis
   - Pattern matching
   - Text parsing
   - Stream processing

3. **Legacy Systems**
   - CGI applications
   - System administration scripts
   - Log processing
   - Data extraction

4. **Security & Validation**
   - Taint mode enforcement
   - Command injection prevention
   - Input validation
   - Secure file operations

## Architectural Laws

### 1. TIMTOWTDI with Best Practices
Perl philosophy: "There's More Than One Way To Do It" - but we enforce modern best practices:
- Use `strict` and `warnings` - mandatory
- Three-argument `open()` - always
- Lexical file handles - preferred
- Modern Perl idioms - encouraged

### 2. Strict Mode Mandatory
All Perl code MUST include:
```perl
use strict;
use warnings;
```

This is non-negotiable for safety and maintainability.

### 3. Use Warnings
Enable warnings to catch common errors:
- Undefined values
- Deprecated features
- Potential problems
- Best practice violations

## Security Doctrine

### Core Principles
1. **Enable taint mode** - Use `-T` flag for all scripts handling external input
2. **Prevent command injection** - Never pass user input to system(), exec(), or backticks
3. **Validate all input** - Sanitize and validate before use
4. **Use three-argument open** - Prevents injection through filenames
5. **Avoid eval with user input** - Code injection risk

### Security Levels
- **SAFE**: No vulnerabilities, proper validation, taint mode enabled
- **WARNING**: Potential issues (two-argument open, missing taint mode)
- **CRITICAL**: Command/code injection vulnerabilities detected

### Common Vulnerabilities
- **Command Injection**: Passing variables to system(), exec(), backticks
- **Code Injection**: Using eval() with user input
- **Path Traversal**: Unvalidated file paths in open(), unlink()
- **ReDoS**: Regular expressions with catastrophic backtracking

## Office Structure

### Architecture Office
**Staff:** 3 Agents

1. **Script Architect** (Senior Architect)
   - Capabilities: script_design, module_architecture, api_design
   - Specialization: Script Architecture
   
2. **Systems Architect** (Architect)
   - Capabilities: system_integration, automation_design, workflow_design
   - Specialization: System Integration
   
3. **Text Processing Architect** (Architect)
   - Capabilities: regex_design, parser_design, text_pipeline
   - Specialization: Text Processing

**Responsibilities:**
- Script architecture design
- Module structure planning
- System integration design
- Text processing pipelines

### Implementation Office
**Staff:** 3 Agents

1. **Script Engineer** (Senior Engineer)
   - Capabilities: script_development, module_creation, cpan_integration
   - Specialization: Script Development
   
2. **Regex Engineer** (Engineer)
   - Capabilities: regex_implementation, text_parsing, pattern_matching
   - Specialization: Regex & Text Processing
   
3. **System Integration Engineer** (Engineer)
   - Capabilities: system_calls, process_management, ipc
   - Specialization: System Integration

**Responsibilities:**
- Script implementation
- Regular expression development
- System call integration
- CPAN module integration

### Review Office
**Staff:** 3 Agents

1. **Senior Code Reviewer** (Lead Reviewer)
   - Capabilities: code_review, best_practices, idiom_enforcement
   - Specialization: Comprehensive Review
   
2. **Perl Critic** (Reviewer)
   - Capabilities: static_analysis, policy_enforcement, style_checking
   - Specialization: Static Analysis
   
3. **Performance Reviewer** (Reviewer)
   - Capabilities: performance_analysis, optimization_review
   - Specialization: Performance Review

**Responsibilities:**
- Code quality review
- Perl::Critic policy enforcement
- Best practices validation
- Performance analysis

### Test Office
**Staff:** 3 Agents

1. **Test Engineer** (Senior Tester)
   - Capabilities: unit_testing, integration_testing, test_more
   - Specialization: Testing
   
2. **TAP Specialist** (Tester)
   - Capabilities: tap_protocol, test_harness, prove
   - Specialization: TAP Testing
   
3. **Coverage Analyst** (Tester)
   - Capabilities: coverage_analysis, devel_cover, quality_metrics
   - Specialization: Code Coverage

**Responsibilities:**
- Unit testing (Test::More)
- TAP protocol compliance
- Test coverage analysis
- Integration testing

### Security Office
**Staff:** 3 Agents

1. **Security Auditor** (Senior Security Engineer)
   - Capabilities: security_audit, vulnerability_detection, taint_mode
   - Specialization: Security Auditing
   
2. **Injection Prevention Specialist** (Security Engineer)
   - Capabilities: command_injection, code_injection, input_validation
   - Specialization: Injection Prevention
   
3. **Cryptography Specialist** (Security Engineer)
   - Capabilities: encryption, hashing, secure_random
   - Specialization: Cryptography

**Responsibilities:**
- Security vulnerability detection
- Command/code injection prevention
- Taint mode enforcement
- Secure coding practices

### Manager Office
**Staff:** 3 Agents

1. **Perl Project Manager** (Senior Manager)
   - Capabilities: project_management, resource_allocation, cpan_publishing
   - Specialization: Project Management
   
2. **Automation Manager** (Manager)
   - Capabilities: automation_strategy, workflow_optimization
   - Specialization: Automation
   
3. **Quality Manager** (Manager)
   - Capabilities: quality_assurance, standards_enforcement
   - Specialization: Quality Assurance

**Responsibilities:**
- Project coordination
- Resource allocation
- CPAN publishing coordination
- Quality assurance

## Specialist Agents

### Service Agent
Handles Perl service operations and pragmas.

**Capabilities:**
- Shebang validation
- Pragma analysis (strict, warnings, autodie)
- Process management

### Text Processing Agent
Analyzes regular expressions and text operations.

**Capabilities:**
- Regex complexity analysis
- ReDoS vulnerability detection
- Pattern optimization suggestions

### Security Agent
Security auditing and vulnerability detection.

**Capabilities:**
- Command injection detection
- Taint mode checking
- File operation security audit

## Installation

### Requirements
- Perl 5.10 or higher
- Core modules only (no CPAN dependencies)

### Setup
```bash
cd floors/perl
chmod +x department_floor.pl
```

All required modules are part of Perl core (JSON::PP, Data::Dumper, IO::Handle).

## Running

Start the Perl Department Floor:

```bash
perl department_floor.pl
```

Or make executable and run directly:

```bash
./department_floor.pl
```

The floor listens on stdin for JSON-RPC requests and writes responses to stdout.

## API Documentation

### Get Floor Information
```json
{"method": "get_info"}
```

**Response:**
```json
{
  "floor_number": 17,
  "language": "perl",
  "domain": "System scripting, Text processing, Legacy systems",
  "architectural_law": "TIMTOWTDI with best practices, Strict mode mandatory, Use warnings",
  "security_doctrine": "Taint mode, Command injection prevention, Input validation",
  "offices": [...],
  "agent_count": 18,
  "agents": [...],
  "specialist_agents": [...]
}
```

### Analyze Perl Script
Comprehensive Perl script analysis.

```json
{
  "method": "analyze_perl_script",
  "params": {
    "script": "#!/usr/bin/env perl\nuse strict;\nuse warnings;\n\nprint \"Hello, World!\\n\";",
    "analysis_type": "comprehensive"
  }
}
```

**Analysis Types:**
- `comprehensive`: Full analysis (default)
- `structure`: Shebang and pragma analysis
- `security`: Security audit only

**Response:**
```json
{
  "status": "success",
  "analysis": {
    "shebang": {
      "valid": true,
      "shebang": "#!/usr/bin/env perl",
      "uses_env": true
    },
    "pragmas": {
      "has_strict": true,
      "has_warnings": true,
      "has_autodie": false,
      "issues": [],
      "safe": true
    },
    "command_injection": {
      "security_level": "safe",
      "vulnerabilities": [],
      "safe": true
    }
  },
  "script_length": 58
}
```

### Analyze Regular Expression
Analyze regex patterns for complexity and vulnerabilities.

```json
{
  "method": "analyze_regex",
  "params": {
    "pattern": "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "regex_analysis": {
    "pattern": "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$",
    "complexity": "simple",
    "warnings": [],
    "has_named_captures": false,
    "modifiers": [],
    "safe": true
  },
  "optimization": {
    "suggestions": [],
    "suggestion_count": 0
  }
}
```

### Review Perl Script
Submit script for review by Review Office.

```json
{
  "method": "review_script",
  "params": {
    "script": "#!/usr/bin/perl\nsystem(\"ls $dir\");",
    "context": {}
  }
}
```

**Response:**
```json
{
  "status": "reviewed",
  "issues": [
    "Missing \"use strict\" - mandatory for safety",
    "Missing \"use warnings\" - mandatory for safety",
    "system() call with variable - command injection risk"
  ],
  "recommendations": [
    "Use three-argument open()",
    "Enable taint mode for security"
  ],
  "approved": false,
  "security_level": "critical"
}
```

### Security Audit
Perform comprehensive security audit.

```json
{
  "method": "security_audit",
  "params": {
    "script": "#!/usr/bin/env perl -T\nuse strict;\nuse warnings;\n\nopen(my $fh, '<', $file) or die;"
  }
}
```

**Response:**
```json
{
  "status": "audited",
  "security_level": "safe",
  "has_taint_mode": true,
  "vulnerabilities": [],
  "file_operation_findings": [],
  "passed_audit": true
}
```

### Test Perl Script
Test script through Test Office.

```json
{
  "method": "test_script",
  "params": {
    "script": "#!/usr/bin/env perl\nuse strict;\nuse warnings;\n\nprint \"Hello\\n\";",
    "test_data": null
  }
}
```

**Response:**
```json
{
  "status": "tested",
  "test_results": {
    "syntax_valid": true,
    "has_strict": true,
    "has_warnings": true,
    "passes_basic_checks": true
  },
  "all_tests_passed": true
}
```

### Create Task
Create a task on this floor.

```json
{
  "method": "create_task",
  "params": {
    "task_id": "perl_task_001",
    "title": "Optimize regex patterns",
    "task_type": "optimization",
    "assigned_to": "impl_002",
    "metadata": {"priority": "high"}
  }
}
```

## Usage Examples

### Example 1: Analyze a Perl Script
```bash
echo '{"method": "analyze_perl_script", "params": {"script": "#!/usr/bin/env perl\nuse strict;\nuse warnings;\n\nmy $name = \"World\";\nprint \"Hello, $name!\\n\";"}}' | perl department_floor.pl
```

### Example 2: Check for Command Injection
```bash
echo '{"method": "security_audit", "params": {"script": "#!/usr/bin/perl\nsystem(\"rm -rf $user_input\");"}}' | perl department_floor.pl
```

### Example 3: Analyze Regex Pattern
```bash
echo '{"method": "analyze_regex", "params": {"pattern": "(a+)+"}}' | perl department_floor.pl
```

### Example 4: Review Script
```bash
echo '{"method": "review_script", "params": {"script": "#!/usr/bin/env perl\nuse strict;\nuse warnings;\n\nopen(my $fh, \"<\", \"/etc/passwd\") or die;"}}' | perl department_floor.pl
```

## Best Practices

### 1. Always Use Strict and Warnings
```perl
#!/usr/bin/env perl
use strict;
use warnings;
# Your code here
```

### 2. Three-Argument Open
```perl
# Good - safe
open(my $fh, '<', $filename) or die "Cannot open: $!";

# Bad - vulnerable to injection
open(my $fh, $filename) or die "Cannot open: $!";
```

### 3. Enable Taint Mode
```perl
#!/usr/bin/env perl -T
use strict;
use warnings;

# Untaint validated input
if ($input =~ /^([\w.-]+)$/) {
    $safe_input = $1;
}
```

### 4. Avoid System Calls with Variables
```perl
# Bad - command injection
system("rm -rf $directory");

# Good - use list form
system('rm', '-rf', $directory);
```

### 5. Use Lexical File Handles
```perl
# Good - lexical file handle
open(my $fh, '<', $file) or die;

# Old style - avoid
open(FH, '<', $file) or die;
```

### 6. Validate Input
```perl
# Validate and untaint
unless ($input =~ /^[\w.-]+$/) {
    die "Invalid input: $input";
}
```

## Common Vulnerabilities

### Command Injection
```perl
# VULNERABLE
system("grep $pattern $file");
exec("ls $directory");
my $output = `cat $file`;

# SAFE
system('grep', $pattern, $file);
exec('ls', $directory);
open(my $fh, '<', $file) or die;
```

### Code Injection
```perl
# VULNERABLE
eval $user_code;
eval "package Foo; $code";

# SAFE - use string eval only for safe operations
# Avoid eval with user input entirely
```

### Path Traversal
```perl
# VULNERABLE
open(my $fh, '<', "/data/$user_file");
unlink("/tmp/$filename");

# SAFE - validate paths
if ($user_file =~ /^([\w.-]+)$/) {
    my $safe_file = $1;
    open(my $fh, '<', "/data/$safe_file") or die;
}
```

### ReDoS (Regular Expression DoS)
```perl
# VULNERABLE - catastrophic backtracking
/(a+)+/
/(a*)*b/

# SAFE - avoid nested quantifiers
/a+/
/a*b/
```

## Error Handling

All errors are returned with explicit status:

```json
{
  "status": "error",
  "message": "Detailed error description"
}
```

Security violations:
```json
{
  "security_level": "critical",
  "vulnerabilities": ["system() call with variable - command injection risk"]
}
```

## Testing

Perl floor supports TAP (Test Anything Protocol):

```perl
use Test::More tests => 3;

ok($result, 'Test passed');
is($actual, $expected, 'Values match');
like($output, qr/pattern/, 'Pattern matches');
```

## CPAN Integration

The floor works with CPAN modules:
- `use strict` - Variable safety
- `use warnings` - Warning messages
- `use autodie` - Automatic error handling
- `JSON::PP` - JSON processing (core module)
- `Test::More` - Testing framework

## Performance Tips

1. **Use compiled regex** - `qr//` for frequently used patterns
2. **Avoid unnecessary captures** - Use `(?:...)` for grouping
3. **Precompile patterns** - Store in variables
4. **Use lexical file handles** - Automatically cleaned up
5. **Enable buffering** - For large file operations

## Contributing

When extending Perl floor capabilities:

1. Maintain strict and warnings enforcement
2. Add security validation for all new operations
3. Follow modern Perl best practices
4. Update office staffing appropriately
5. Document all new methods
6. Include TAP tests

## License

Part of Thirsty's Projects Miniature Office - See main LICENSE file.
