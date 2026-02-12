#!/usr/bin/env pwsh
<#
.SYNOPSIS
    FLOOR 18 - POWERSHELL JURISDICTION
    Department Floor Implementation

.DESCRIPTION
    Domain: Windows automation, System administration, DevOps scripting
    Architectural Law: Verb-Noun naming, Pipeline-first design, Type safety
    Security Doctrine: Command injection prevention, Credential handling, Execution policy compliance
#>

#Requires -Version 7.0

using namespace System.Collections.Generic
using namespace System.Management.Automation

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# ============================================================================
# CORE DATA MODELS
# ============================================================================

class FloorAgent {
    [string]$AgentId
    [string]$Name
    [string]$Role
    [string]$Office
    [string[]]$Capabilities

    FloorAgent([string]$agentId, [string]$name, [string]$role, [string]$office, [string[]]$capabilities) {
        $this.AgentId = $agentId
        $this.Name = $name
        $this.Role = $role
        $this.Office = $office
        $this.Capabilities = $capabilities
    }

    [hashtable] ToHashtable() {
        return @{
            agentId = $this.AgentId
            name = $this.Name
            role = $this.Role
            office = $this.Office
            capabilities = $this.Capabilities
        }
    }
}

class Task {
    [string]$TaskId
    [string]$Title
    [string]$Status
    [string]$AssignedTo
    [datetime]$CreatedAt
    [nullable[datetime]]$CompletedAt

    Task([string]$taskId, [string]$title, [string]$status, [string]$assignedTo) {
        $this.TaskId = $taskId
        $this.Title = $title
        $this.Status = $status
        $this.AssignedTo = $assignedTo
        $this.CreatedAt = [datetime]::UtcNow
        $this.CompletedAt = $null
    }

    [hashtable] ToHashtable() {
        return @{
            taskId = $this.TaskId
            title = $this.Title
            status = $this.Status
            assignedTo = $this.AssignedTo
            createdAt = $this.CreatedAt.ToString('o')
            completedAt = if ($this.CompletedAt) { $this.CompletedAt.ToString('o') } else { $null }
        }
    }
}

class CodeAnalysis {
    [int]$Lines
    [int]$Functions
    [int]$Classes
    [int]$CmdletBindings
    [int]$Parameters
    [string]$Language

    CodeAnalysis([int]$lines, [int]$functions, [int]$classes, [int]$cmdletBindings, [int]$parameters) {
        $this.Lines = $lines
        $this.Functions = $functions
        $this.Classes = $classes
        $this.CmdletBindings = $cmdletBindings
        $this.Parameters = $parameters
        $this.Language = 'powershell'
    }

    [hashtable] ToHashtable() {
        return @{
            lines = $this.Lines
            functions = $this.Functions
            classes = $this.Classes
            cmdletBindings = $this.CmdletBindings
            parameters = $this.Parameters
            language = $this.Language
        }
    }
}

# ============================================================================
# AGENT CLASSES - Production Grade Implementations
# ============================================================================

class ServiceAgent {
    <#
    .SYNOPSIS
        Service Agent - Handles Windows services, scheduled tasks, automation
        Office: Implementation Office
    #>
    
    [string]$AgentId
    [string]$Name
    [string[]]$Capabilities

    ServiceAgent([string]$agentId, [string]$name) {
        $this.AgentId = $agentId
        $this.Name = $name
        $this.Capabilities = @(
            'windows_services',
            'scheduled_tasks',
            'automation_workflow',
            'registry_operations'
        )
    }

    [hashtable] ExecuteService([string]$serviceName, [hashtable]$params) {
        $result = switch ($serviceName) {
            'validate_style' { $this.ValidateStyle($params) }
            'check_cmdlets' { $this.CheckCmdlets($params) }
            'analyze_security' { $this.AnalyzeSecurity($params) }
            default { throw "Unknown service: $serviceName" }
        }
        return $result
    }

    hidden [hashtable] ValidateStyle([hashtable]$params) {
        if (-not $params.ContainsKey('code')) {
            throw 'Code parameter required'
        }
        
        $code = $params['code']
        $issues = [List[string]]::new()
        
        # Check Verb-Noun naming convention
        if ($code -match 'function\s+([a-z]+)\s+\{' -and $code -notmatch 'function\s+[A-Z][a-z]+-[A-Z]') {
            $issues.Add('Use Verb-Noun naming convention for functions')
        }
        
        # Check for approved verbs
        if ($code -match 'function\s+[A-Z][a-z]+-[A-Z][a-z]+') {
            # Good - using approved verbs with proper Verb-Noun
        } elseif ($code -match 'function\s+[A-Z]') {
            $issues.Add('Use approved PowerShell verbs (Get, Set, New, Remove, etc.)')
        }
        
        # Check for CmdletBinding
        if ($code -match 'function\s+\w+' -and $code -notmatch '\[CmdletBinding\(\)\]') {
            $issues.Add('Advanced functions should use [CmdletBinding()]')
        }
        
        return @{
            valid = ($issues.Count -eq 0)
            issues = $issues.ToArray()
            issueCount = $issues.Count
        }
    }

    hidden [hashtable] CheckCmdlets([hashtable]$params) {
        if (-not $params.ContainsKey('code')) {
            throw 'Code parameter required'
        }
        
        $code = $params['code']
        
        # Extract cmdlet usage
        $cmdletPattern = '\b(Get|Set|New|Remove|Test|Invoke|Add|Clear|Copy|Move|Start|Stop|Wait|Write|Read)-[A-Z][a-zA-Z0-9]*\b'
        $matches = [regex]::Matches($code, $cmdletPattern)
        
        $cmdlets = [List[string]]::new()
        foreach ($match in $matches) {
            if (-not $cmdlets.Contains($match.Value)) {
                $cmdlets.Add($match.Value)
            }
        }
        
        return @{
            cmdletCount = $cmdlets.Count
            cmdlets = $cmdlets.ToArray()
            pipelineUsed = ($code -match '\|')
        }
    }

    hidden [hashtable] AnalyzeSecurity([hashtable]$params) {
        if (-not $params.ContainsKey('code')) {
            throw 'Code parameter required'
        }
        
        $code = $params['code']
        $vulnerabilities = [List[string]]::new()
        
        # Check for Invoke-Expression with variables (command injection risk)
        if ($code -match 'Invoke-Expression.*\$' -or $code -match 'iex\s+\$') {
            $vulnerabilities.Add('CRITICAL: Invoke-Expression with variables (command injection risk)')
        }
        
        # Check for Start-Process with user input
        if ($code -match 'Start-Process.*\$') {
            $vulnerabilities.Add('HIGH: Start-Process with variables may allow command injection')
        }
        
        # Check for hardcoded credentials
        if ($code -match 'password\s*=\s*[''"]' -or $code -match 'ConvertTo-SecureString.*-AsPlainText') {
            $vulnerabilities.Add('HIGH: Hardcoded credentials detected')
        }
        
        # Check for -Force without validation
        if ($code -match 'Remove-.*-Force' -and $code -notmatch '-Confirm') {
            $vulnerabilities.Add('MEDIUM: Destructive operation with -Force but no -Confirm')
        }
        
        return @{
            secure = ($vulnerabilities.Count -eq 0)
            vulnerabilities = $vulnerabilities.ToArray()
            vulnerabilityCount = $vulnerabilities.Count
        }
    }

    [FloorAgent] GetInfo() {
        return [FloorAgent]::new($this.AgentId, $this.Name, 'Service Agent', 'Implementation Office', $this.Capabilities)
    }
}

class DataModelAgent {
    <#
    .SYNOPSIS
        Data Model Agent - Manages PSCustomObject, hashtables, XML, JSON
        Office: Architecture Office
    #>
    
    [string]$AgentId
    [string]$Name
    [string[]]$Capabilities

    DataModelAgent([string]$agentId, [string]$name) {
        $this.AgentId = $agentId
        $this.Name = $name
        $this.Capabilities = @(
            'data_modeling',
            'type_validation',
            'serialization',
            'object_transformation'
        )
    }

    [hashtable] ProcessData([string]$operation, [object]$data) {
        $result = switch ($operation) {
            'validate' { $this.ValidateData($data) }
            'transform' { $this.TransformData($data) }
            'infer_type' { $this.InferType($data) }
            default { throw "Unknown operation: $operation" }
        }
        return $result
    }

    hidden [hashtable] ValidateData([object]$data) {
        if ($null -eq $data) {
            return @{
                valid = $false
                error = 'Data is null'
            }
        }
        
        $typeName = $data.GetType().Name
        $isHashtable = $data -is [hashtable]
        $isArray = $data -is [array]
        $isPSCustomObject = $data -is [PSCustomObject]
        
        return @{
            valid = $true
            type = $typeName
            isHashtable = $isHashtable
            isArray = $isArray
            isPSCustomObject = $isPSCustomObject
            properties = if ($isPSCustomObject) { ($data.PSObject.Properties.Name) } else { @() }
        }
    }

    hidden [hashtable] TransformData([object]$data) {
        if ($data -is [hashtable]) {
            $transformed = @{}
            foreach ($key in $data.Keys) {
                $pascalKey = $this.ToPascalCase($key)
                $transformed[$pascalKey] = $data[$key]
            }
            return @{
                transformed = $transformed
                changesMade = $true
            }
        }
        
        return @{
            transformed = $data
            changesMade = $false
        }
    }

    hidden [hashtable] InferType([object]$data) {
        if ($null -eq $data) {
            return @{ inferredType = 'null' }
        }
        
        $type = $data.GetType()
        return @{
            inferredType = $type.Name
            fullName = $type.FullName
            isValueType = $type.IsValueType
            isClass = $type.IsClass
        }
    }

    hidden [string] ToPascalCase([string]$text) {
        $words = $text -split '_'
        $pascalCase = ($words | ForEach-Object { 
            $_.Substring(0,1).ToUpper() + $_.Substring(1).ToLower() 
        }) -join ''
        return $pascalCase
    }

    [FloorAgent] GetInfo() {
        return [FloorAgent]::new($this.AgentId, $this.Name, 'Data Model Agent', 'Architecture Office', $this.Capabilities)
    }
}

class OperationsAgent {
    <#
    .SYNOPSIS
        Operations Agent - Code analysis, quality checks, PowerShell best practices
        Office: Review Office
    #>
    
    [string]$AgentId
    [string]$Name
    [string[]]$Capabilities

    OperationsAgent([string]$agentId, [string]$name) {
        $this.AgentId = $agentId
        $this.Name = $name
        $this.Capabilities = @(
            'code_analysis',
            'quality_metrics',
            'best_practices',
            'performance_review'
        )
    }

    [CodeAnalysis] AnalyzeCode([string]$code) {
        $lines = ($code -split "`n").Count
        
        # Count functions
        $functionMatches = [regex]::Matches($code, '\bfunction\s+\w+')
        $functions = $functionMatches.Count
        
        # Count classes
        $classMatches = [regex]::Matches($code, '\bclass\s+\w+')
        $classes = $classMatches.Count
        
        # Count CmdletBinding
        $cmdletBindingMatches = [regex]::Matches($code, '\[CmdletBinding\(\)\]')
        $cmdletBindings = $cmdletBindingMatches.Count
        
        # Count parameters
        $paramMatches = [regex]::Matches($code, '\[Parameter\(')
        $parameters = $paramMatches.Count
        
        return [CodeAnalysis]::new($lines, $functions, $classes, $cmdletBindings, $parameters)
    }

    [hashtable] CheckQuality([string]$code) {
        $issues = [List[string]]::new()
        $score = 100
        
        # Check for error handling
        if ($code -notmatch 'try\s*\{' -and $code -notmatch '\$ErrorActionPreference') {
            $issues.Add('Missing error handling (try/catch or $ErrorActionPreference)')
            $score -= 15
        }
        
        # Check for comment-based help
        if ($code -match 'function\s+\w+' -and $code -notmatch '\.SYNOPSIS') {
            $issues.Add('Missing comment-based help (.SYNOPSIS, .DESCRIPTION, etc.)')
            $score -= 10
        }
        
        # Check for StrictMode
        if ($code -notmatch 'Set-StrictMode') {
            $issues.Add('Missing Set-StrictMode declaration')
            $score -= 10
        }
        
        # Check for pipeline support
        if ($code -match 'function\s+\w+' -and $code -notmatch '\[Parameter\([^\)]*ValueFromPipeline') {
            $issues.Add('Functions should support pipeline input where appropriate')
            $score -= 5
        }
        
        return @{
            qualityScore = [Math]::Max(0, $score)
            issues = $issues.ToArray()
            issueCount = $issues.Count
        }
    }

    [FloorAgent] GetInfo() {
        return [FloorAgent]::new($this.AgentId, $this.Name, 'Operations Agent', 'Review Office', $this.Capabilities)
    }
}

class TestAgent {
    <#
    .SYNOPSIS
        Test Agent - Manages Pester tests, code coverage, test automation
        Office: Test Office
    #>
    
    [string]$AgentId
    [string]$Name
    [string[]]$Capabilities

    TestAgent([string]$agentId, [string]$name) {
        $this.AgentId = $agentId
        $this.Name = $name
        $this.Capabilities = @(
            'pester_testing',
            'code_coverage',
            'integration_tests',
            'mock_objects'
        )
    }

    [hashtable] AnalyzeTests([string]$code) {
        # Count Pester test blocks
        $describeMatches = [regex]::Matches($code, '\bDescribe\s+')
        $describes = $describeMatches.Count
        
        $contextMatches = [regex]::Matches($code, '\bContext\s+')
        $contexts = $contextMatches.Count
        
        $itMatches = [regex]::Matches($code, '\bIt\s+')
        $tests = $itMatches.Count
        
        # Count assertions
        $shouldMatches = [regex]::Matches($code, '\|\s*Should\s+')
        $assertions = $shouldMatches.Count
        
        # Count mocks
        $mockMatches = [regex]::Matches($code, '\bMock\s+')
        $mocks = $mockMatches.Count
        
        return @{
            describeBlocks = $describes
            contextBlocks = $contexts
            testCount = $tests
            assertionCount = $assertions
            mockCount = $mocks
            isPesterTest = ($describes -gt 0 -or $tests -gt 0)
        }
    }

    [FloorAgent] GetInfo() {
        return [FloorAgent]::new($this.AgentId, $this.Name, 'Test Agent', 'Test Office', $this.Capabilities)
    }
}

class SecurityAgent {
    <#
    .SYNOPSIS
        Security Agent - Security vulnerability scanning, credential management
        Office: Security Office
    #>
    
    [string]$AgentId
    [string]$Name
    [string[]]$Capabilities

    SecurityAgent([string]$agentId, [string]$name) {
        $this.AgentId = $agentId
        $this.Name = $name
        $this.Capabilities = @(
            'vulnerability_scanning',
            'credential_security',
            'injection_detection',
            'execution_policy_compliance'
        )
    }

    [hashtable] ScanSecurity([string]$code) {
        $vulnerabilities = [List[hashtable]]::new()
        
        # Command injection checks
        if ($code -match 'Invoke-Expression\s+\$' -or $code -match 'iex\s+\$') {
            $vulnerabilities.Add(@{
                severity = 'CRITICAL'
                type = 'command_injection'
                description = 'Invoke-Expression with variable input allows command injection'
                line = 0
            })
        }
        
        # Credential exposure
        if ($code -match 'ConvertTo-SecureString.*-AsPlainText.*-Force') {
            $vulnerabilities.Add(@{
                severity = 'HIGH'
                type = 'credential_exposure'
                description = 'Hardcoded credentials using ConvertTo-SecureString'
                line = 0
            })
        }
        
        # Unsafe file operations
        if ($code -match 'Remove-Item.*-Recurse.*-Force' -and $code -notmatch '-WhatIf|-Confirm') {
            $vulnerabilities.Add(@{
                severity = 'HIGH'
                type = 'unsafe_operation'
                description = 'Recursive deletion without confirmation'
                line = 0
            })
        }
        
        # Unsafe web requests
        if ($code -match 'Invoke-WebRequest.*-Uri\s+\$' -and $code -notmatch 'https://') {
            $vulnerabilities.Add(@{
                severity = 'MEDIUM'
                type = 'insecure_connection'
                description = 'Web request may use insecure HTTP'
                line = 0
            })
        }
        
        # Unsafe deserialization
        if ($code -match 'Import-Clixml\s+\$' -or $code -match 'ConvertFrom-Json.*\$') {
            $vulnerabilities.Add(@{
                severity = 'MEDIUM'
                type = 'unsafe_deserialization'
                description = 'Deserializing untrusted data without validation'
                line = 0
            })
        }
        
        return @{
            secure = ($vulnerabilities.Count -eq 0)
            vulnerabilities = $vulnerabilities.ToArray()
            vulnerabilityCount = $vulnerabilities.Count
        }
    }

    [FloorAgent] GetInfo() {
        return [FloorAgent]::new($this.AgentId, $this.Name, 'Security Agent', 'Security Office', $this.Capabilities)
    }
}

class ManagerAgent {
    <#
    .SYNOPSIS
        Manager Agent - Task delegation, workflow orchestration, resource management
        Office: Manager Office
    #>
    
    [string]$AgentId
    [string]$Name
    [string[]]$Capabilities

    ManagerAgent([string]$agentId, [string]$name) {
        $this.AgentId = $agentId
        $this.Name = $name
        $this.Capabilities = @(
            'task_delegation',
            'workflow_orchestration',
            'resource_management',
            'priority_scheduling'
        )
    }

    [hashtable] ManageWorkflow([hashtable]$params) {
        $taskCount = if ($params.ContainsKey('taskCount')) { $params['taskCount'] } else { 0 }
        
        return @{
            workflowStatus = 'active'
            tasksManaged = $taskCount
            priorityQueue = @()
            resourceUtilization = 'optimal'
        }
    }

    [FloorAgent] GetInfo() {
        return [FloorAgent]::new($this.AgentId, $this.Name, 'Manager Agent', 'Manager Office', $this.Capabilities)
    }
}

# ============================================================================
# MAIN DEPARTMENT FLOOR CLASS
# ============================================================================

class PowerShellDepartmentFloor {
    <#
    .SYNOPSIS
        PowerShell Department Floor - Floor 18
        
    .DESCRIPTION
        Implements the department logic for PowerShell jurisdiction following:
        - Language Sovereignty
        - Identical Internal Topology (6 offices)
        - Contract-Bound Operation
        - Non-Creative Mandate
        - Failure Escalation Guarantee
    #>
    
    [int]$FloorNumber
    [string]$Language
    [string]$Domain
    [hashtable]$Agents
    [hashtable]$Tasks
    [string[]]$Offices
    
    # Agent instances
    hidden [ServiceAgent]$ServiceAgent
    hidden [DataModelAgent]$DataModelAgent
    hidden [OperationsAgent]$OperationsAgent
    hidden [TestAgent]$TestAgent
    hidden [SecurityAgent]$SecurityAgent
    hidden [ManagerAgent]$ManagerAgent

    PowerShellDepartmentFloor() {
        $this.FloorNumber = 18
        $this.Language = 'powershell'
        $this.Domain = 'Windows automation, System administration, DevOps scripting'
        $this.Agents = @{}
        $this.Tasks = @{}
        $this.Offices = @(
            'Architecture Office',
            'Implementation Office',
            'Review Office',
            'Test Office',
            'Security Office',
            'Manager Office'
        )
        
        # Initialize agents
        $this.ServiceAgent = [ServiceAgent]::new('service-001', 'WindowsServiceAgent')
        $this.DataModelAgent = [DataModelAgent]::new('data-001', 'DataTransformAgent')
        $this.OperationsAgent = [OperationsAgent]::new('ops-001', 'CodeQualityAgent')
        $this.TestAgent = [TestAgent]::new('test-001', 'PesterTestAgent')
        $this.SecurityAgent = [SecurityAgent]::new('sec-001', 'SecurityScanAgent')
        $this.ManagerAgent = [ManagerAgent]::new('mgr-001', 'WorkflowManager')
        
        # Register agents
        $this.RegisterAgent($this.ServiceAgent.GetInfo())
        $this.RegisterAgent($this.DataModelAgent.GetInfo())
        $this.RegisterAgent($this.OperationsAgent.GetInfo())
        $this.RegisterAgent($this.TestAgent.GetInfo())
        $this.RegisterAgent($this.SecurityAgent.GetInfo())
        $this.RegisterAgent($this.ManagerAgent.GetInfo())
    }

    hidden [void] RegisterAgent([FloorAgent]$agent) {
        $this.Agents[$agent.AgentId] = $agent
    }

    [hashtable] AddAgent([string]$agentId, [string]$name, [string]$role, [string]$office, [string[]]$capabilities) {
        try {
            $agent = [FloorAgent]::new($agentId, $name, $role, $office, $capabilities)
            $this.Agents[$agentId] = $agent
            
            return @{
                status = 'success'
                agent = $agent.ToHashtable()
            }
        }
        catch {
            return @{
                status = 'error'
                message = $_.Exception.Message
            }
        }
    }

    [hashtable] CreateTask([string]$taskId, [string]$title, [string]$assignedTo) {
        try {
            $task = [Task]::new($taskId, $title, 'pending', $assignedTo)
            $this.Tasks[$taskId] = $task
            
            return @{
                status = 'success'
                task = $task.ToHashtable()
            }
        }
        catch {
            return @{
                status = 'error'
                message = $_.Exception.Message
            }
        }
    }

    [hashtable] GetFloorInfo() {
        $agentList = [List[hashtable]]::new()
        foreach ($agent in $this.Agents.Values) {
            $agentList.Add($agent.ToHashtable())
        }
        
        $taskList = [List[hashtable]]::new()
        foreach ($task in $this.Tasks.Values) {
            $taskList.Add($task.ToHashtable())
        }
        
        return @{
            floorNumber = $this.FloorNumber
            language = $this.Language
            domain = $this.Domain
            offices = $this.Offices
            agentCount = $this.Agents.Count
            taskCount = $this.Tasks.Count
            agents = $agentList.ToArray()
            tasks = $taskList.ToArray()
        }
    }

    [hashtable] ProcessCode([string]$code, [string]$operation) {
        try {
            $result = switch ($operation) {
                'analyze' {
                    $analysis = $this.OperationsAgent.AnalyzeCode($code)
                    @{
                        status = 'success'
                        analysis = $analysis.ToHashtable()
                    }
                }
                'quality' {
                    $quality = $this.OperationsAgent.CheckQuality($code)
                    @{
                        status = 'success'
                        quality = $quality
                    }
                }
                'security' {
                    $security = $this.SecurityAgent.ScanSecurity($code)
                    @{
                        status = 'success'
                        security = $security
                    }
                }
                'test_analysis' {
                    $testAnalysis = $this.TestAgent.AnalyzeTests($code)
                    @{
                        status = 'success'
                        testAnalysis = $testAnalysis
                    }
                }
                default {
                    @{
                        status = 'error'
                        message = "Unknown operation: $operation"
                    }
                }
            }
            return $result
        }
        catch {
            return @{
                status = 'error'
                message = $_.Exception.Message
            }
        }
    }

    [hashtable] HandleRequest([hashtable]$request) {
        try {
            $method = $request['method']
            $params = if ($request.ContainsKey('params')) { $request['params'] } else { @{} }
            
            $result = switch ($method) {
                'get_info' {
                    $this.GetFloorInfo()
                }
                'add_agent' {
                    $this.AddAgent(
                        $params['agentId'],
                        $params['name'],
                        $params['role'],
                        $params['office'],
                        $params['capabilities']
                    )
                }
                'create_task' {
                    $this.CreateTask(
                        $params['taskId'],
                        $params['title'],
                        $params['assignedTo']
                    )
                }
                'process_code' {
                    $this.ProcessCode($params['code'], $params['operation'])
                }
                'execute_service' {
                    $this.ServiceAgent.ExecuteService($params['serviceName'], $params)
                }
                'process_data' {
                    $this.DataModelAgent.ProcessData($params['operation'], $params['data'])
                }
                default {
                    @{
                        status = 'error'
                        message = "Unknown method: $method"
                    }
                }
            }
            return $result
        }
        catch {
            return @{
                status = 'error'
                message = $_.Exception.Message
                stackTrace = $_.ScriptStackTrace
            }
        }
    }
}

# ============================================================================
# MAIN ENTRY POINT - JSON-RPC SERVER
# ============================================================================

function Start-FloorServer {
    <#
    .SYNOPSIS
        Main entry point - JSON-RPC server over stdin/stdout
    #>
    
    $floor = [PowerShellDepartmentFloor]::new()
    
    [Console]::Error.WriteLine("PowerShell Department Floor (Floor 18) - Ready")
    [Console]::Error.WriteLine("Domain: $($floor.Domain)")
    [Console]::Error.WriteLine("Offices: $($floor.Offices -join ', ')")
    
    # Process requests line by line from stdin
    while ($null -ne ($line = [Console]::In.ReadLine())) {
        try {
            $request = $line | ConvertFrom-Json -AsHashtable
            $response = $floor.HandleRequest($request)
            $jsonResponse = $response | ConvertTo-Json -Depth 10 -Compress
            [Console]::Out.WriteLine($jsonResponse)
            [Console]::Out.Flush()
        }
        catch {
            $errorResponse = @{
                status = 'error'
                message = $_.Exception.Message
            }
            $jsonError = $errorResponse | ConvertTo-Json -Compress
            [Console]::Out.WriteLine($jsonError)
            [Console]::Out.Flush()
        }
    }
}

# Start the server
Start-FloorServer
