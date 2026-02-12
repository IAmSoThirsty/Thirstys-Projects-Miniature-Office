#!/usr/bin/env node
/**
 * FLOOR 6 - TYPESCRIPT JURISDICTION
 * Department Floor Implementation
 * 
 * Domain: Typed frontend systems, Node.js backends, Type-safe applications
 * Architectural Law: Strict typing mandatory, no 'any' types, compile-time safety
 * Security Doctrine: Type guards, runtime validation, null safety
 */

import * as readline from 'readline';

// ============================================================================
// CORE DATA MODELS
// ============================================================================

interface FloorAgent {
    agentId: string;
    name: string;
    role: string;
    office: string;
    capabilities: string[];
}

interface Task {
    taskId: string;
    title: string;
    status: 'pending' | 'in_progress' | 'completed' | 'failed';
    assignedTo: string;
    createdAt: string;
    completedAt?: string;
}

interface CodeAnalysis {
    lines: number;
    interfaces: number;
    types: number;
    classes: number;
    functions: number;
    anyUsage: number;
    language: string;
}

interface JSONRPCRequest {
    method: string;
    params?: Record<string, unknown>;
    id?: string | number;
}

interface JSONRPCResponse {
    result?: unknown;
    error?: {
        code: number;
        message: string;
    };
    id?: string | number;
}

// ============================================================================
// AGENT CLASSES - Production Grade Implementations
// ============================================================================

class ServiceAgent {
    /**
     * Service Agent - Handles external integrations and I/O operations
     * Office: Implementation Office
     */
    private agentId: string;
    private name: string;
    private capabilities: string[];

    constructor(agentId: string, name: string) {
        this.agentId = agentId;
        this.name = name;
        this.capabilities = [
            'api_integration',
            'file_operations',
            'network_communication',
            'event_handling'
        ];
    }

    public executeService(serviceName: string, params: Record<string, unknown>): Record<string, unknown> {
        switch (serviceName) {
            case 'validate_types':
                return this.validateTypes(params);
            case 'check_imports':
                return this.checkImports(params);
            case 'analyze_dependencies':
                return this.analyzeDependencies(params);
            default:
                throw new Error(`Unknown service: ${serviceName}`);
        }
    }

    private validateTypes(params: Record<string, unknown>): Record<string, unknown> {
        const code = params.code as string;
        if (!code) throw new Error('Code parameter required');
        
        const anyCount = (code.match(/:\s*any\b/g) || []).length;
        const unknownCount = (code.match(/:\s*unknown\b/g) || []).length;
        
        return {
            valid: anyCount === 0,
            anyUsage: anyCount,
            unknownUsage: unknownCount,
            recommendation: anyCount > 0 ? 'Replace any with specific types' : 'Type usage acceptable'
        };
    }

    private checkImports(params: Record<string, unknown>): Record<string, unknown> {
        const code = params.code as string;
        if (!code) throw new Error('Code parameter required');
        
        const importMatches = code.match(/^import\s+.*from\s+['"].*['"];?$/gm) || [];
        const requireMatches = code.match(/require\(['"].*['"]\)/g) || [];
        
        return {
            esModules: importMatches.length,
            commonJS: requireMatches.length,
            imports: importMatches,
            mixed: importMatches.length > 0 && requireMatches.length > 0
        };
    }

    private analyzeDependencies(params: Record<string, unknown>): Record<string, unknown> {
        const code = params.code as string;
        if (!code) throw new Error('Code parameter required');
        
        const imports = code.match(/from\s+['"]([^'"]+)['"]/g) || [];
        const packages = imports.map(imp => imp.match(/['"]([^'"]+)['"]/)?.[1] || '');
        
        return {
            totalDependencies: packages.length,
            externalPackages: packages.filter(p => !p.startsWith('.')).length,
            localModules: packages.filter(p => p.startsWith('.')).length,
            packages: [...new Set(packages)]
        };
    }

    public getInfo(): FloorAgent {
        return {
            agentId: this.agentId,
            name: this.name,
            role: 'Service Agent',
            office: 'Implementation Office',
            capabilities: this.capabilities
        };
    }
}

class DataModelAgent {
    /**
     * Data Model Agent - Manages data structures, validation, and transformations
     * Office: Architecture Office
     */
    private agentId: string;
    private name: string;
    private capabilities: string[];

    constructor(agentId: string, name: string) {
        this.agentId = agentId;
        this.name = name;
        this.capabilities = [
            'schema_validation',
            'data_transformation',
            'type_inference',
            'model_generation'
        ];
    }

    public processData(operation: string, data: unknown): Record<string, unknown> {
        switch (operation) {
            case 'validate':
                return this.validateData(data);
            case 'transform':
                return this.transformData(data);
            case 'infer_types':
                return this.inferTypes(data);
            default:
                throw new Error(`Unknown operation: ${operation}`);
        }
    }

    private validateData(data: unknown): Record<string, unknown> {
        if (data === null || data === undefined) {
            return { valid: false, error: 'Data is null or undefined' };
        }
        
        const dataType = typeof data;
        const isObject = dataType === 'object';
        const isArray = Array.isArray(data);
        
        return {
            valid: true,
            type: isArray ? 'array' : dataType,
            isObject,
            isArray,
            keys: isObject && !isArray ? Object.keys(data as Record<string, unknown>) : []
        };
    }

    private transformData(data: unknown): Record<string, unknown> {
        if (typeof data !== 'object' || data === null) {
            return { transformed: data, changesMade: false };
        }
        
        const obj = data as Record<string, unknown>;
        const transformed: Record<string, unknown> = {};
        let changesMade = false;
        
        for (const [key, value] of Object.entries(obj)) {
            const camelKey = key.replace(/_([a-z])/g, (_, letter) => letter.toUpperCase());
            transformed[camelKey] = value;
            if (camelKey !== key) changesMade = true;
        }
        
        return { transformed, changesMade };
    }

    private inferTypes(data: unknown): Record<string, unknown> {
        if (data === null) return { type: 'null' };
        if (data === undefined) return { type: 'undefined' };
        
        const basicType = typeof data;
        
        if (basicType !== 'object') {
            return { type: basicType };
        }
        
        if (Array.isArray(data)) {
            const elementTypes = data.map(item => typeof item);
            const uniqueTypes = [...new Set(elementTypes)];
            return {
                type: 'array',
                elementTypes: uniqueTypes,
                length: data.length
            };
        }
        
        const obj = data as Record<string, unknown>;
        const schema: Record<string, string> = {};
        for (const [key, value] of Object.entries(obj)) {
            schema[key] = typeof value;
        }
        
        return {
            type: 'object',
            schema
        };
    }

    public getInfo(): FloorAgent {
        return {
            agentId: this.agentId,
            name: this.name,
            role: 'Data Model Agent',
            office: 'Architecture Office',
            capabilities: this.capabilities
        };
    }
}

class OperationsAgent {
    /**
     * Operations Agent - Handles code operations, analysis, and transformations
     * Office: Review Office
     */
    private agentId: string;
    private name: string;
    private capabilities: string[];

    constructor(agentId: string, name: string) {
        this.agentId = agentId;
        this.name = name;
        this.capabilities = [
            'code_analysis',
            'quality_checks',
            'optimization',
            'refactoring'
        ];
    }

    public executeOperation(operation: string, params: Record<string, unknown>): Record<string, unknown> {
        const code = params.code as string;
        if (!code) throw new Error('Code parameter required');

        switch (operation) {
            case 'analyze':
                return this.analyzeCode(code);
            case 'check_quality':
                return this.checkQuality(code);
            case 'find_issues':
                return this.findIssues(code);
            default:
                throw new Error(`Unknown operation: ${operation}`);
        }
    }

    private analyzeCode(code: string): Record<string, unknown> {
        const analysis: CodeAnalysis = {
            lines: code.split('\n').length,
            interfaces: (code.match(/interface\s+\w+/g) || []).length,
            types: (code.match(/type\s+\w+/g) || []).length,
            classes: (code.match(/class\s+\w+/g) || []).length,
            functions: (code.match(/function\s+\w+/g) || []).length,
            anyUsage: (code.match(/:\s*any\b/g) || []).length,
            language: 'typescript'
        };

        return { status: 'success', analysis };
    }

    private checkQuality(code: string): Record<string, unknown> {
        const issues: string[] = [];
        let score = 100;

        const anyCount = (code.match(/:\s*any\b/g) || []).length;
        if (anyCount > 0) {
            issues.push(`Found ${anyCount} usage(s) of 'any' type`);
            score -= anyCount * 10;
        }

        const consoleCount = (code.match(/console\.(log|warn|error)/g) || []).length;
        if (consoleCount > 0) {
            issues.push(`Found ${consoleCount} console statements`);
            score -= consoleCount * 5;
        }

        if (!code.includes('export')) {
            issues.push('No exports found - module may not be reusable');
            score -= 10;
        }

        return {
            score: Math.max(0, score),
            issues,
            passed: score >= 70
        };
    }

    private findIssues(code: string): Record<string, unknown> {
        const issues: Array<{ severity: string; message: string; line?: number }> = [];

        const lines = code.split('\n');
        lines.forEach((line, index) => {
            if (line.includes(': any')) {
                issues.push({
                    severity: 'error',
                    message: "Usage of 'any' type violates strict typing law",
                    line: index + 1
                });
            }
            if (line.match(/==\s*(?!==)/)) {
                issues.push({
                    severity: 'warning',
                    message: 'Use === instead of == for comparison',
                    line: index + 1
                });
            }
            if (line.includes('@ts-ignore')) {
                issues.push({
                    severity: 'error',
                    message: '@ts-ignore bypasses type safety',
                    line: index + 1
                });
            }
        });

        return {
            totalIssues: issues.length,
            issues,
            critical: issues.filter(i => i.severity === 'error').length
        };
    }

    public getInfo(): FloorAgent {
        return {
            agentId: this.agentId,
            name: this.name,
            role: 'Operations Agent',
            office: 'Review Office',
            capabilities: this.capabilities
        };
    }
}

class SecurityAgent {
    /**
     * Security Agent - Performs security analysis and validation
     * Office: Security Office
     */
    private agentId: string;
    private name: string;
    private capabilities: string[];

    constructor(agentId: string, name: string) {
        this.agentId = agentId;
        this.name = name;
        this.capabilities = [
            'vulnerability_scanning',
            'secure_coding_checks',
            'dependency_audit',
            'injection_detection'
        ];
    }

    public auditSecurity(code: string): Record<string, unknown> {
        const vulnerabilities: Array<{ type: string; severity: string; description: string }> = [];

        if (code.includes('eval(')) {
            vulnerabilities.push({
                type: 'code_injection',
                severity: 'critical',
                description: 'eval() usage detected - potential code injection risk'
            });
        }

        if (code.match(/innerHTML\s*=/)) {
            vulnerabilities.push({
                type: 'xss',
                severity: 'high',
                description: 'innerHTML assignment - potential XSS vulnerability'
            });
        }

        if (code.includes('dangerouslySetInnerHTML')) {
            vulnerabilities.push({
                type: 'xss',
                severity: 'high',
                description: 'dangerouslySetInnerHTML detected - ensure content is sanitized'
            });
        }

        const passwordRegex = /password.*=.*['"][^'"]*['"]/i;
        if (passwordRegex.test(code)) {
            vulnerabilities.push({
                type: 'hardcoded_secrets',
                severity: 'critical',
                description: 'Potential hardcoded password detected'
            });
        }

        if (code.includes('http://') && !code.includes('localhost')) {
            vulnerabilities.push({
                type: 'insecure_protocol',
                severity: 'medium',
                description: 'HTTP protocol detected - use HTTPS for production'
            });
        }

        return {
            secure: vulnerabilities.length === 0,
            vulnerabilities,
            criticalCount: vulnerabilities.filter(v => v.severity === 'critical').length,
            highCount: vulnerabilities.filter(v => v.severity === 'high').length
        };
    }

    public getInfo(): FloorAgent {
        return {
            agentId: this.agentId,
            name: this.name,
            role: 'Security Agent',
            office: 'Security Office',
            capabilities: this.capabilities
        };
    }
}

class TestAgent {
    /**
     * Test Agent - Handles test generation and validation
     * Office: Test Office
     */
    private agentId: string;
    private name: string;
    private capabilities: string[];

    constructor(agentId: string, name: string) {
        this.agentId = agentId;
        this.name = name;
        this.capabilities = [
            'test_generation',
            'coverage_analysis',
            'test_validation',
            'mock_generation'
        ];
    }

    public analyzeTests(code: string): Record<string, unknown> {
        const testFrameworks = {
            jest: code.includes('describe(') || code.includes('test('),
            mocha: code.includes('describe(') && code.includes('it('),
            jasmine: code.includes('describe(') && code.includes('beforeEach(')
        };

        const assertions = [
            ...code.matchAll(/expect\([^)]+\)\./g),
            ...code.matchAll(/assert\.[a-z]+\(/g)
        ];

        return {
            hasTests: Object.values(testFrameworks).some(v => v),
            frameworks: Object.entries(testFrameworks)
                .filter(([_, has]) => has)
                .map(([name, _]) => name),
            assertionCount: assertions.length,
            coverage: 'unknown'
        };
    }

    public getInfo(): FloorAgent {
        return {
            agentId: this.agentId,
            name: this.name,
            role: 'Test Agent',
            office: 'Test Office',
            capabilities: this.capabilities
        };
    }
}

class ManagerAgent {
    /**
     * Manager Agent - Coordinates tasks and manages workflow
     * Office: Manager Office
     */
    private agentId: string;
    private name: string;
    private capabilities: string[];
    private tasks: Map<string, Task>;

    constructor(agentId: string, name: string) {
        this.agentId = agentId;
        this.name = name;
        this.capabilities = [
            'task_coordination',
            'workflow_management',
            'resource_allocation',
            'progress_tracking'
        ];
        this.tasks = new Map();
    }

    public createTask(taskId: string, title: string, assignedTo: string): Task {
        const task: Task = {
            taskId,
            title,
            status: 'pending',
            assignedTo,
            createdAt: new Date().toISOString()
        };
        this.tasks.set(taskId, task);
        return task;
    }

    public updateTaskStatus(taskId: string, status: Task['status']): Task {
        const task = this.tasks.get(taskId);
        if (!task) throw new Error(`Task not found: ${taskId}`);
        
        task.status = status;
        if (status === 'completed') {
            task.completedAt = new Date().toISOString();
        }
        return task;
    }

    public getTasks(): Task[] {
        return Array.from(this.tasks.values());
    }

    public getInfo(): FloorAgent {
        return {
            agentId: this.agentId,
            name: this.name,
            role: 'Manager Agent',
            office: 'Manager Office',
            capabilities: this.capabilities
        };
    }
}

// ============================================================================
// MAIN DEPARTMENT FLOOR CLASS
// ============================================================================

class TypeScriptDepartmentFloor {
    /**
     * TypeScript Department Floor - Floor 6
     * 
     * Implements department logic following:
     * - Language Sovereignty: All code in TypeScript
     * - Identical Internal Topology: 6 offices (Architecture, Implementation, Review, Test, Security, Manager)
     * - Contract-Bound Operation: JSON-RPC protocol
     * - Non-Creative Mandate: Strict adherence to requests
     * - Failure Escalation Guarantee: Explicit error handling
     */
    
    private readonly floorNumber: number = 6;
    private readonly language: string = 'typescript';
    private readonly domain: string = 'Typed frontend systems, Node.js backends, Type-safe applications';
    private readonly offices: string[] = [
        'Architecture Office',
        'Implementation Office',
        'Review Office',
        'Test Office',
        'Security Office',
        'Manager Office'
    ];

    private serviceAgent: ServiceAgent;
    private dataModelAgent: DataModelAgent;
    private operationsAgent: OperationsAgent;
    private securityAgent: SecurityAgent;
    private testAgent: TestAgent;
    private managerAgent: ManagerAgent;

    constructor() {
        // Initialize all agents
        this.serviceAgent = new ServiceAgent('service-001', 'TypeScript Service Agent');
        this.dataModelAgent = new DataModelAgent('data-001', 'TypeScript Data Agent');
        this.operationsAgent = new OperationsAgent('ops-001', 'TypeScript Operations Agent');
        this.securityAgent = new SecurityAgent('security-001', 'TypeScript Security Agent');
        this.testAgent = new TestAgent('test-001', 'TypeScript Test Agent');
        this.managerAgent = new ManagerAgent('manager-001', 'TypeScript Manager Agent');
    }

    public getFloorInfo(): Record<string, unknown> {
        const agents = [
            this.serviceAgent.getInfo(),
            this.dataModelAgent.getInfo(),
            this.operationsAgent.getInfo(),
            this.securityAgent.getInfo(),
            this.testAgent.getInfo(),
            this.managerAgent.getInfo()
        ];

        return {
            floorNumber: this.floorNumber,
            language: this.language,
            domain: this.domain,
            offices: this.offices,
            agentCount: agents.length,
            agents,
            tasks: this.managerAgent.getTasks(),
            architecturalLaw: 'Strict typing mandatory, no any types, compile-time safety',
            securityDoctrine: 'Type guards, runtime validation, null safety'
        };
    }

    public processCode(code: string, operation: string): Record<string, unknown> {
        switch (operation) {
            case 'analyze':
                return this.operationsAgent.executeOperation('analyze', { code });
            case 'security_audit':
                return this.securityAgent.auditSecurity(code);
            case 'check_quality':
                return this.operationsAgent.executeOperation('check_quality', { code });
            case 'find_issues':
                return this.operationsAgent.executeOperation('find_issues', { code });
            case 'analyze_tests':
                return this.testAgent.analyzeTests(code);
            default:
                throw new Error(`Unknown code operation: ${operation}`);
        }
    }

    public handleRequest(request: JSONRPCRequest): JSONRPCResponse {
        try {
            const { method, params = {} } = request;
            let result: unknown;

            switch (method) {
                case 'get_info':
                    result = this.getFloorInfo();
                    break;

                case 'process_code':
                    result = this.processCode(
                        params.code as string,
                        params.operation as string
                    );
                    break;

                case 'create_task':
                    result = this.managerAgent.createTask(
                        params.taskId as string,
                        params.title as string,
                        params.assignedTo as string
                    );
                    break;

                case 'update_task':
                    result = this.managerAgent.updateTaskStatus(
                        params.taskId as string,
                        params.status as Task['status']
                    );
                    break;

                case 'execute_service':
                    result = this.serviceAgent.executeService(
                        params.serviceName as string,
                        params.params as Record<string, unknown>
                    );
                    break;

                case 'process_data':
                    result = this.dataModelAgent.processData(
                        params.operation as string,
                        params.data
                    );
                    break;

                default:
                    throw new Error(`Unknown method: ${method}`);
            }

            return {
                result,
                id: request.id
            };

        } catch (error) {
            return {
                error: {
                    code: -32603,
                    message: error instanceof Error ? error.message : String(error)
                },
                id: request.id
            };
        }
    }
}

// ============================================================================
// MAIN ENTRY POINT
// ============================================================================

function main(): void {
    const floor = new TypeScriptDepartmentFloor();

    console.error('TypeScript Department Floor (Floor 6) - Ready');
    console.error(`Domain: ${floor.getFloorInfo().domain}`);
    console.error(`Offices: ${(floor.getFloorInfo().offices as string[]).join(', ')}`);

    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        terminal: false
    });

    rl.on('line', (line: string) => {
        try {
            const request = JSON.parse(line) as JSONRPCRequest;
            const response = floor.handleRequest(request);
            console.log(JSON.stringify(response));
        } catch (error) {
            const errorResponse: JSONRPCResponse = {
                error: {
                    code: -32700,
                    message: `Parse error: ${error instanceof Error ? error.message : String(error)}`
                }
            };
            console.log(JSON.stringify(errorResponse));
        }
    });
}

if (require.main === module) {
    main();
}

export { TypeScriptDepartmentFloor, ServiceAgent, DataModelAgent, OperationsAgent };
