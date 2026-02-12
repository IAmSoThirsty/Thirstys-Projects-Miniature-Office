#!/usr/bin/env php
<?php
/**
 * FLOOR 15 - PHP JURISDICTION
 * Department Floor Implementation
 * 
 * Domain: Web backend systems, CMS platforms, Server-side applications
 * Architectural Law: PSR compliance, type declarations, secure by default
 * Security Doctrine: Input validation, SQL injection prevention, XSS protection
 */

declare(strict_types=1);

// ============================================================================
// CORE DATA MODELS
// ============================================================================

class FloorAgent {
    public function __construct(
        public readonly string $agentId,
        public readonly string $name,
        public readonly string $role,
        public readonly string $office,
        public readonly array $capabilities
    ) {}

    public function toArray(): array {
        return [
            'agentId' => $this->agentId,
            'name' => $this->name,
            'role' => $this->role,
            'office' => $this->office,
            'capabilities' => $this->capabilities
        ];
    }
}

class Task {
    public ?string $completedAt = null;

    public function __construct(
        public readonly string $taskId,
        public readonly string $title,
        public string $status,
        public readonly string $assignedTo,
        public readonly string $createdAt
    ) {}

    public function toArray(): array {
        return [
            'taskId' => $this->taskId,
            'title' => $this->title,
            'status' => $this->status,
            'assignedTo' => $this->assignedTo,
            'createdAt' => $this->createdAt,
            'completedAt' => $this->completedAt
        ];
    }
}

class CodeAnalysis {
    public function __construct(
        public readonly int $lines,
        public readonly int $classes,
        public readonly int $functions,
        public readonly int $namespaces,
        public readonly int $traits,
        public readonly string $language
    ) {}

    public function toArray(): array {
        return [
            'lines' => $this->lines,
            'classes' => $this->classes,
            'functions' => $this->functions,
            'namespaces' => $this->namespaces,
            'traits' => $this->traits,
            'language' => $this->language
        ];
    }
}

// ============================================================================
// AGENT CLASSES - Production Grade Implementations
// ============================================================================

class ServiceAgent {
    /**
     * Service Agent - Handles web services, APIs, and external integrations
     * Office: Implementation Office
     */
    private string $agentId;
    private string $name;
    private array $capabilities;

    public function __construct(string $agentId, string $name) {
        $this->agentId = $agentId;
        $this->name = $name;
        $this->capabilities = [
            'http_handling',
            'api_integration',
            'session_management',
            'file_operations'
        ];
    }

    public function executeService(string $serviceName, array $params): array {
        return match($serviceName) {
            'validate_types' => $this->validateTypes($params),
            'check_security' => $this->checkSecurity($params),
            'analyze_dependencies' => $this->analyzeDependencies($params),
            default => throw new \Exception("Unknown service: $serviceName")
        };
    }

    private function validateTypes(array $params): array {
        if (!isset($params['code'])) {
            throw new \Exception('Code parameter required');
        }

        $code = $params['code'];
        $strictTypesCount = preg_match_all('/declare\s*\(\s*strict_types\s*=\s*1\s*\)/i', $code);
        $typeHints = preg_match_all('/function\s+\w+\s*\([^)]*:\s*\w+/', $code);
        $returnTypes = preg_match_all('/\)\s*:\s*\w+/', $code);

        return [
            'hasStrictTypes' => $strictTypesCount > 0,
            'typeHintCount' => $typeHints,
            'returnTypeCount' => $returnTypes,
            'recommendation' => $strictTypesCount === 0 ? 'Add declare(strict_types=1)' : 'Type usage acceptable'
        ];
    }

    private function checkSecurity(array $params): array {
        if (!isset($params['code'])) {
            throw new \Exception('Code parameter required');
        }

        $code = $params['code'];
        $issues = [];

        if (preg_match('/\$_(GET|POST|REQUEST|COOKIE)\[/', $code)) {
            $issues[] = 'Direct superglobal access - use input filtering';
        }

        if (preg_match('/eval\s*\(/', $code)) {
            $issues[] = 'eval() usage detected - potential code injection';
        }

        if (preg_match('/mysql_query|mysqli_query.*\$_/', $code)) {
            $issues[] = 'Potential SQL injection - use prepared statements';
        }

        if (preg_match('/echo\s+\$_|print\s+\$_/', $code)) {
            $issues[] = 'Potential XSS - escape output';
        }

        return [
            'secure' => empty($issues),
            'issues' => $issues,
            'issueCount' => count($issues)
        ];
    }

    private function analyzeDependencies(array $params): array {
        if (!isset($params['code'])) {
            throw new \Exception('Code parameter required');
        }

        $code = $params['code'];
        preg_match_all('/use\s+([\\\\a-zA-Z0-9_]+)/', $code, $matches);
        $namespaces = $matches[1] ?? [];

        preg_match_all('/require(?:_once)?\s*[\'"]([^\'"]+)[\'"]/', $code, $requireMatches);
        $requires = $requireMatches[1] ?? [];

        return [
            'namespaceImports' => count($namespaces),
            'fileRequires' => count($requires),
            'imports' => $namespaces,
            'requires' => $requires
        ];
    }

    public function getInfo(): FloorAgent {
        return new FloorAgent(
            $this->agentId,
            $this->name,
            'Service Agent',
            'Implementation Office',
            $this->capabilities
        );
    }
}

class DataModelAgent {
    /**
     * Data Model Agent - Manages database models, ORM, and data structures
     * Office: Architecture Office
     */
    private string $agentId;
    private string $name;
    private array $capabilities;

    public function __construct(string $agentId, string $name) {
        $this->agentId = $agentId;
        $this->name = $name;
        $this->capabilities = [
            'database_modeling',
            'orm_integration',
            'data_validation',
            'schema_generation'
        ];
    }

    public function processData(string $operation, mixed $data): array {
        return match($operation) {
            'validate' => $this->validateData($data),
            'transform' => $this->transformData($data),
            'infer_schema' => $this->inferSchema($data),
            default => throw new \Exception("Unknown operation: $operation")
        };
    }

    private function validateData(mixed $data): array {
        if ($data === null) {
            return ['valid' => false, 'error' => 'Data is null'];
        }

        $type = gettype($data);
        $isObject = is_object($data);
        $isArray = is_array($data);

        return [
            'valid' => true,
            'type' => $type,
            'isObject' => $isObject,
            'isArray' => $isArray,
            'keys' => $isArray ? array_keys($data) : []
        ];
    }

    private function transformData(mixed $data): array {
        if (!is_array($data)) {
            return ['transformed' => $data, 'changesMade' => false];
        }

        $transformed = [];
        $changesMade = false;

        foreach ($data as $key => $value) {
            $camelKey = lcfirst(str_replace('_', '', ucwords($key, '_')));
            $transformed[$camelKey] = $value;
            if ($camelKey !== $key) {
                $changesMade = true;
            }
        }

        return ['transformed' => $transformed, 'changesMade' => $changesMade];
    }

    private function inferSchema(mixed $data): array {
        if ($data === null) {
            return ['type' => 'null'];
        }

        $type = gettype($data);

        if ($type !== 'array' && $type !== 'object') {
            return ['type' => $type];
        }

        if (is_array($data)) {
            if (empty($data)) {
                return ['type' => 'array', 'elementTypes' => [], 'length' => 0];
            }

            $elementTypes = array_unique(array_map('gettype', $data));
            return [
                'type' => 'array',
                'elementTypes' => array_values($elementTypes),
                'length' => count($data)
            ];
        }

        return ['type' => 'object', 'class' => get_class($data)];
    }

    public function getInfo(): FloorAgent {
        return new FloorAgent(
            $this->agentId,
            $this->name,
            'Data Model Agent',
            'Architecture Office',
            $this->capabilities
        );
    }
}

class OperationsAgent {
    /**
     * Operations Agent - Handles code analysis, quality checks, and optimization
     * Office: Review Office
     */
    private string $agentId;
    private string $name;
    private array $capabilities;

    public function __construct(string $agentId, string $name) {
        $this->agentId = $agentId;
        $this->name = $name;
        $this->capabilities = [
            'code_analysis',
            'quality_checks',
            'psr_compliance',
            'performance_review'
        ];
    }

    public function executeOperation(string $operation, array $params): array {
        $code = $params['code'] ?? throw new \Exception('Code parameter required');

        return match($operation) {
            'analyze' => $this->analyzeCode($code),
            'check_quality' => $this->checkQuality($code),
            'find_issues' => $this->findIssues($code),
            default => throw new \Exception("Unknown operation: $operation")
        };
    }

    private function analyzeCode(string $code): array {
        $lines = count(explode("\n", $code));
        $classes = preg_match_all('/class\s+\w+/', $code);
        $functions = preg_match_all('/function\s+\w+/', $code);
        $namespaces = preg_match_all('/namespace\s+[\\\\a-zA-Z0-9_]+/', $code);
        $traits = preg_match_all('/trait\s+\w+/', $code);

        $analysis = new CodeAnalysis(
            $lines,
            $classes,
            $functions,
            $namespaces,
            $traits,
            'php'
        );

        return ['status' => 'success', 'analysis' => $analysis->toArray()];
    }

    private function checkQuality(string $code): array {
        $issues = [];
        $score = 100;

        if (!preg_match('/declare\s*\(\s*strict_types\s*=\s*1\s*\)/', $code)) {
            $issues[] = 'Missing declare(strict_types=1)';
            $score -= 15;
        }

        if (preg_match('/var_dump|print_r/', $code)) {
            $issues[] = 'Debug statements found (var_dump/print_r)';
            $score -= 10;
        }

        if (!preg_match('/namespace\s+[\\\\a-zA-Z0-9_]+/', $code)) {
            $issues[] = 'No namespace declaration';
            $score -= 10;
        }

        if (preg_match('/\$_(GET|POST|REQUEST|COOKIE)\[/', $code)) {
            $issues[] = 'Direct superglobal access without filtering';
            $score -= 20;
        }

        if (preg_match('/mysql_/', $code)) {
            $issues[] = 'Deprecated mysql_ functions';
            $score -= 25;
        }

        return [
            'score' => max(0, $score),
            'issues' => $issues,
            'passed' => $score >= 70
        ];
    }

    private function findIssues(string $code): array {
        $issues = [];
        $lines = explode("\n", $code);

        foreach ($lines as $lineNum => $line) {
            if (preg_match('/eval\s*\(/', $line)) {
                $issues[] = [
                    'severity' => 'critical',
                    'message' => 'eval() usage is dangerous',
                    'line' => $lineNum + 1
                ];
            }

            if (preg_match('/\$_(GET|POST)\[.*\].*query|exec/', $line)) {
                $issues[] = [
                    'severity' => 'critical',
                    'message' => 'Potential SQL injection',
                    'line' => $lineNum + 1
                ];
            }

            if (preg_match('/echo\s+\$_/', $line)) {
                $issues[] = [
                    'severity' => 'high',
                    'message' => 'Potential XSS vulnerability',
                    'line' => $lineNum + 1
                ];
            }

            if (preg_match('/=\s*=\s*[^=]/', $line) && !preg_match('/===/', $line)) {
                $issues[] = [
                    'severity' => 'warning',
                    'message' => 'Use === for strict comparison',
                    'line' => $lineNum + 1
                ];
            }
        }

        $critical = array_filter($issues, fn($i) => $i['severity'] === 'critical');

        return [
            'totalIssues' => count($issues),
            'issues' => $issues,
            'critical' => count($critical)
        ];
    }

    public function getInfo(): FloorAgent {
        return new FloorAgent(
            $this->agentId,
            $this->name,
            'Operations Agent',
            'Review Office',
            $this->capabilities
        );
    }
}

class SecurityAgent {
    /**
     * Security Agent - Performs security audits and vulnerability scanning
     * Office: Security Office
     */
    private string $agentId;
    private string $name;
    private array $capabilities;

    public function __construct(string $agentId, string $name) {
        $this->agentId = $agentId;
        $this->name = $name;
        $this->capabilities = [
            'vulnerability_scanning',
            'sql_injection_detection',
            'xss_detection',
            'authentication_review'
        ];
    }

    public function auditSecurity(string $code): array {
        $vulnerabilities = [];

        if (preg_match('/eval\s*\(/', $code)) {
            $vulnerabilities[] = [
                'type' => 'code_injection',
                'severity' => 'critical',
                'description' => 'eval() usage - remote code execution risk'
            ];
        }

        if (preg_match('/\$_(GET|POST|REQUEST|COOKIE)\[.*\].*(?:query|exec|system|passthru)/', $code)) {
            $vulnerabilities[] = [
                'type' => 'sql_injection',
                'severity' => 'critical',
                'description' => 'User input in database query without sanitization'
            ];
        }

        if (preg_match('/echo\s+\$_|print\s+\$_/', $code)) {
            $vulnerabilities[] = [
                'type' => 'xss',
                'severity' => 'high',
                'description' => 'Unsanitized output - XSS vulnerability'
            ];
        }

        if (preg_match('/password.*=.*[\'"][^\'"]+[\'"]/', $code)) {
            $vulnerabilities[] = [
                'type' => 'hardcoded_credentials',
                'severity' => 'critical',
                'description' => 'Hardcoded password detected'
            ];
        }

        if (preg_match('/file_get_contents\s*\(\s*\$_/', $code)) {
            $vulnerabilities[] = [
                'type' => 'path_traversal',
                'severity' => 'high',
                'description' => 'File inclusion with user input'
            ];
        }

        if (preg_match('/unserialize\s*\(\s*\$_/', $code)) {
            $vulnerabilities[] = [
                'type' => 'deserialization',
                'severity' => 'critical',
                'description' => 'Unsafe deserialization of user input'
            ];
        }

        $critical = array_filter($vulnerabilities, fn($v) => $v['severity'] === 'critical');
        $high = array_filter($vulnerabilities, fn($v) => $v['severity'] === 'high');

        return [
            'secure' => empty($vulnerabilities),
            'vulnerabilities' => $vulnerabilities,
            'criticalCount' => count($critical),
            'highCount' => count($high)
        ];
    }

    public function getInfo(): FloorAgent {
        return new FloorAgent(
            $this->agentId,
            $this->name,
            'Security Agent',
            'Security Office',
            $this->capabilities
        );
    }
}

class TestAgent {
    /**
     * Test Agent - Handles unit testing, integration testing, and test analysis
     * Office: Test Office
     */
    private string $agentId;
    private string $name;
    private array $capabilities;

    public function __construct(string $agentId, string $name) {
        $this->agentId = $agentId;
        $this->name = $name;
        $this->capabilities = [
            'phpunit_testing',
            'test_coverage',
            'assertion_analysis',
            'mock_generation'
        ];
    }

    public function analyzeTests(string $code): array {
        $frameworks = [
            'phpunit' => (bool)preg_match('/class\s+\w+Test|extends\s+TestCase/', $code),
            'pest' => (bool)preg_match('/test\s*\(|it\s*\(/', $code),
            'codeception' => (bool)preg_match('/\$I->/', $code)
        ];

        preg_match_all('/\$this->assert[A-Z]/', $code, $assertMatches);
        preg_match_all('/expect\s*\(/', $code, $expectMatches);

        $assertions = count($assertMatches[0]) + count($expectMatches[0]);

        return [
            'hasTests' => in_array(true, $frameworks, true),
            'frameworks' => array_keys(array_filter($frameworks)),
            'assertionCount' => $assertions,
            'coverage' => 'unknown'
        ];
    }

    public function getInfo(): FloorAgent {
        return new FloorAgent(
            $this->agentId,
            $this->name,
            'Test Agent',
            'Test Office',
            $this->capabilities
        );
    }
}

class ManagerAgent {
    /**
     * Manager Agent - Coordinates tasks and manages workflow
     * Office: Manager Office
     */
    private string $agentId;
    private string $name;
    private array $capabilities;
    private array $tasks = [];

    public function __construct(string $agentId, string $name) {
        $this->agentId = $agentId;
        $this->name = $name;
        $this->capabilities = [
            'task_management',
            'workflow_coordination',
            'resource_allocation',
            'progress_tracking'
        ];
    }

    public function createTask(string $taskId, string $title, string $assignedTo): Task {
        $task = new Task(
            $taskId,
            $title,
            'pending',
            $assignedTo,
            date('c')
        );
        $this->tasks[$taskId] = $task;
        return $task;
    }

    public function updateTaskStatus(string $taskId, string $status): Task {
        if (!isset($this->tasks[$taskId])) {
            throw new \Exception("Task not found: $taskId");
        }

        $task = $this->tasks[$taskId];
        $task->status = $status;
        
        if ($status === 'completed') {
            $task->completedAt = date('c');
        }

        return $task;
    }

    public function getTasks(): array {
        return array_values($this->tasks);
    }

    public function getInfo(): FloorAgent {
        return new FloorAgent(
            $this->agentId,
            $this->name,
            'Manager Agent',
            'Manager Office',
            $this->capabilities
        );
    }
}

// ============================================================================
// MAIN DEPARTMENT FLOOR CLASS
// ============================================================================

class PHPDepartmentFloor {
    /**
     * PHP Department Floor - Floor 15
     * 
     * Implements department logic following:
     * - Language Sovereignty: All code in PHP
     * - Identical Internal Topology: 6 offices
     * - Contract-Bound Operation: JSON-RPC protocol
     * - Non-Creative Mandate: Strict adherence to requests
     * - Failure Escalation Guarantee: Explicit error handling
     */
    
    private const FLOOR_NUMBER = 15;
    private const LANGUAGE = 'php';
    private const DOMAIN = 'Web backend systems, CMS platforms, Server-side applications';
    private const OFFICES = [
        'Architecture Office',
        'Implementation Office',
        'Review Office',
        'Test Office',
        'Security Office',
        'Manager Office'
    ];

    private ServiceAgent $serviceAgent;
    private DataModelAgent $dataModelAgent;
    private OperationsAgent $operationsAgent;
    private SecurityAgent $securityAgent;
    private TestAgent $testAgent;
    private ManagerAgent $managerAgent;

    public function __construct() {
        $this->serviceAgent = new ServiceAgent('service-001', 'PHP Service Agent');
        $this->dataModelAgent = new DataModelAgent('data-001', 'PHP Data Agent');
        $this->operationsAgent = new OperationsAgent('ops-001', 'PHP Operations Agent');
        $this->securityAgent = new SecurityAgent('security-001', 'PHP Security Agent');
        $this->testAgent = new TestAgent('test-001', 'PHP Test Agent');
        $this->managerAgent = new ManagerAgent('manager-001', 'PHP Manager Agent');
    }

    public function getFloorInfo(): array {
        $agents = [
            $this->serviceAgent->getInfo()->toArray(),
            $this->dataModelAgent->getInfo()->toArray(),
            $this->operationsAgent->getInfo()->toArray(),
            $this->securityAgent->getInfo()->toArray(),
            $this->testAgent->getInfo()->toArray(),
            $this->managerAgent->getInfo()->toArray()
        ];

        return [
            'floorNumber' => self::FLOOR_NUMBER,
            'language' => self::LANGUAGE,
            'domain' => self::DOMAIN,
            'offices' => self::OFFICES,
            'agentCount' => count($agents),
            'agents' => $agents,
            'tasks' => array_map(fn($t) => $t->toArray(), $this->managerAgent->getTasks()),
            'architecturalLaw' => 'PSR compliance, type declarations, secure by default',
            'securityDoctrine' => 'Input validation, SQL injection prevention, XSS protection'
        ];
    }

    public function processCode(string $code, string $operation): array {
        return match($operation) {
            'analyze' => $this->operationsAgent->executeOperation('analyze', ['code' => $code]),
            'security_audit' => $this->securityAgent->auditSecurity($code),
            'check_quality' => $this->operationsAgent->executeOperation('check_quality', ['code' => $code]),
            'find_issues' => $this->operationsAgent->executeOperation('find_issues', ['code' => $code]),
            'analyze_tests' => $this->testAgent->analyzeTests($code),
            default => throw new \Exception("Unknown code operation: $operation")
        };
    }

    public function handleRequest(array $request): array {
        try {
            $method = $request['method'] ?? throw new \Exception('Method required');
            $params = $request['params'] ?? [];
            $id = $request['id'] ?? null;

            $result = match($method) {
                'get_info' => $this->getFloorInfo(),
                'process_code' => $this->processCode($params['code'], $params['operation']),
                'create_task' => $this->managerAgent->createTask(
                    $params['taskId'],
                    $params['title'],
                    $params['assignedTo']
                )->toArray(),
                'update_task' => $this->managerAgent->updateTaskStatus(
                    $params['taskId'],
                    $params['status']
                )->toArray(),
                'execute_service' => $this->serviceAgent->executeService(
                    $params['serviceName'],
                    $params['params']
                ),
                'process_data' => $this->dataModelAgent->processData(
                    $params['operation'],
                    $params['data']
                ),
                default => throw new \Exception("Unknown method: $method")
            };

            return ['result' => $result, 'id' => $id];

        } catch (\Exception $e) {
            return [
                'error' => [
                    'code' => -32603,
                    'message' => $e->getMessage()
                ],
                'id' => $request['id'] ?? null
            ];
        }
    }
}

// ============================================================================
// MAIN ENTRY POINT
// ============================================================================

function main(): void {
    $floor = new PHPDepartmentFloor();

    fwrite(STDERR, "PHP Department Floor (Floor 15) - Ready\n");
    fwrite(STDERR, "Domain: " . $floor->getFloorInfo()['domain'] . "\n");
    fwrite(STDERR, "Offices: " . implode(', ', $floor->getFloorInfo()['offices']) . "\n");

    while ($line = fgets(STDIN)) {
        $line = trim($line);
        if (empty($line)) continue;

        try {
            $request = json_decode($line, true, 512, JSON_THROW_ON_ERROR);
            $response = $floor->handleRequest($request);
            echo json_encode($response, JSON_THROW_ON_ERROR) . "\n";
        } catch (\JsonException $e) {
            $errorResponse = [
                'error' => [
                    'code' => -32700,
                    'message' => 'Parse error: ' . $e->getMessage()
                ]
            ];
            echo json_encode($errorResponse) . "\n";
        }
        
        flush();
    }
}

if (php_sapi_name() === 'cli') {
    main();
}
