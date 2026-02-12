//
// department_floor.m
// Floor 14 - Objective-C Jurisdiction
//
// Domain: Legacy Apple systems, macOS/iOS development, Darwin frameworks
// Architectural Law: MRC/ARC patterns, Message passing, Memory management
// Security Doctrine: Memory safety, Retain cycles, Buffer overflows
//

#import "department_floor.h"

// Error domain
static NSString * const ObjectiveCFloorErrorDomain = @"com.floor14.objc";

// ============================================================================
// CORE DATA MODELS IMPLEMENTATION
// ============================================================================

@implementation FloorAgent

- (instancetype)initWithAgentId:(NSString *)agentId
                           name:(NSString *)name
                           role:(NSString *)role
                         office:(NSString *)office
                   capabilities:(NSArray<NSString *> *)capabilities {
    self = [super init];
    if (self) {
        _agentId = [agentId copy];
        _name = [name copy];
        _role = [role copy];
        _office = [office copy];
        _capabilities = [capabilities copy];
    }
    return self;
}

- (NSDictionary *)toDictionary {
    return @{
        @"agentId": self.agentId ?: [NSNull null],
        @"name": self.name ?: [NSNull null],
        @"role": self.role ?: [NSNull null],
        @"office": self.office ?: [NSNull null],
        @"capabilities": self.capabilities ?: @[]
    };
}

@end

@implementation Task

- (instancetype)initWithTaskId:(NSString *)taskId
                         title:(NSString *)title
                        status:(NSString *)status
                    assignedTo:(NSString *)assignedTo {
    self = [super init];
    if (self) {
        _taskId = [taskId copy];
        _title = [title copy];
        _status = [status copy];
        _assignedTo = [assignedTo copy];
        _createdAt = [NSDate date];
        _completedAt = nil;
    }
    return self;
}

- (NSDictionary *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    dict[@"taskId"] = self.taskId ?: [NSNull null];
    dict[@"title"] = self.title ?: [NSNull null];
    dict[@"status"] = self.status ?: [NSNull null];
    dict[@"assignedTo"] = self.assignedTo ?: [NSNull null];
    
    if (self.createdAt) {
        dict[@"createdAt"] = [self ISO8601StringFromDate:self.createdAt];
    }
    if (self.completedAt) {
        dict[@"completedAt"] = [self ISO8601StringFromDate:self.completedAt];
    }
    
    return [dict copy];
}

- (NSString *)ISO8601StringFromDate:(NSDate *)date {
    NSISO8601DateFormatter *formatter = [[NSISO8601DateFormatter alloc] init];
    return [formatter stringFromDate:date];
}

@end

@implementation CodeAnalysis

- (instancetype)initWithLines:(NSInteger)lines
                   interfaces:(NSInteger)interfaces
              implementations:(NSInteger)implementations
                      methods:(NSInteger)methods
                   properties:(NSInteger)properties {
    self = [super init];
    if (self) {
        _lines = lines;
        _interfaces = interfaces;
        _implementations = implementations;
        _methods = methods;
        _properties = properties;
        _language = @"objective-c";
    }
    return self;
}

- (NSDictionary *)toDictionary {
    return @{
        @"lines": @(self.lines),
        @"interfaces": @(self.interfaces),
        @"implementations": @(self.implementations),
        @"methods": @(self.methods),
        @"properties": @(self.properties),
        @"language": self.language
    };
}

@end

// ============================================================================
// AGENT IMPLEMENTATIONS - Production Grade
// ============================================================================

@implementation ServiceAgent {
    NSString *_agentId;
    NSString *_name;
    NSArray<NSString *> *_capabilities;
}

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name {
    self = [super init];
    if (self) {
        _agentId = [agentId copy];
        _name = [name copy];
        _capabilities = @[
            @"cocoa_frameworks",
            @"memory_management",
            @"message_passing",
            @"delegate_patterns"
        ];
    }
    return self;
}

- (NSString *)agentId {
    return _agentId;
}

- (NSString *)name {
    return _name;
}

- (NSArray<NSString *> *)capabilities {
    return _capabilities;
}

- (NSDictionary *)executeService:(NSString *)serviceName params:(NSDictionary *)params error:(NSError **)error {
    if ([serviceName isEqualToString:@"validate_style"]) {
        return [self validateStyle:params error:error];
    } else if ([serviceName isEqualToString:@"check_patterns"]) {
        return [self checkPatterns:params error:error];
    } else if ([serviceName isEqualToString:@"analyze_memory"]) {
        return [self analyzeMemory:params error:error];
    } else {
        if (error) {
            *error = [NSError errorWithDomain:ObjectiveCFloorErrorDomain
                                        code:1001
                                    userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"Unknown service: %@", serviceName]}];
        }
        return nil;
    }
}

- (NSDictionary *)validateStyle:(NSDictionary *)params error:(NSError **)error {
    NSString *code = params[@"code"];
    if (!code) {
        if (error) {
            *error = [NSError errorWithDomain:ObjectiveCFloorErrorDomain
                                        code:1002
                                    userInfo:@{NSLocalizedDescriptionKey: @"Code parameter required"}];
        }
        return nil;
    }
    
    NSMutableArray<NSString *> *issues = [NSMutableArray array];
    
    // Check for proper @interface/@implementation
    if ([code containsString:@"@interface"] && ![code containsString:@"@end"]) {
        [issues addObject:@"@interface must have matching @end"];
    }
    
    // Check for proper property attributes
    if ([code containsString:@"@property"] && ![code containsString:@"nonatomic"]) {
        [issues addObject:@"Properties should specify atomicity (nonatomic/atomic)"];
    }
    
    // Check for nullability annotations
    if ([code containsString:@"*"] && ![code containsString:@"nullable"] && ![code containsString:@"nonnull"]) {
        [issues addObject:@"Use nullability annotations (nullable/nonnull)"];
    }
    
    return @{
        @"valid": @(issues.count == 0),
        @"issues": [issues copy],
        @"issueCount": @(issues.count)
    };
}

- (NSDictionary *)checkPatterns:(NSDictionary *)params error:(NSError **)error {
    NSString *code = params[@"code"];
    if (!code) {
        if (error) {
            *error = [NSError errorWithDomain:ObjectiveCFloorErrorDomain
                                        code:1002
                                    userInfo:@{NSLocalizedDescriptionKey: @"Code parameter required"}];
        }
        return nil;
    }
    
    NSMutableArray<NSString *> *patterns = [NSMutableArray array];
    
    if ([code containsString:@"@protocol"]) {
        [patterns addObject:@"Protocol-oriented design"];
    }
    if ([code containsString:@"delegate"]) {
        [patterns addObject:@"Delegate pattern"];
    }
    if ([code containsString:@"NSNotificationCenter"]) {
        [patterns addObject:@"Notification pattern"];
    }
    if ([code rangeOfString:@"^" options:NSRegularExpressionSearch].location != NSNotFound) {
        [patterns addObject:@"Block (closure) usage"];
    }
    if ([code containsString:@"@synchronized"]) {
        [patterns addObject:@"Thread synchronization"];
    }
    
    return @{
        @"patterns": [patterns copy],
        @"patternCount": @(patterns.count)
    };
}

- (NSDictionary *)analyzeMemory:(NSDictionary *)params error:(NSError **)error {
    NSString *code = params[@"code"];
    if (!code) {
        if (error) {
            *error = [NSError errorWithDomain:ObjectiveCFloorErrorDomain
                                        code:1002
                                    userInfo:@{NSLocalizedDescriptionKey: @"Code parameter required"}];
        }
        return nil;
    }
    
    NSMutableArray<NSString *> *issues = [NSMutableArray array];
    
    // Check for strong reference cycles
    NSRegularExpression *blockRegex = [NSRegularExpression regularExpressionWithPattern:@"\\^[\\s\\(]" options:0 error:nil];
    BOOL hasBlocks = [blockRegex numberOfMatchesInString:code options:0 range:NSMakeRange(0, code.length)] > 0;
    
    if ([code containsString:@"self."] && hasBlocks && ![code containsString:@"__weak"]) {
        [issues addObject:@"Potential retain cycle: use __weak self in blocks"];
    }
    
    // Check for retain/release in ARC
    if ([code containsString:@"retain"] || [code containsString:@"release"] || [code containsString:@"autorelease"]) {
        [issues addObject:@"Manual memory management detected (retain/release/autorelease)"];
    }
    
    // Check for assign with objects
    if ([code containsString:@"@property"] && [code containsString:@"assign"] && [code containsString:@"*"]) {
        [issues addObject:@"Using assign with object types can cause dangling pointers"];
    }
    
    return @{
        @"memoryIssues": [issues copy],
        @"issueCount": @(issues.count)
    };
}

- (FloorAgent *)getInfo {
    return [[FloorAgent alloc] initWithAgentId:self.agentId
                                          name:self.name
                                          role:@"Service Agent"
                                        office:@"Implementation Office"
                                  capabilities:self.capabilities];
}

@end

@implementation DataModelAgent {
    NSString *_agentId;
    NSString *_name;
    NSArray<NSString *> *_capabilities;
}

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name {
    self = [super init];
    if (self) {
        _agentId = [agentId copy];
        _name = [name copy];
        _capabilities = @[
            @"model_design",
            @"core_data",
            @"serialization",
            @"type_encoding"
        ];
    }
    return self;
}

- (NSString *)agentId {
    return _agentId;
}

- (NSString *)name {
    return _name;
}

- (NSArray<NSString *> *)capabilities {
    return _capabilities;
}

- (NSDictionary *)processData:(NSString *)operation data:(id)data error:(NSError **)error {
    if ([operation isEqualToString:@"validate"]) {
        return [self validateData:data];
    } else if ([operation isEqualToString:@"serialize"]) {
        return [self serializeData:data error:error];
    } else if ([operation isEqualToString:@"infer_type"]) {
        return [self inferType:data];
    } else {
        if (error) {
            *error = [NSError errorWithDomain:ObjectiveCFloorErrorDomain
                                        code:1001
                                    userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"Unknown operation: %@", operation]}];
        }
        return nil;
    }
}

- (NSDictionary *)validateData:(id)data {
    if (!data || data == [NSNull null]) {
        return @{
            @"valid": @NO,
            @"error": @"Data is nil or null"
        };
    }
    
    NSString *className = NSStringFromClass([data class]);
    BOOL isDictionary = [data isKindOfClass:[NSDictionary class]];
    BOOL isArray = [data isKindOfClass:[NSArray class]];
    BOOL isString = [data isKindOfClass:[NSString class]];
    
    return @{
        @"valid": @YES,
        @"type": className,
        @"isDictionary": @(isDictionary),
        @"isArray": @(isArray),
        @"isString": @(isString)
    };
}

- (NSDictionary *)serializeData:(id)data error:(NSError **)error {
    if ([NSJSONSerialization isValidJSONObject:data]) {
        NSData *jsonData = [NSJSONSerialization dataWithJSONObject:data options:0 error:error];
        if (jsonData) {
            NSString *jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
            return @{
                @"serialized": jsonString,
                @"format": @"json"
            };
        }
    }
    
    return @{
        @"serialized": [data description],
        @"format": @"description"
    };
}

- (NSDictionary *)inferType:(id)data {
    if (!data || data == [NSNull null]) {
        return @{@"inferredType": @"null"};
    }
    
    return @{
        @"inferredType": NSStringFromClass([data class]),
        @"isCollection": @([data respondsToSelector:@selector(count)]),
        @"isMutable": @([data respondsToSelector:@selector(addObject:)])
    };
}

- (FloorAgent *)getInfo {
    return [[FloorAgent alloc] initWithAgentId:self.agentId
                                          name:self.name
                                          role:@"Data Model Agent"
                                        office:@"Architecture Office"
                                  capabilities:self.capabilities];
}

@end

@implementation OperationsAgent {
    NSString *_agentId;
    NSString *_name;
    NSArray<NSString *> *_capabilities;
}

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name {
    self = [super init];
    if (self) {
        _agentId = [agentId copy];
        _name = [name copy];
        _capabilities = @[
            @"code_analysis",
            @"quality_metrics",
            @"pattern_detection",
            @"refactoring_suggestions"
        ];
    }
    return self;
}

- (NSString *)agentId {
    return _agentId;
}

- (NSString *)name {
    return _name;
}

- (NSArray<NSString *> *)capabilities {
    return _capabilities;
}

- (CodeAnalysis *)analyzeCode:(NSString *)code {
    NSInteger lines = [[code componentsSeparatedByString:@"\n"] count];
    
    // Count @interface declarations
    NSInteger interfaces = [self countOccurrencesOf:@"@interface" inString:code];
    
    // Count @implementation blocks
    NSInteger implementations = [self countOccurrencesOf:@"@implementation" inString:code];
    
    // Count method declarations (- or + at start of line)
    NSRegularExpression *methodRegex = [NSRegularExpression regularExpressionWithPattern:@"^[\\s]*[-+]\\s*\\(" options:NSRegularExpressionAnchorsMatchLines error:nil];
    NSInteger methods = [methodRegex numberOfMatchesInString:code options:0 range:NSMakeRange(0, code.length)];
    
    // Count @property declarations
    NSInteger properties = [self countOccurrencesOf:@"@property" inString:code];
    
    return [[CodeAnalysis alloc] initWithLines:lines
                                    interfaces:interfaces
                               implementations:implementations
                                       methods:methods
                                    properties:properties];
}

- (NSDictionary *)checkQuality:(NSString *)code {
    NSMutableArray<NSString *> *issues = [NSMutableArray array];
    NSInteger score = 100;
    
    // Check for header guards
    if ([code containsString:@".h"] && ![code containsString:@"#ifndef"]) {
        [issues addObject:@"Header files should use include guards"];
        score -= 15;
    }
    
    // Check for documentation comments
    if ([code containsString:@"@interface"] && ![code containsString:@"/**"] && ![code containsString:@"///"]) {
        [issues addObject:@"Missing documentation comments"];
        score -= 10;
    }
    
    // Check for nullability annotations
    if ([code containsString:@"@property"] && ![code containsString:@"nullable"] && ![code containsString:@"nonnull"]) {
        [issues addObject:@"Missing nullability annotations"];
        score -= 10;
    }
    
    // Check for designated initializers
    if ([code containsString:@"@implementation"] && ![code containsString:@"NS_DESIGNATED_INITIALIZER"]) {
        [issues addObject:@"Consider marking designated initializers"];
        score -= 5;
    }
    
    return @{
        @"qualityScore": @(MAX(0, score)),
        @"issues": [issues copy],
        @"issueCount": @(issues.count)
    };
}

- (NSInteger)countOccurrencesOf:(NSString *)substring inString:(NSString *)string {
    NSInteger count = 0;
    NSRange range = NSMakeRange(0, string.length);
    
    while (range.location != NSNotFound) {
        range = [string rangeOfString:substring options:0 range:range];
        if (range.location != NSNotFound) {
            count++;
            range = NSMakeRange(range.location + range.length, string.length - (range.location + range.length));
        }
    }
    
    return count;
}

- (FloorAgent *)getInfo {
    return [[FloorAgent alloc] initWithAgentId:self.agentId
                                          name:self.name
                                          role:@"Operations Agent"
                                        office:@"Review Office"
                                  capabilities:self.capabilities];
}

@end

@implementation TestAgent {
    NSString *_agentId;
    NSString *_name;
    NSArray<NSString *> *_capabilities;
}

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name {
    self = [super init];
    if (self) {
        _agentId = [agentId copy];
        _name = [name copy];
        _capabilities = @[
            @"xctest_framework",
            @"unit_testing",
            @"ui_testing",
            @"test_coverage"
        ];
    }
    return self;
}

- (NSString *)agentId {
    return _agentId;
}

- (NSString *)name {
    return _name;
}

- (NSArray<NSString *> *)capabilities {
    return _capabilities;
}

- (NSDictionary *)analyzeTests:(NSString *)code {
    // Count XCTest cases
    NSInteger testCases = [self countOccurrencesOf:@": XCTestCase" inString:code];
    
    // Count test methods (methods starting with "test")
    NSRegularExpression *testMethodRegex = [NSRegularExpression regularExpressionWithPattern:@"^[\\s]*-\\s*\\(void\\)test" options:NSRegularExpressionAnchorsMatchLines error:nil];
    NSInteger testMethods = [testMethodRegex numberOfMatchesInString:code options:0 range:NSMakeRange(0, code.length)];
    
    // Count XCTAssert calls
    NSInteger assertions = [self countOccurrencesOf:@"XCTAssert" inString:code];
    
    // Count setUp/tearDown
    BOOL hasSetUp = [code containsString:@"- (void)setUp"];
    BOOL hasTearDown = [code containsString:@"- (void)tearDown"];
    
    return @{
        @"testCases": @(testCases),
        @"testMethods": @(testMethods),
        @"assertions": @(assertions),
        @"hasSetUp": @(hasSetUp),
        @"hasTearDown": @(hasTearDown),
        @"isXCTest": @(testCases > 0 || testMethods > 0)
    };
}

- (NSInteger)countOccurrencesOf:(NSString *)substring inString:(NSString *)string {
    NSInteger count = 0;
    NSRange range = NSMakeRange(0, string.length);
    
    while (range.location != NSNotFound) {
        range = [string rangeOfString:substring options:0 range:range];
        if (range.location != NSNotFound) {
            count++;
            range = NSMakeRange(range.location + range.length, string.length - (range.location + range.length));
        }
    }
    
    return count;
}

- (FloorAgent *)getInfo {
    return [[FloorAgent alloc] initWithAgentId:self.agentId
                                          name:self.name
                                          role:@"Test Agent"
                                        office:@"Test Office"
                                  capabilities:self.capabilities];
}

@end

@implementation SecurityAgent {
    NSString *_agentId;
    NSString *_name;
    NSArray<NSString *> *_capabilities;
}

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name {
    self = [super init];
    if (self) {
        _agentId = [agentId copy];
        _name = [name copy];
        _capabilities = @[
            @"memory_safety",
            @"buffer_overflow_detection",
            @"retain_cycle_detection",
            @"security_scanning"
        ];
    }
    return self;
}

- (NSString *)agentId {
    return _agentId;
}

- (NSString *)name {
    return _name;
}

- (NSArray<NSString *> *)capabilities {
    return _capabilities;
}

- (NSDictionary *)scanSecurity:(NSString *)code {
    NSMutableArray<NSDictionary *> *vulnerabilities = [NSMutableArray array];
    
    // Buffer overflow risks
    if ([code containsString:@"strcpy"] || [code containsString:@"strcat"] || [code containsString:@"sprintf"]) {
        [vulnerabilities addObject:@{
            @"severity": @"CRITICAL",
            @"type": @"buffer_overflow",
            @"description": @"Unsafe C string functions (strcpy/strcat/sprintf) - use strlcpy/strlcat/snprintf"
        }];
    }
    
    // Retain cycle risks
    if ([code containsString:@"self."] && [code containsString:@"^{"] && ![code containsString:@"__weak"]) {
        [vulnerabilities addObject:@{
            @"severity": @"HIGH",
            @"type": @"retain_cycle",
            @"description": @"Potential retain cycle: capturing self in block without __weak"
        }];
    }
    
    // Memory leak with manual memory management
    if ([code containsString:@"alloc"] && ![code containsString:@"autorelease"] && [code containsString:@"return"]) {
        [vulnerabilities addObject:@{
            @"severity": @"HIGH",
            @"type": @"memory_leak",
            @"description": @"Potential memory leak: allocated object may not be released"
        }];
    }
    
    // SQL injection
    if ([code containsString:@"sqlite3_exec"] && [code containsString:@"stringWithFormat"]) {
        [vulnerabilities addObject:@{
            @"severity": @"CRITICAL",
            @"type": @"sql_injection",
            @"description": @"Potential SQL injection: use parameterized queries"
        }];
    }
    
    // Insecure random
    if ([code containsString:@"rand()"] || [code containsString:@"random()"]) {
        [vulnerabilities addObject:@{
            @"severity": @"MEDIUM",
            @"type": @"weak_random",
            @"description": @"Weak random number generator - use SecRandomCopyBytes for security"
        }];
    }
    
    // Hardcoded credentials
    if ([code containsString:@"password"] && [code containsString:@"@\""]) {
        [vulnerabilities addObject:@{
            @"severity": @"HIGH",
            @"type": @"hardcoded_credentials",
            @"description": @"Potential hardcoded credentials detected"
        }];
    }
    
    return @{
        @"secure": @(vulnerabilities.count == 0),
        @"vulnerabilities": [vulnerabilities copy],
        @"vulnerabilityCount": @(vulnerabilities.count)
    };
}

- (FloorAgent *)getInfo {
    return [[FloorAgent alloc] initWithAgentId:self.agentId
                                          name:self.name
                                          role:@"Security Agent"
                                        office:@"Security Office"
                                  capabilities:self.capabilities];
}

@end

@implementation ManagerAgent {
    NSString *_agentId;
    NSString *_name;
    NSArray<NSString *> *_capabilities;
}

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name {
    self = [super init];
    if (self) {
        _agentId = [agentId copy];
        _name = [name copy];
        _capabilities = @[
            @"task_coordination",
            @"resource_allocation",
            @"workflow_management",
            @"priority_scheduling"
        ];
    }
    return self;
}

- (NSString *)agentId {
    return _agentId;
}

- (NSString *)name {
    return _name;
}

- (NSArray<NSString *> *)capabilities {
    return _capabilities;
}

- (NSDictionary *)manageWorkflow:(NSDictionary *)params {
    NSNumber *taskCount = params[@"taskCount"] ?: @0;
    
    return @{
        @"workflowStatus": @"active",
        @"tasksManaged": taskCount,
        @"priorityQueue": @[],
        @"resourceUtilization": @"optimal"
    };
}

- (FloorAgent *)getInfo {
    return [[FloorAgent alloc] initWithAgentId:self.agentId
                                          name:self.name
                                          role:@"Manager Agent"
                                        office:@"Manager Office"
                                  capabilities:self.capabilities];
}

@end

// ============================================================================
// MAIN DEPARTMENT FLOOR IMPLEMENTATION
// ============================================================================

@implementation ObjectiveCDepartmentFloor {
    ServiceAgent *_serviceAgent;
    DataModelAgent *_dataModelAgent;
    OperationsAgent *_operationsAgent;
    TestAgent *_testAgent;
    SecurityAgent *_securityAgent;
    ManagerAgent *_managerAgent;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _floorNumber = 14;
        _language = @"objective-c";
        _domain = @"Legacy Apple systems, macOS/iOS development, Darwin frameworks";
        _agents = [NSMutableDictionary dictionary];
        _tasks = [NSMutableDictionary dictionary];
        _offices = @[
            @"Architecture Office",
            @"Implementation Office",
            @"Review Office",
            @"Test Office",
            @"Security Office",
            @"Manager Office"
        ];
        
        // Initialize agents
        _serviceAgent = [[ServiceAgent alloc] initWithAgentId:@"service-001" name:@"CocoaServiceAgent"];
        _dataModelAgent = [[DataModelAgent alloc] initWithAgentId:@"data-001" name:@"CoreDataAgent"];
        _operationsAgent = [[OperationsAgent alloc] initWithAgentId:@"ops-001" name:@"CodeAnalysisAgent"];
        _testAgent = [[TestAgent alloc] initWithAgentId:@"test-001" name:@"XCTestAgent"];
        _securityAgent = [[SecurityAgent alloc] initWithAgentId:@"sec-001" name:@"MemorySafetyAgent"];
        _managerAgent = [[ManagerAgent alloc] initWithAgentId:@"mgr-001" name:@"WorkflowCoordinator"];
        
        // Register agents
        [self registerAgent:[_serviceAgent getInfo]];
        [self registerAgent:[_dataModelAgent getInfo]];
        [self registerAgent:[_operationsAgent getInfo]];
        [self registerAgent:[_testAgent getInfo]];
        [self registerAgent:[_securityAgent getInfo]];
        [self registerAgent:[_managerAgent getInfo]];
    }
    return self;
}

- (void)registerAgent:(FloorAgent *)agent {
    self.agents[agent.agentId] = agent;
}

- (NSDictionary *)addAgentWithId:(NSString *)agentId
                            name:(NSString *)name
                            role:(NSString *)role
                          office:(NSString *)office
                    capabilities:(NSArray<NSString *> *)capabilities {
    @try {
        FloorAgent *agent = [[FloorAgent alloc] initWithAgentId:agentId
                                                            name:name
                                                            role:role
                                                          office:office
                                                    capabilities:capabilities];
        self.agents[agentId] = agent;
        
        return @{
            @"status": @"success",
            @"agent": [agent toDictionary]
        };
    }
    @catch (NSException *exception) {
        return @{
            @"status": @"error",
            @"message": exception.reason ?: @"Unknown error"
        };
    }
}

- (NSDictionary *)createTaskWithId:(NSString *)taskId
                             title:(NSString *)title
                        assignedTo:(NSString *)assignedTo {
    @try {
        Task *task = [[Task alloc] initWithTaskId:taskId
                                            title:title
                                           status:@"pending"
                                       assignedTo:assignedTo];
        self.tasks[taskId] = task;
        
        return @{
            @"status": @"success",
            @"task": [task toDictionary]
        };
    }
    @catch (NSException *exception) {
        return @{
            @"status": @"error",
            @"message": exception.reason ?: @"Unknown error"
        };
    }
}

- (NSDictionary *)getFloorInfo {
    NSMutableArray *agentList = [NSMutableArray array];
    for (FloorAgent *agent in self.agents.allValues) {
        [agentList addObject:[agent toDictionary]];
    }
    
    NSMutableArray *taskList = [NSMutableArray array];
    for (Task *task in self.tasks.allValues) {
        [taskList addObject:[task toDictionary]];
    }
    
    return @{
        @"floorNumber": @(self.floorNumber),
        @"language": self.language,
        @"domain": self.domain,
        @"offices": self.offices,
        @"agentCount": @(self.agents.count),
        @"taskCount": @(self.tasks.count),
        @"agents": agentList,
        @"tasks": taskList
    };
}

- (NSDictionary *)processCode:(NSString *)code operation:(NSString *)operation {
    @try {
        if ([operation isEqualToString:@"analyze"]) {
            CodeAnalysis *analysis = [_operationsAgent analyzeCode:code];
            return @{
                @"status": @"success",
                @"analysis": [analysis toDictionary]
            };
        } else if ([operation isEqualToString:@"quality"]) {
            NSDictionary *quality = [_operationsAgent checkQuality:code];
            return @{
                @"status": @"success",
                @"quality": quality
            };
        } else if ([operation isEqualToString:@"security"]) {
            NSDictionary *security = [_securityAgent scanSecurity:code];
            return @{
                @"status": @"success",
                @"security": security
            };
        } else if ([operation isEqualToString:@"test_analysis"]) {
            NSDictionary *testAnalysis = [_testAgent analyzeTests:code];
            return @{
                @"status": @"success",
                @"testAnalysis": testAnalysis
            };
        } else {
            return @{
                @"status": @"error",
                @"message": [NSString stringWithFormat:@"Unknown operation: %@", operation]
            };
        }
    }
    @catch (NSException *exception) {
        return @{
            @"status": @"error",
            @"message": exception.reason ?: @"Unknown error"
        };
    }
}

- (NSDictionary *)handleRequest:(NSDictionary *)request {
    @try {
        NSString *method = request[@"method"];
        NSDictionary *params = request[@"params"] ?: @{};
        
        if ([method isEqualToString:@"get_info"]) {
            return [self getFloorInfo];
        } else if ([method isEqualToString:@"add_agent"]) {
            return [self addAgentWithId:params[@"agentId"]
                                   name:params[@"name"]
                                   role:params[@"role"]
                                 office:params[@"office"]
                           capabilities:params[@"capabilities"]];
        } else if ([method isEqualToString:@"create_task"]) {
            return [self createTaskWithId:params[@"taskId"]
                                    title:params[@"title"]
                               assignedTo:params[@"assignedTo"]];
        } else if ([method isEqualToString:@"process_code"]) {
            return [self processCode:params[@"code"] operation:params[@"operation"]];
        } else if ([method isEqualToString:@"execute_service"]) {
            NSError *error = nil;
            NSDictionary *result = [_serviceAgent executeService:params[@"serviceName"] params:params error:&error];
            if (error) {
                return @{
                    @"status": @"error",
                    @"message": error.localizedDescription
                };
            }
            return result ?: @{@"status": @"error", @"message": @"Service returned nil"};
        } else if ([method isEqualToString:@"process_data"]) {
            NSError *error = nil;
            NSDictionary *result = [_dataModelAgent processData:params[@"operation"] data:params[@"data"] error:&error];
            if (error) {
                return @{
                    @"status": @"error",
                    @"message": error.localizedDescription
                };
            }
            return result ?: @{@"status": @"error", @"message": @"Operation returned nil"};
        } else {
            return @{
                @"status": @"error",
                @"message": [NSString stringWithFormat:@"Unknown method: %@", method]
            };
        }
    }
    @catch (NSException *exception) {
        return @{
            @"status": @"error",
            @"message": exception.reason ?: @"Unknown error",
            @"exception": exception.name
        };
    }
}

@end

// ============================================================================
// MAIN ENTRY POINT - JSON-RPC SERVER
// ============================================================================

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        ObjectiveCDepartmentFloor *floor = [[ObjectiveCDepartmentFloor alloc] init];
        
        fprintf(stderr, "Objective-C Department Floor (Floor 14) - Ready\n");
        fprintf(stderr, "Domain: %s\n", [floor.domain UTF8String]);
        fprintf(stderr, "Offices: %s\n", [[floor.offices componentsJoinedByString:@", "] UTF8String]);
        fflush(stderr);
        
        // Process requests line by line from stdin
        char buffer[8192];
        while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
            @autoreleasepool {
                NSString *line = [NSString stringWithUTF8String:buffer];
                line = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
                
                if (line.length == 0) continue;
                
                NSError *error = nil;
                NSData *jsonData = [line dataUsingEncoding:NSUTF8StringEncoding];
                NSDictionary *request = [NSJSONSerialization JSONObjectWithData:jsonData options:0 error:&error];
                
                NSDictionary *response;
                if (error) {
                    response = @{
                        @"status": @"error",
                        @"message": [NSString stringWithFormat:@"Invalid JSON: %@", error.localizedDescription]
                    };
                } else {
                    response = [floor handleRequest:request];
                }
                
                NSData *responseData = [NSJSONSerialization dataWithJSONObject:response options:0 error:nil];
                if (responseData) {
                    NSString *responseString = [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding];
                    printf("%s\n", [responseString UTF8String]);
                    fflush(stdout);
                }
            }
        }
    }
    return 0;
}
