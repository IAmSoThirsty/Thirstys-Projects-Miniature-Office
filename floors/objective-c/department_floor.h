//
// department_floor.h
// Floor 14 - Objective-C Jurisdiction
//
// Domain: Legacy Apple systems, macOS/iOS development, Darwin frameworks
// Architectural Law: MRC/ARC patterns, Message passing, Memory management
// Security Doctrine: Memory safety, Retain cycles, Buffer overflows
//

#import <Foundation/Foundation.h>

// ============================================================================
// CORE DATA MODELS
// ============================================================================

@interface FloorAgent : NSObject

@property (nonatomic, copy) NSString *agentId;
@property (nonatomic, copy) NSString *name;
@property (nonatomic, copy) NSString *role;
@property (nonatomic, copy) NSString *office;
@property (nonatomic, strong) NSArray<NSString *> *capabilities;

- (instancetype)initWithAgentId:(NSString *)agentId
                           name:(NSString *)name
                           role:(NSString *)role
                         office:(NSString *)office
                   capabilities:(NSArray<NSString *> *)capabilities;

- (NSDictionary *)toDictionary;

@end

@interface Task : NSObject

@property (nonatomic, copy) NSString *taskId;
@property (nonatomic, copy) NSString *title;
@property (nonatomic, copy) NSString *status;
@property (nonatomic, copy) NSString *assignedTo;
@property (nonatomic, strong) NSDate *createdAt;
@property (nonatomic, strong, nullable) NSDate *completedAt;

- (instancetype)initWithTaskId:(NSString *)taskId
                         title:(NSString *)title
                        status:(NSString *)status
                    assignedTo:(NSString *)assignedTo;

- (NSDictionary *)toDictionary;

@end

@interface CodeAnalysis : NSObject

@property (nonatomic, assign) NSInteger lines;
@property (nonatomic, assign) NSInteger interfaces;
@property (nonatomic, assign) NSInteger implementations;
@property (nonatomic, assign) NSInteger methods;
@property (nonatomic, assign) NSInteger properties;
@property (nonatomic, copy) NSString *language;

- (instancetype)initWithLines:(NSInteger)lines
                   interfaces:(NSInteger)interfaces
              implementations:(NSInteger)implementations
                      methods:(NSInteger)methods
                   properties:(NSInteger)properties;

- (NSDictionary *)toDictionary;

@end

// ============================================================================
// AGENT PROTOCOLS AND CLASSES
// ============================================================================

@protocol Agent <NSObject>
- (FloorAgent *)getInfo;
@end

// Service Agent - Implementation Office
@interface ServiceAgent : NSObject <Agent>

@property (nonatomic, copy, readonly) NSString *agentId;
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, strong, readonly) NSArray<NSString *> *capabilities;

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name;
- (NSDictionary *)executeService:(NSString *)serviceName params:(NSDictionary *)params error:(NSError **)error;

@end

// Data Model Agent - Architecture Office
@interface DataModelAgent : NSObject <Agent>

@property (nonatomic, copy, readonly) NSString *agentId;
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, strong, readonly) NSArray<NSString *> *capabilities;

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name;
- (NSDictionary *)processData:(NSString *)operation data:(id)data error:(NSError **)error;

@end

// Operations Agent - Review Office
@interface OperationsAgent : NSObject <Agent>

@property (nonatomic, copy, readonly) NSString *agentId;
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, strong, readonly) NSArray<NSString *> *capabilities;

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name;
- (CodeAnalysis *)analyzeCode:(NSString *)code;
- (NSDictionary *)checkQuality:(NSString *)code;

@end

// Test Agent - Test Office
@interface TestAgent : NSObject <Agent>

@property (nonatomic, copy, readonly) NSString *agentId;
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, strong, readonly) NSArray<NSString *> *capabilities;

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name;
- (NSDictionary *)analyzeTests:(NSString *)code;

@end

// Security Agent - Security Office
@interface SecurityAgent : NSObject <Agent>

@property (nonatomic, copy, readonly) NSString *agentId;
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, strong, readonly) NSArray<NSString *> *capabilities;

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name;
- (NSDictionary *)scanSecurity:(NSString *)code;

@end

// Manager Agent - Manager Office
@interface ManagerAgent : NSObject <Agent>

@property (nonatomic, copy, readonly) NSString *agentId;
@property (nonatomic, copy, readonly) NSString *name;
@property (nonatomic, strong, readonly) NSArray<NSString *> *capabilities;

- (instancetype)initWithAgentId:(NSString *)agentId name:(NSString *)name;
- (NSDictionary *)manageWorkflow:(NSDictionary *)params;

@end

// ============================================================================
// MAIN DEPARTMENT FLOOR CLASS
// ============================================================================

@interface ObjectiveCDepartmentFloor : NSObject

@property (nonatomic, assign, readonly) NSInteger floorNumber;
@property (nonatomic, copy, readonly) NSString *language;
@property (nonatomic, copy, readonly) NSString *domain;
@property (nonatomic, strong, readonly) NSMutableDictionary<NSString *, FloorAgent *> *agents;
@property (nonatomic, strong, readonly) NSMutableDictionary<NSString *, Task *> *tasks;
@property (nonatomic, strong, readonly) NSArray<NSString *> *offices;

- (instancetype)init;
- (NSDictionary *)addAgentWithId:(NSString *)agentId
                            name:(NSString *)name
                            role:(NSString *)role
                          office:(NSString *)office
                    capabilities:(NSArray<NSString *> *)capabilities;
- (NSDictionary *)createTaskWithId:(NSString *)taskId
                             title:(NSString *)title
                        assignedTo:(NSString *)assignedTo;
- (NSDictionary *)getFloorInfo;
- (NSDictionary *)processCode:(NSString *)code operation:(NSString *)operation;
- (NSDictionary *)handleRequest:(NSDictionary *)request;

@end
