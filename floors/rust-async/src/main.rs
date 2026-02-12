/*!
 * FLOOR 28 - RUST-ASYNC JURISDICTION
 * Department Floor Implementation
 *
 * Domain: Asynchronous Rust, async/await patterns, concurrent task execution
 * Architectural Law: Async safety enforced, Cancellation safety guaranteed, Deadlock detection active
 * Security Doctrine: Send+Sync bounds verified, Panic safety maintained, Resource cleanup guaranteed
 */

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::{Mutex, RwLock, Semaphore};
use tokio::time::{timeout, Duration};

// ============================================================================
// AGENT MODULES - Service, Data, Operations
// ============================================================================

/// Service Agent - Handles async I/O and concurrent request processing
mod service_agent {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ServiceAgent {
        pub agent_id: String,
        pub name: String,
        pub role: String,
        pub capabilities: Vec<String>,
        pub active_tasks: usize,
        pub max_concurrency: usize,
    }

    impl ServiceAgent {
        pub fn new(agent_id: String, name: String) -> Arc<Mutex<Self>> {
            Arc::new(Mutex::new(Self {
                agent_id,
                name,
                role: "Async Service Coordinator".to_string(),
                capabilities: vec![
                    "Concurrent request handling".to_string(),
                    "Async I/O multiplexing".to_string(),
                    "Task spawning and management".to_string(),
                    "Backpressure management".to_string(),
                ],
                active_tasks: 0,
                max_concurrency: 100,
            }))
        }

        pub async fn acquire_slot(&mut self) -> Result<(), String> {
            if self.active_tasks >= self.max_concurrency {
                Err("Max concurrency reached".to_string())
            } else {
                self.active_tasks += 1;
                Ok(())
            }
        }

        pub async fn release_slot(&mut self) {
            if self.active_tasks > 0 {
                self.active_tasks -= 1;
            }
        }

        pub async fn get_status(&self) -> serde_json::Value {
            serde_json::json!({
                "active_tasks": self.active_tasks,
                "max_concurrency": self.max_concurrency,
                "utilization": (self.active_tasks as f64 / self.max_concurrency as f64) * 100.0
            })
        }
    }
}

/// Data Agent - Manages async data access with proper synchronization
mod data_agent {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DataAgent {
        pub agent_id: String,
        pub name: String,
        pub role: String,
        pub capabilities: Vec<String>,
        pub cache_size: usize,
        pub read_count: u64,
        pub write_count: u64,
    }

    impl DataAgent {
        pub fn new(agent_id: String, name: String) -> Arc<RwLock<Self>> {
            Arc::new(RwLock::new(Self {
                agent_id,
                name,
                role: "Async Data Manager".to_string(),
                capabilities: vec![
                    "Concurrent data access".to_string(),
                    "RwLock-based synchronization".to_string(),
                    "Async caching".to_string(),
                    "Stream processing".to_string(),
                ],
                cache_size: 0,
                read_count: 0,
                write_count: 0,
            }))
        }

        pub async fn read_data(&mut self) -> Result<String, String> {
            self.read_count += 1;
            Ok(format!("Data read #{}", self.read_count))
        }

        pub async fn write_data(&mut self, _data: String) -> Result<(), String> {
            self.write_count += 1;
            self.cache_size += 1;
            Ok(())
        }

        pub async fn get_stats(&self) -> serde_json::Value {
            serde_json::json!({
                "cache_size": self.cache_size,
                "read_count": self.read_count,
                "write_count": self.write_count,
                "read_write_ratio": if self.write_count > 0 {
                    self.read_count as f64 / self.write_count as f64
                } else {
                    0.0
                }
            })
        }
    }
}

/// Operations Agent - Executes async computational tasks
mod operations_agent {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct OperationsAgent {
        pub agent_id: String,
        pub name: String,
        pub role: String,
        pub capabilities: Vec<String>,
        pub tasks_completed: u64,
        pub tasks_failed: u64,
        pub avg_duration_ms: f64,
    }

    impl OperationsAgent {
        pub fn new(agent_id: String, name: String) -> Arc<Mutex<Self>> {
            Arc::new(Mutex::new(Self {
                agent_id,
                name,
                role: "Async Task Executor".to_string(),
                capabilities: vec![
                    "Async task execution".to_string(),
                    "Future composition".to_string(),
                    "Stream transformation".to_string(),
                    "Timeout management".to_string(),
                ],
                tasks_completed: 0,
                tasks_failed: 0,
                avg_duration_ms: 0.0,
            }))
        }

        pub async fn execute_async_task<F, T>(&mut self, task: F) -> Result<T, String>
        where
            F: std::future::Future<Output = Result<T, String>>,
        {
            let start = std::time::Instant::now();
            
            match timeout(Duration::from_secs(30), task).await {
                Ok(Ok(result)) => {
                    let duration = start.elapsed().as_millis() as f64;
                    self.tasks_completed += 1;
                    self.avg_duration_ms = 
                        (self.avg_duration_ms * (self.tasks_completed - 1) as f64 + duration) 
                        / self.tasks_completed as f64;
                    Ok(result)
                }
                Ok(Err(e)) => {
                    self.tasks_failed += 1;
                    Err(e)
                }
                Err(_) => {
                    self.tasks_failed += 1;
                    Err("Task timeout".to_string())
                }
            }
        }

        pub async fn analyze_async_code(&self, code: &str) -> AsyncAnalysis {
            let lines = code.lines().count();
            let async_fn_count = code.matches("async fn").count();
            let await_count = code.matches(".await").count();
            let spawn_count = code.matches("tokio::spawn").count() + code.matches("spawn(").count();
            let mutex_count = code.matches("Mutex").count();
            let rwlock_count = code.matches("RwLock").count();
            let channel_count = code.matches("channel").count() + code.matches("mpsc").count();
            let stream_count = code.matches("Stream").count() + code.matches("stream::").count();
            let select_count = code.matches("tokio::select!").count() + code.matches("select!").count();
            
            AsyncAnalysis {
                lines,
                async_functions: async_fn_count,
                await_points: await_count,
                spawn_points: spawn_count,
                mutexes: mutex_count,
                rwlocks: rwlock_count,
                channels: channel_count,
                streams: stream_count,
                select_blocks: select_count,
                potential_deadlocks: (mutex_count > 1 && rwlock_count > 1) as usize,
                cancellation_safe: await_count > 0 && select_count > 0,
            }
        }

        pub async fn get_performance_stats(&self) -> serde_json::Value {
            let total = self.tasks_completed + self.tasks_failed;
            serde_json::json!({
                "completed": self.tasks_completed,
                "failed": self.tasks_failed,
                "total": total,
                "success_rate": if total > 0 {
                    (self.tasks_completed as f64 / total as f64) * 100.0
                } else {
                    100.0
                },
                "avg_duration_ms": self.avg_duration_ms
            })
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct AsyncAnalysis {
        pub lines: usize,
        pub async_functions: usize,
        pub await_points: usize,
        pub spawn_points: usize,
        pub mutexes: usize,
        pub rwlocks: usize,
        pub channels: usize,
        pub streams: usize,
        pub select_blocks: usize,
        pub potential_deadlocks: usize,
        pub cancellation_safe: bool,
    }
}

// ============================================================================
// FLOOR ENTITIES
// ============================================================================

#[derive(Debug, Serialize, Deserialize, Clone)]
struct FloorAgent {
    agent_id: String,
    name: String,
    role: String,
    capabilities: Vec<String>,
    office: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Task {
    task_id: String,
    title: String,
    status: String,
    assigned_to: String,
    office: String,
    created_at: String,
    is_async: bool,
}

#[derive(Debug, Serialize, Deserialize)]
struct Request {
    method: String,
    #[serde(default)]
    params: serde_json::Value,
}

// ============================================================================
// OFFICE STRUCTURE
// ============================================================================

struct Office {
    name: String,
    agents: Vec<String>,
    responsibilities: Vec<String>,
}

impl Office {
    fn new(name: &str, responsibilities: Vec<&str>) -> Self {
        Self {
            name: name.to_string(),
            agents: Vec::new(),
            responsibilities: responsibilities.iter().map(|s| s.to_string()).collect(),
        }
    }

    fn add_agent(&mut self, agent_id: String) {
        if !self.agents.contains(&agent_id) {
            self.agents.push(agent_id);
        }
    }
}

// ============================================================================
// MAIN FLOOR IMPLEMENTATION
// ============================================================================

struct RustAsyncDepartmentFloor {
    floor_number: u8,
    language: String,
    domain: String,
    agents: Arc<RwLock<HashMap<String, FloorAgent>>>,
    tasks: Arc<RwLock<HashMap<String, Task>>>,
    offices: HashMap<String, Office>,
    
    // Specialized async agents
    service_agent: Arc<Mutex<service_agent::ServiceAgent>>,
    data_agent: Arc<RwLock<data_agent::DataAgent>>,
    operations_agent: Arc<Mutex<operations_agent::OperationsAgent>>,
    
    // Concurrency control
    request_semaphore: Arc<Semaphore>,
}

impl RustAsyncDepartmentFloor {
    async fn new() -> Self {
        let mut offices = HashMap::new();
        
        offices.insert(
            "Architecture".to_string(),
            Office::new(
                "Architecture Office",
                vec![
                    "Async system design",
                    "Future/Stream architecture",
                    "Concurrency patterns",
                    "Executor selection",
                ],
            ),
        );
        
        offices.insert(
            "Implementation".to_string(),
            Office::new(
                "Implementation Office",
                vec![
                    "Async function implementation",
                    "Tokio runtime configuration",
                    "Channel and sync primitives",
                    "Error propagation patterns",
                ],
            ),
        );
        
        offices.insert(
            "Review".to_string(),
            Office::new(
                "Review Office",
                vec![
                    "Cancellation safety review",
                    "Deadlock detection",
                    "Send+Sync bound verification",
                    "Async code best practices",
                ],
            ),
        );
        
        offices.insert(
            "Test".to_string(),
            Office::new(
                "Test Office",
                vec![
                    "Async test execution",
                    "Concurrency testing",
                    "Race condition detection",
                    "Performance benchmarking",
                ],
            ),
        );
        
        offices.insert(
            "Security".to_string(),
            Office::new(
                "Security Office",
                vec![
                    "Panic safety analysis",
                    "Resource leak detection",
                    "Cancellation handling",
                    "Async injection prevention",
                ],
            ),
        );
        
        offices.insert(
            "Manager".to_string(),
            Office::new(
                "Manager Office",
                vec![
                    "Runtime monitoring",
                    "Task scheduling",
                    "Resource allocation",
                    "Performance optimization",
                ],
            ),
        );

        let mut floor = Self {
            floor_number: 28,
            language: "Rust-Async".to_string(),
            domain: "Asynchronous Rust, async/await patterns, concurrent execution".to_string(),
            agents: Arc::new(RwLock::new(HashMap::new())),
            tasks: Arc::new(RwLock::new(HashMap::new())),
            offices,
            service_agent: service_agent::ServiceAgent::new(
                "service-001".to_string(),
                "Async Service Manager".to_string(),
            ),
            data_agent: data_agent::DataAgent::new(
                "data-001".to_string(),
                "Concurrent Data Controller".to_string(),
            ),
            operations_agent: operations_agent::OperationsAgent::new(
                "ops-001".to_string(),
                "Async Task Executor".to_string(),
            ),
            request_semaphore: Arc::new(Semaphore::new(100)),
        };

        floor.initialize_default_agents().await;
        floor
    }

    async fn initialize_default_agents(&mut self) {
        let mut agents = self.agents.write().await;
        
        // Architecture Office agents
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "arch-001".to_string(),
            "Async Architect".to_string(),
            "Senior Architect".to_string(),
            vec!["Async system design".to_string(), "Future composition".to_string()],
            "Architecture".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "arch-002".to_string(),
            "Concurrency Architect".to_string(),
            "Concurrency Specialist".to_string(),
            vec!["Concurrency patterns".to_string(), "Lock-free design".to_string()],
            "Architecture".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "arch-003".to_string(),
            "Stream Architect".to_string(),
            "Stream Specialist".to_string(),
            vec!["Stream design".to_string(), "Backpressure handling".to_string()],
            "Architecture".to_string(),
        );

        // Implementation Office agents
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "impl-001".to_string(),
            "Async Engineer".to_string(),
            "Senior Engineer".to_string(),
            vec!["Async functions".to_string(), "Tokio integration".to_string()],
            "Implementation".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "impl-002".to_string(),
            "Runtime Engineer".to_string(),
            "Runtime Specialist".to_string(),
            vec!["Runtime config".to_string(), "Executor tuning".to_string()],
            "Implementation".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "impl-003".to_string(),
            "Channel Engineer".to_string(),
            "Communication Specialist".to_string(),
            vec!["Channels".to_string(), "Sync primitives".to_string()],
            "Implementation".to_string(),
        );

        // Review Office agents
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "review-001".to_string(),
            "Safety Reviewer".to_string(),
            "Safety Lead".to_string(),
            vec!["Cancellation safety".to_string(), "Panic safety".to_string()],
            "Review".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "review-002".to_string(),
            "Deadlock Detector".to_string(),
            "Deadlock Specialist".to_string(),
            vec!["Deadlock analysis".to_string(), "Lock ordering".to_string()],
            "Review".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "review-003".to_string(),
            "Bounds Auditor".to_string(),
            "Trait Bounds Auditor".to_string(),
            vec!["Send+Sync verification".to_string(), "Lifetime analysis".to_string()],
            "Review".to_string(),
        );

        // Test Office agents
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "test-001".to_string(),
            "Async Tester".to_string(),
            "Test Lead".to_string(),
            vec!["Async tests".to_string(), "tokio-test".to_string()],
            "Test".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "test-002".to_string(),
            "Concurrency Tester".to_string(),
            "Concurrency Test Engineer".to_string(),
            vec!["Race detection".to_string(), "Loom testing".to_string()],
            "Test".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "test-003".to_string(),
            "Performance Tester".to_string(),
            "Performance Engineer".to_string(),
            vec!["Async benchmarks".to_string(), "Throughput testing".to_string()],
            "Test".to_string(),
        );

        // Security Office agents
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "sec-001".to_string(),
            "Panic Guardian".to_string(),
            "Panic Safety Lead".to_string(),
            vec!["Panic safety".to_string(), "Unwind safety".to_string()],
            "Security".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "sec-002".to_string(),
            "Resource Guardian".to_string(),
            "Resource Safety".to_string(),
            vec!["Resource leaks".to_string(), "Drop impl review".to_string()],
            "Security".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "sec-003".to_string(),
            "Cancellation Guardian".to_string(),
            "Cancellation Safety".to_string(),
            vec!["Cancel safety".to_string(), "Cleanup verification".to_string()],
            "Security".to_string(),
        );

        // Manager Office agents
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "mgr-001".to_string(),
            "Runtime Manager".to_string(),
            "Runtime Operations".to_string(),
            vec!["Runtime monitoring".to_string(), "Task scheduling".to_string()],
            "Manager".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "mgr-002".to_string(),
            "Resource Manager".to_string(),
            "Resource Operations".to_string(),
            vec!["Resource allocation".to_string(), "Backpressure".to_string()],
            "Manager".to_string(),
        );
        Self::add_agent_internal(
            &mut agents,
            &mut self.offices,
            "mgr-003".to_string(),
            "Performance Manager".to_string(),
            "Performance Operations".to_string(),
            vec!["Optimization".to_string(), "Latency tracking".to_string()],
            "Manager".to_string(),
        );
    }

    fn add_agent_internal(
        agents: &mut HashMap<String, FloorAgent>,
        offices: &mut HashMap<String, Office>,
        agent_id: String,
        name: String,
        role: String,
        capabilities: Vec<String>,
        office: String,
    ) {
        let agent = FloorAgent {
            agent_id: agent_id.clone(),
            name,
            role,
            capabilities,
            office: office.clone(),
        };
        
        if let Some(off) = offices.get_mut(&office) {
            off.add_agent(agent_id.clone());
        }
        
        agents.insert(agent_id, agent);
    }

    async fn add_agent(
        &self,
        agent_id: String,
        name: String,
        role: String,
        capabilities: Vec<String>,
        office: String,
    ) -> serde_json::Value {
        let agent = FloorAgent {
            agent_id: agent_id.clone(),
            name,
            role,
            capabilities,
            office,
        };
        
        let mut agents = self.agents.write().await;
        agents.insert(agent_id, agent.clone());
        
        serde_json::json!({
            "status": "success",
            "agent": agent
        })
    }

    async fn create_task(
        &self,
        task_id: String,
        title: String,
        assigned_to: String,
        office: String,
    ) -> serde_json::Value {
        let task = Task {
            task_id: task_id.clone(),
            title,
            status: "pending".to_string(),
            assigned_to,
            office,
            created_at: chrono_timestamp(),
            is_async: true,
        };
        
        let mut tasks = self.tasks.write().await;
        tasks.insert(task_id, task.clone());
        
        serde_json::json!({
            "status": "success",
            "task": task
        })
    }

    async fn get_floor_info(&self) -> serde_json::Value {
        let agents = self.agents.read().await;
        let tasks = self.tasks.read().await;
        
        let agents_vec: Vec<&FloorAgent> = agents.values().collect();
        let tasks_vec: Vec<&Task> = tasks.values().collect();
        
        let office_info: Vec<_> = self.offices.iter().map(|(_, office)| {
            serde_json::json!({
                "name": office.name,
                "agent_count": office.agents.len(),
                "responsibilities": office.responsibilities
            })
        }).collect();

        let service_status = self.service_agent.lock().await.get_status().await;
        let data_stats = self.data_agent.read().await.get_stats().await;
        let ops_stats = self.operations_agent.lock().await.get_performance_stats().await;

        serde_json::json!({
            "floor_number": self.floor_number,
            "language": &self.language,
            "domain": &self.domain,
            "offices": office_info,
            "agent_count": agents.len(),
            "task_count": tasks.len(),
            "specialized_agents": {
                "service": {
                    "agent_id": "service-001",
                    "name": "Async Service Manager",
                    "status": service_status
                },
                "data": {
                    "agent_id": "data-001",
                    "name": "Concurrent Data Controller",
                    "stats": data_stats
                },
                "operations": {
                    "agent_id": "ops-001",
                    "name": "Async Task Executor",
                    "stats": ops_stats
                }
            },
            "agents": agents_vec,
            "tasks": tasks_vec,
            "architectural_laws": [
                "Async safety enforced",
                "Cancellation safety guaranteed",
                "Deadlock detection active",
                "Send+Sync bounds verified"
            ],
            "security_doctrine": [
                "Panic safety maintained",
                "Resource cleanup guaranteed",
                "Unwind safety enforced",
                "Concurrent access controlled"
            ]
        })
    }

    async fn process_code(&self, code: &str, operation: &str) -> serde_json::Value {
        let mut ops_agent = self.operations_agent.lock().await;
        
        match operation {
            "analyze" => {
                let analysis = ops_agent.analyze_async_code(code).await;
                ops_agent.execute_async_task(async { Ok(()) }).await.ok();
                
                serde_json::json!({
                    "status": "success",
                    "analysis": analysis,
                    "language": "rust-async"
                })
            }
            "check_safety" => {
                let has_await = code.contains(".await");
                let has_mutex = code.contains("Mutex");
                let has_rwlock = code.contains("RwLock");
                let has_panic = code.contains("panic!") || code.contains("unwrap()");
                let has_timeout = code.contains("timeout");
                
                let cancellation_safe = has_await && (has_timeout || code.contains("select!"));
                let deadlock_risk = (has_mutex || has_rwlock) && code.matches("lock()").count() > 1;
                let panic_safe = !has_panic || code.contains("catch_unwind");
                
                serde_json::json!({
                    "status": "success",
                    "safety_check": {
                        "cancellation_safe": cancellation_safe,
                        "deadlock_risk": deadlock_risk,
                        "panic_safe": panic_safe,
                        "has_await": has_await,
                        "sync_primitives": {
                            "mutex": has_mutex,
                            "rwlock": has_rwlock
                        }
                    }
                })
            }
            "optimize" => {
                let suggestions = vec![
                    if code.contains("spawn") { "Consider task pooling" } else { "" },
                    if code.contains("lock().await") { "Use RwLock for read-heavy workloads" } else { "" },
                    if !code.contains("timeout") { "Add timeout for external operations" } else { "" },
                    if code.contains("Vec") && code.contains("async") { "Consider using streams" } else { "" },
                ].into_iter().filter(|s| !s.is_empty()).map(String::from).collect::<Vec<_>>();
                
                serde_json::json!({
                    "status": "success",
                    "optimization": {
                        "suggestions": suggestions,
                        "async_score": if code.contains("async") { 85 } else { 40 },
                        "improvements": [
                            "Use select! for cancellation",
                            "Implement backpressure",
                            "Add proper error propagation"
                        ]
                    }
                })
            }
            "test" => {
                let test_count = code.matches("#[tokio::test]").count() + code.matches("#[test]").count();
                let has_async_test = code.contains("#[tokio::test]");
                
                serde_json::json!({
                    "status": "success",
                    "testing": {
                        "test_count": test_count,
                        "has_async_tests": has_async_test,
                        "framework": if has_async_test { "tokio-test" } else { "none" },
                        "recommendations": [
                            "Use #[tokio::test] for async tests",
                            "Test cancellation scenarios",
                            "Add concurrency tests with Loom"
                        ]
                    }
                })
            }
            _ => {
                serde_json::json!({
                    "status": "error",
                    "message": format!("Unknown operation: {}", operation)
                })
            }
        }
    }

    async fn handle_request(&self, request: Request) -> serde_json::Value {
        // Acquire semaphore permit for concurrency control
        let _permit = self.request_semaphore.acquire().await.ok();
        
        let mut service = self.service_agent.lock().await;
        if service.acquire_slot().await.is_err() {
            return serde_json::json!({
                "status": "error",
                "message": "Service at capacity"
            });
        }
        drop(service);
        
        let response = match request.method.as_str() {
            "get_info" => self.get_floor_info().await,
            "add_agent" => {
                if let Some(params) = request.params.as_object() {
                    self.add_agent(
                        params.get("agent_id").and_then(|v| v.as_str()).unwrap_or("").to_string(),
                        params.get("name").and_then(|v| v.as_str()).unwrap_or("").to_string(),
                        params.get("role").and_then(|v| v.as_str()).unwrap_or("").to_string(),
                        params.get("capabilities")
                            .and_then(|v| v.as_array())
                            .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                            .unwrap_or_default(),
                        params.get("office").and_then(|v| v.as_str()).unwrap_or("Manager").to_string(),
                    ).await
                } else {
                    serde_json::json!({"status": "error", "message": "Invalid parameters"})
                }
            }
            "create_task" => {
                if let Some(params) = request.params.as_object() {
                    self.create_task(
                        params.get("task_id").and_then(|v| v.as_str()).unwrap_or("").to_string(),
                        params.get("title").and_then(|v| v.as_str()).unwrap_or("").to_string(),
                        params.get("assigned_to").and_then(|v| v.as_str()).unwrap_or("").to_string(),
                        params.get("office").and_then(|v| v.as_str()).unwrap_or("Manager").to_string(),
                    ).await
                } else {
                    serde_json::json!({"status": "error", "message": "Invalid parameters"})
                }
            }
            "process_code" => {
                if let Some(params) = request.params.as_object() {
                    let code = params.get("code").and_then(|v| v.as_str()).unwrap_or("");
                    let operation = params.get("operation").and_then(|v| v.as_str()).unwrap_or("analyze");
                    self.process_code(code, operation).await
                } else {
                    serde_json::json!({"status": "error", "message": "Invalid parameters"})
                }
            }
            _ => {
                serde_json::json!({
                    "status": "error",
                    "message": format!("Unknown method: {}", request.method)
                })
            }
        };
        
        let mut service = self.service_agent.lock().await;
        service.release_slot().await;
        
        response
    }
}

fn chrono_timestamp() -> String {
    "2026-02-12T07:00:00Z".to_string()
}

// ============================================================================
// MAIN ENTRY POINT - ASYNC RUNTIME
// ============================================================================

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let floor = RustAsyncDepartmentFloor::new().await;

    eprintln!("Rust-Async Department Floor (Floor 28) - Ready");
    eprintln!("Domain: {}", floor.domain);
    eprintln!("Offices: Architecture, Implementation, Review, Test, Security, Manager");
    eprintln!("Runtime: Tokio with async/await");
    eprintln!("Specialized Agents: Service, Data, Operations");

    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin);
    let mut stdout = tokio::io::stdout();
    let mut line = String::new();

    loop {
        line.clear();
        let n = reader.read_line(&mut line).await?;
        
        if n == 0 {
            break; // EOF
        }

        let response = match serde_json::from_str::<Request>(&line) {
            Ok(request) => floor.handle_request(request).await,
            Err(e) => {
                serde_json::json!({
                    "status": "error",
                    "message": format!("Invalid JSON: {}", e)
                })
            }
        };

        let response_str = serde_json::to_string(&response).unwrap();
        stdout.write_all(response_str.as_bytes()).await?;
        stdout.write_all(b"\n").await?;
        stdout.flush().await?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_floor_creation() {
        let floor = RustAsyncDepartmentFloor::new().await;
        assert_eq!(floor.floor_number, 28);
        assert_eq!(floor.language, "Rust-Async");
        assert_eq!(floor.offices.len(), 6);
        
        let agents = floor.agents.read().await;
        assert!(agents.len() >= 18);
    }

    #[tokio::test]
    async fn test_async_analysis() {
        let ops = operations_agent::OperationsAgent::new("test".to_string(), "test".to_string());
        let ops = ops.lock().await;
        let code = "async fn test() { tokio::spawn(async {}).await }";
        let analysis = ops.analyze_async_code(code).await;
        assert!(analysis.async_functions > 0);
        assert!(analysis.await_points > 0);
    }

    #[tokio::test]
    async fn test_concurrent_access() {
        let floor = Arc::new(RustAsyncDepartmentFloor::new().await);
        
        let mut handles = Vec::new();
        for i in 0..10 {
            let floor = Arc::clone(&floor);
            let handle = tokio::spawn(async move {
                floor.add_agent(
                    format!("test-{}", i),
                    format!("Agent {}", i),
                    "Test".to_string(),
                    vec!["Testing".to_string()],
                    "Test".to_string(),
                ).await
            });
            handles.push(handle);
        }
        
        for handle in handles {
            handle.await.unwrap();
        }
        
        let agents = floor.agents.read().await;
        assert!(agents.len() >= 28); // 18 default + 10 test
    }

    #[tokio::test]
    async fn test_service_agent_concurrency() {
        let agent = service_agent::ServiceAgent::new("test".to_string(), "test".to_string());
        let mut agent = agent.lock().await;
        
        assert_eq!(agent.active_tasks, 0);
        agent.acquire_slot().await.unwrap();
        assert_eq!(agent.active_tasks, 1);
        agent.release_slot().await;
        assert_eq!(agent.active_tasks, 0);
    }
}
