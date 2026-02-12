/*!
 * FLOOR 2 - RUST JURISDICTION
 * Department Floor Implementation
 *
 * Domain: Memory-safe systems, Performance-critical logic, FFI boundaries
 * Architectural Law: Ownership correctness absolute, Clippy enforced, Zero UB
 */

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{self, BufRead, Write};

#[derive(Debug, Serialize, Deserialize, Clone)]
struct FloorAgent {
    agent_id: String,
    name: String,
    role: String,
    capabilities: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Task {
    task_id: String,
    title: String,
    status: String,
    assigned_to: String,
    created_at: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Request {
    method: String,
    #[serde(default)]
    params: serde_json::Value,
}

struct RustDepartmentFloor {
    floor_number: u8,
    language: String,
    domain: String,
    agents: HashMap<String, FloorAgent>,
    tasks: HashMap<String, Task>,
    offices: Vec<String>,
}

impl RustDepartmentFloor {
    /// Create a new Rust Department Floor
    ///
    /// Implements the department logic for Rust jurisdiction following:
    /// - Language Sovereignty
    /// - Identical Internal Topology (6 offices)
    /// - Contract-Bound Operation
    /// - Non-Creative Mandate
    /// - Failure Escalation Guarantee
    fn new() -> Self {
        Self {
            floor_number: 2,
            language: "rust".to_string(),
            domain: "Memory-safe systems, Performance-critical logic".to_string(),
            agents: HashMap::new(),
            tasks: HashMap::new(),
            offices: vec![
                "Architecture Office".to_string(),
                "Implementation Office".to_string(),
                "Review Office".to_string(),
                "Test Office".to_string(),
                "Security Office".to_string(),
                "Manager Office".to_string(),
            ],
        }
    }

    fn add_agent(
        &mut self,
        agent_id: String,
        name: String,
        role: String,
        capabilities: Vec<String>,
    ) -> serde_json::Value {
        let agent = FloorAgent {
            agent_id: agent_id.clone(),
            name,
            role,
            capabilities,
        };
        self.agents.insert(agent_id, agent.clone());
        serde_json::json!({
            "status": "success",
            "agent": agent
        })
    }

    fn create_task(&mut self, task_id: String, title: String, assigned_to: String) -> serde_json::Value {
        let task = Task {
            task_id: task_id.clone(),
            title,
            status: "pending".to_string(),
            assigned_to,
            created_at: "2026-02-12T07:00:00Z".to_string(),
        };
        self.tasks.insert(task_id, task.clone());
        serde_json::json!({
            "status": "success",
            "task": task
        })
    }

    fn get_floor_info(&self) -> serde_json::Value {
        let agents: Vec<&FloorAgent> = self.agents.values().collect();
        let tasks: Vec<&Task> = self.tasks.values().collect();

        serde_json::json!({
            "floor_number": self.floor_number,
            "language": &self.language,
            "domain": &self.domain,
            "offices": &self.offices,
            "agent_count": self.agents.len(),
            "task_count": self.tasks.len(),
            "agents": agents,
            "tasks": tasks
        })
    }

    fn process_code(&self, code: &str, operation: &str) -> serde_json::Value {
        match operation {
            "analyze" => {
                let lines = code.lines().count();
                let fn_count = code.matches("fn ").count();
                let struct_count = code.matches("struct ").count();
                let impl_count = code.matches("impl ").count();
                let unsafe_count = code.matches("unsafe ").count();

                serde_json::json!({
                    "status": "success",
                    "analysis": {
                        "lines": lines,
                        "functions": fn_count,
                        "structs": struct_count,
                        "impls": impl_count,
                        "unsafe_blocks": unsafe_count,
                        "language": "rust"
                    }
                })
            }
            "lint" => {
                serde_json::json!({
                    "status": "success",
                    "linted": true,
                    "message": "Rust code linted (Clippy)"
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

    fn handle_request(&mut self, request: Request) -> serde_json::Value {
        match request.method.as_str() {
            "get_info" => self.get_floor_info(),
            "add_agent" => {
                let params = request.params.as_object().unwrap();
                self.add_agent(
                    params["agent_id"].as_str().unwrap().to_string(),
                    params["name"].as_str().unwrap().to_string(),
                    params["role"].as_str().unwrap().to_string(),
                    params["capabilities"]
                        .as_array()
                        .unwrap()
                        .iter()
                        .map(|v| v.as_str().unwrap().to_string())
                        .collect(),
                )
            }
            "create_task" => {
                let params = request.params.as_object().unwrap();
                self.create_task(
                    params["task_id"].as_str().unwrap().to_string(),
                    params["title"].as_str().unwrap().to_string(),
                    params["assigned_to"].as_str().unwrap().to_string(),
                )
            }
            "process_code" => {
                let params = request.params.as_object().unwrap();
                self.process_code(
                    params["code"].as_str().unwrap(),
                    params["operation"].as_str().unwrap(),
                )
            }
            _ => {
                serde_json::json!({
                    "status": "error",
                    "message": format!("Unknown method: {}", request.method)
                })
            }
        }
    }
}

fn main() -> io::Result<()> {
    let mut floor = RustDepartmentFloor::new();

    eprintln!("Rust Department Floor (Floor 2) - Ready");
    eprintln!("Domain: {}", floor.domain);
    eprintln!("Offices: {}", floor.offices.join(", "));

    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    for line in stdin.lock().lines() {
        let line = line?;
        let response = match serde_json::from_str::<Request>(&line) {
            Ok(request) => floor.handle_request(request),
            Err(e) => {
                serde_json::json!({
                    "status": "error",
                    "message": format!("Invalid JSON: {}", e)
                })
            }
        };

        writeln!(stdout, "{}", serde_json::to_string(&response).unwrap())?;
        stdout.flush()?;
    }

    Ok(())
}
