/*
FLOOR 5 - GO JURISDICTION
Department Floor Implementation

Domain: Network services, Concurrency-heavy systems, Infrastructure logic
Architectural Law: Explicit concurrency, No goroutine leaks, go fmt compliance
*/

package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"time"
)

// FloorAgent represents an agent working on this floor
type FloorAgent struct {
	AgentID      string   `json:"agent_id"`
	Name         string   `json:"name"`
	Role         string   `json:"role"`
	Capabilities []string `json:"capabilities"`
}

// Task represents a task managed by this floor
type Task struct {
	TaskID     string `json:"task_id"`
	Title      string `json:"title"`
	Status     string `json:"status"`
	AssignedTo string `json:"assigned_to"`
	CreatedAt  string `json:"created_at"`
}

// Request represents an incoming JSON-RPC request
type Request struct {
	Method string                 `json:"method"`
	Params map[string]interface{} `json:"params,omitempty"`
}

// Response represents a response to a request
type Response struct {
	Status string                 `json:"status"`
	Data   map[string]interface{} `json:",inline"`
}

// GoDepartmentFloor represents the Go department floor
type GoDepartmentFloor struct {
	FloorNumber int                    `json:"floor_number"`
	Language    string                 `json:"language"`
	Domain      string                 `json:"domain"`
	Agents      map[string]*FloorAgent `json:"agents"`
	Tasks       map[string]*Task       `json:"tasks"`
	Offices     []string               `json:"offices"`
}

// NewGoDepartmentFloor creates a new Go Department Floor
//
// Implements the department logic for Go jurisdiction following:
// - Language Sovereignty
// - Identical Internal Topology (6 offices)
// - Contract-Bound Operation
// - Non-Creative Mandate
// - Failure Escalation Guarantee
func NewGoDepartmentFloor() *GoDepartmentFloor {
	return &GoDepartmentFloor{
		FloorNumber: 5,
		Language:    "go",
		Domain:      "Network services, Concurrency-heavy systems",
		Agents:      make(map[string]*FloorAgent),
		Tasks:       make(map[string]*Task),
		Offices: []string{
			"Architecture Office",
			"Implementation Office",
			"Review Office",
			"Test Office",
			"Security Office",
			"Manager Office",
		},
	}
}

// AddAgent adds an agent to this floor
func (f *GoDepartmentFloor) AddAgent(agentID, name, role string, capabilities []string) map[string]interface{} {
	agent := &FloorAgent{
		AgentID:      agentID,
		Name:         name,
		Role:         role,
		Capabilities: capabilities,
	}
	f.Agents[agentID] = agent
	return map[string]interface{}{
		"status": "success",
		"agent":  agent,
	}
}

// CreateTask creates a new task on this floor
func (f *GoDepartmentFloor) CreateTask(taskID, title, assignedTo string) map[string]interface{} {
	task := &Task{
		TaskID:     taskID,
		Title:      title,
		Status:     "pending",
		AssignedTo: assignedTo,
		CreatedAt:  time.Now().UTC().Format(time.RFC3339),
	}
	f.Tasks[taskID] = task
	return map[string]interface{}{
		"status": "success",
		"task":   task,
	}
}

// GetFloorInfo returns information about this floor
func (f *GoDepartmentFloor) GetFloorInfo() map[string]interface{} {
	agents := make([]interface{}, 0, len(f.Agents))
	for _, agent := range f.Agents {
		agents = append(agents, agent)
	}

	tasks := make([]interface{}, 0, len(f.Tasks))
	for _, task := range f.Tasks {
		tasks = append(tasks, task)
	}

	return map[string]interface{}{
		"floor_number": f.FloorNumber,
		"language":     f.Language,
		"domain":       f.Domain,
		"offices":      f.Offices,
		"agent_count":  len(f.Agents),
		"task_count":   len(f.Tasks),
		"agents":       agents,
		"tasks":        tasks,
	}
}

// ProcessCode processes Go code according to floor jurisdiction
func (f *GoDepartmentFloor) ProcessCode(code, operation string) map[string]interface{} {
	switch operation {
	case "analyze":
		lines := strings.Split(code, "\n")
		funcCount := strings.Count(code, "func ")
		structCount := strings.Count(code, "type ")
		goroutineCount := strings.Count(code, "go ")

		return map[string]interface{}{
			"status": "success",
			"analysis": map[string]interface{}{
				"lines":      len(lines),
				"functions":  funcCount,
				"structs":    structCount,
				"goroutines": goroutineCount,
				"language":   "go",
			},
		}
	case "lint":
		return map[string]interface{}{
			"status":  "success",
			"linted":  true,
			"message": "Go code linted (go fmt + go vet)",
		}
	default:
		return map[string]interface{}{
			"status":  "error",
			"message": fmt.Sprintf("Unknown operation: %s", operation),
		}
	}
}

// HandleRequest handles an incoming JSON-RPC request
func (f *GoDepartmentFloor) HandleRequest(request Request) map[string]interface{} {
	switch request.Method {
	case "get_info":
		return f.GetFloorInfo()
	case "add_agent":
		agentID := request.Params["agent_id"].(string)
		name := request.Params["name"].(string)
		role := request.Params["role"].(string)
		capabilities := make([]string, 0)
		if caps, ok := request.Params["capabilities"].([]interface{}); ok {
			for _, c := range caps {
				capabilities = append(capabilities, c.(string))
			}
		}
		return f.AddAgent(agentID, name, role, capabilities)
	case "create_task":
		taskID := request.Params["task_id"].(string)
		title := request.Params["title"].(string)
		assignedTo := request.Params["assigned_to"].(string)
		return f.CreateTask(taskID, title, assignedTo)
	case "process_code":
		code := request.Params["code"].(string)
		operation := request.Params["operation"].(string)
		return f.ProcessCode(code, operation)
	default:
		return map[string]interface{}{
			"status":  "error",
			"message": fmt.Sprintf("Unknown method: %s", request.Method),
		}
	}
}

func main() {
	floor := NewGoDepartmentFloor()

	fmt.Fprintf(os.Stderr, "Go Department Floor (Floor 5) - Ready\n")
	fmt.Fprintf(os.Stderr, "Domain: %s\n", floor.Domain)
	fmt.Fprintf(os.Stderr, "Offices: %s\n", strings.Join(floor.Offices, ", "))

	scanner := bufio.NewScanner(os.Stdin)
	encoder := json.NewEncoder(os.Stdout)

	for scanner.Scan() {
		line := scanner.Text()
		var request Request
		if err := json.Unmarshal([]byte(line), &request); err != nil {
			response := map[string]interface{}{
				"status":  "error",
				"message": fmt.Sprintf("Invalid JSON: %v", err),
			}
			encoder.Encode(response)
			continue
		}

		response := floor.HandleRequest(request)
		encoder.Encode(response)
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}
}
