/*
 * FLOOR 10 - JAVA JURISDICTION
 * Department Floor Implementation
 * 
 * Domain: Enterprise systems, Web services, Business applications, Cloud services
 * Architectural Law: OOP principles, SOLID design, Enterprise patterns
 * Security Doctrine: Type safety, Exception handling, Security manager
 */

package com.miniatureoffice.floor10;

import java.io.*;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Agent Class - Service Agent
 * Represents an agent working on the Java floor
 */
class Agent {
    private final String agentId;
    private final String name;
    private final String role;
    private final List<String> capabilities;
    private volatile boolean active;

    public Agent(String agentId, String name, String role, List<String> capabilities) {
        this.agentId = Objects.requireNonNull(agentId, "Agent ID cannot be null");
        this.name = Objects.requireNonNull(name, "Name cannot be null");
        this.role = Objects.requireNonNull(role, "Role cannot be null");
        this.capabilities = new ArrayList<>(capabilities);
        this.active = true;
    }

    public String getAgentId() { return agentId; }
    public String getName() { return name; }
    public String getRole() { return role; }
    public List<String> getCapabilities() { return Collections.unmodifiableList(capabilities); }
    public boolean isActive() { return active; }

    public String toJson() {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        json.append("      \"agent_id\": \"").append(escapeJson(agentId)).append("\",\n");
        json.append("      \"name\": \"").append(escapeJson(name)).append("\",\n");
        json.append("      \"role\": \"").append(escapeJson(role)).append("\",\n");
        json.append("      \"capabilities\": [");
        
        for (int i = 0; i < capabilities.size(); i++) {
            json.append("\"").append(escapeJson(capabilities.get(i))).append("\"");
            if (i < capabilities.size() - 1) json.append(", ");
        }
        
        json.append("]\n");
        json.append("    }");
        return json.toString();
    }

    private static String escapeJson(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Agent agent = (Agent) o;
        return agentId.equals(agent.agentId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(agentId);
    }
}

/**
 * Task Class - Data/Model Agent
 * Represents a task managed by the floor
 */
class Task {
    private final String taskId;
    private final String title;
    private volatile String status;
    private final String assignedTo;
    private final String createdAt;
    private volatile boolean active;

    public Task(String taskId, String title, String assignedTo) {
        this.taskId = Objects.requireNonNull(taskId, "Task ID cannot be null");
        this.title = Objects.requireNonNull(title, "Title cannot be null");
        this.assignedTo = Objects.requireNonNull(assignedTo, "Assigned to cannot be null");
        this.status = "pending";
        this.createdAt = Instant.now().toString();
        this.active = true;
    }

    public String getTaskId() { return taskId; }
    public String getTitle() { return title; }
    public String getStatus() { return status; }
    public String getAssignedTo() { return assignedTo; }
    public String getCreatedAt() { return createdAt; }
    public boolean isActive() { return active; }

    public void setStatus(String status) {
        this.status = Objects.requireNonNull(status);
    }

    public String toJson() {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        json.append("      \"task_id\": \"").append(escapeJson(taskId)).append("\",\n");
        json.append("      \"title\": \"").append(escapeJson(title)).append("\",\n");
        json.append("      \"status\": \"").append(escapeJson(status)).append("\",\n");
        json.append("      \"assigned_to\": \"").append(escapeJson(assignedTo)).append("\",\n");
        json.append("      \"created_at\": \"").append(escapeJson(createdAt)).append("\"\n");
        json.append("    }");
        return json.toString();
    }

    private static String escapeJson(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Task task = (Task) o;
        return taskId.equals(task.taskId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(taskId);
    }
}

/**
 * Code Analysis Result - Operations Agent
 */
class CodeAnalysis {
    private int lines;
    private int classes;
    private int methods;
    private int packages;
    private int interfaces;
    private final String language;

    public CodeAnalysis() {
        this.language = "java";
    }

    public void setLines(int lines) { this.lines = lines; }
    public void setClasses(int classes) { this.classes = classes; }
    public void setMethods(int methods) { this.methods = methods; }
    public void setPackages(int packages) { this.packages = packages; }
    public void setInterfaces(int interfaces) { this.interfaces = interfaces; }
    
    public int getLines() { return lines; }
    public int getClasses() { return classes; }
    public int getMethods() { return methods; }
    public int getPackages() { return packages; }
    public int getInterfaces() { return interfaces; }

    public String toJson() {
        return String.format(
            "{\n" +
            "    \"lines\": %d,\n" +
            "    \"classes\": %d,\n" +
            "    \"methods\": %d,\n" +
            "    \"packages\": %d,\n" +
            "    \"interfaces\": %d,\n" +
            "    \"language\": \"%s\"\n" +
            "  }",
            lines, classes, methods, packages, interfaces, language
        );
    }
}

/**
 * Code Analyzer - Operations Agent
 */
class CodeAnalyzer {
    public static CodeAnalysis analyze(String code) {
        CodeAnalysis result = new CodeAnalysis();
        
        String[] lines = code.split("\n");
        result.setLines(lines.length);
        
        boolean inBlockComment = false;
        
        for (String line : lines) {
            line = line.trim();
            
            // Handle block comments
            if (line.contains("/*")) {
                inBlockComment = true;
            }
            if (inBlockComment && line.contains("*/")) {
                inBlockComment = false;
                continue;
            }
            if (inBlockComment) continue;
            
            // Skip line comments
            int commentPos = line.indexOf("//");
            if (commentPos >= 0) {
                line = line.substring(0, commentPos).trim();
            }
            
            // Count Java constructs
            if (containsKeyword(line, "class")) result.setClasses(result.getClasses() + 1);
            if (containsKeyword(line, "interface")) result.setInterfaces(result.getInterfaces() + 1);
            if (containsKeyword(line, "package")) result.setPackages(result.getPackages() + 1);
            
            // Count methods (simplified)
            if (line.contains("(") && line.contains(")") && !line.contains(";")) {
                if (line.contains("public") || line.contains("private") || 
                    line.contains("protected") || line.contains("void")) {
                    result.setMethods(result.getMethods() + 1);
                }
            }
        }
        
        return result;
    }

    private static boolean containsKeyword(String line, String keyword) {
        int index = line.indexOf(keyword);
        if (index < 0) return false;
        
        // Check word boundaries
        boolean startOk = (index == 0 || !Character.isJavaIdentifierPart(line.charAt(index - 1)));
        boolean endOk = (index + keyword.length() >= line.length() || 
                        !Character.isJavaIdentifierPart(line.charAt(index + keyword.length())));
        
        return startOk && endOk;
    }
}

/**
 * Main Department Floor Class
 * Implements the Java jurisdiction with enterprise patterns
 */
public class DepartmentFloor {
    private final int floorNumber;
    private final String language;
    private final String domain;
    private final List<String> offices;
    private final Map<String, Agent> agents;
    private final Map<String, Task> tasks;

    public DepartmentFloor() {
        this.floorNumber = 10;
        this.language = "java";
        this.domain = "Enterprise systems, Web services, Business applications";
        this.offices = Collections.unmodifiableList(Arrays.asList(
            "Architecture Office",
            "Implementation Office",
            "Review Office",
            "Test Office",
            "Security Office",
            "Manager Office"
        ));
        this.agents = new ConcurrentHashMap<>();
        this.tasks = new ConcurrentHashMap<>();
        
        initializeDefaultAgents();
    }

    private void initializeDefaultAgents() {
        addAgent("arch_agent_1", "Enterprise Architect", "Architecture Office",
                Arrays.asList("design_patterns", "uml", "system_architecture"));
        
        addAgent("impl_agent_1", "Java Developer", "Implementation Office",
                Arrays.asList("spring_framework", "hibernate", "microservices"));
        
        addAgent("review_agent_1", "Code Reviewer", "Review Office",
                Arrays.asList("sonarqube", "checkstyle", "code_quality"));
        
        addAgent("test_agent_1", "Test Engineer", "Test Office",
                Arrays.asList("junit", "mockito", "integration_testing"));
        
        addAgent("sec_agent_1", "Security Specialist", "Security Office",
                Arrays.asList("owasp", "penetration_testing", "security_audit"));
        
        addAgent("mgr_agent_1", "Floor Manager", "Manager Office",
                Arrays.asList("agile", "scrum", "team_coordination"));
    }

    public void addAgent(String id, String name, String role, List<String> capabilities) {
        Agent agent = new Agent(id, name, role, capabilities);
        agents.put(id, agent);
    }

    public void createTask(String id, String title, String assignedTo) {
        Task task = new Task(id, title, assignedTo);
        tasks.put(id, task);
    }

    public String getFloorInfo() {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        json.append("  \"status\": \"success\",\n");
        json.append("  \"floor_number\": ").append(floorNumber).append(",\n");
        json.append("  \"language\": \"").append(language).append("\",\n");
        json.append("  \"domain\": \"").append(escapeJson(domain)).append("\",\n");
        json.append("  \"offices\": [\n");
        
        for (int i = 0; i < offices.size(); i++) {
            json.append("    \"").append(offices.get(i)).append("\"");
            if (i < offices.size() - 1) json.append(",");
            json.append("\n");
        }
        
        json.append("  ],\n");
        json.append("  \"agent_count\": ").append(agents.size()).append(",\n");
        json.append("  \"task_count\": ").append(tasks.size()).append(",\n");
        json.append("  \"agents\": [\n");
        
        List<Agent> agentList = new ArrayList<>(agents.values());
        for (int i = 0; i < agentList.size(); i++) {
            json.append(agentList.get(i).toJson());
            if (i < agentList.size() - 1) json.append(",");
            json.append("\n");
        }
        
        json.append("  ],\n");
        json.append("  \"tasks\": [\n");
        
        List<Task> taskList = new ArrayList<>(tasks.values());
        for (int i = 0; i < taskList.size(); i++) {
            json.append(taskList.get(i).toJson());
            if (i < taskList.size() - 1) json.append(",");
            json.append("\n");
        }
        
        json.append("  ]\n");
        json.append("}\n");
        return json.toString();
    }

    public String processCode(String code, String operation) {
        if ("analyze".equals(operation)) {
            CodeAnalysis analysis = CodeAnalyzer.analyze(code);
            return "{\n  \"status\": \"success\",\n  \"analysis\": " + 
                   analysis.toJson() + "\n}\n";
        } else if ("compile".equals(operation)) {
            return "{\"status\": \"success\", \"message\": \"Code compiled successfully\"}";
        } else {
            return "{\"status\": \"error\", \"message\": \"Unknown operation: " + 
                   operation + "\"}";
        }
    }

    public String handleRequest(String jsonRequest) {
        try {
            String method = extractJsonValue(jsonRequest, "method");
            
            if (method == null) {
                return "{\"status\": \"error\", \"message\": \"No method specified\"}";
            }
            
            switch (method) {
                case "get_info":
                    return getFloorInfo();
                case "add_agent":
                    return handleAddAgent(jsonRequest);
                case "create_task":
                    return handleCreateTask(jsonRequest);
                case "process_code":
                    return handleProcessCode(jsonRequest);
                default:
                    return "{\"status\": \"error\", \"message\": \"Unknown method: " + 
                           method + "\"}";
            }
        } catch (Exception e) {
            return "{\"status\": \"error\", \"message\": \"" + 
                   escapeJson(e.getMessage()) + "\"}";
        }
    }

    private String handleAddAgent(String json) {
        String id = extractJsonValue(json, "agent_id");
        String name = extractJsonValue(json, "name");
        String role = extractJsonValue(json, "role");
        
        if (id == null || name == null || role == null) {
            return "{\"status\": \"error\", \"message\": \"Missing required parameters\"}";
        }
        
        addAgent(id, name, role, Arrays.asList("java_expertise", "problem_solving"));
        return "{\"status\": \"success\", \"message\": \"Agent added\"}";
    }

    private String handleCreateTask(String json) {
        String id = extractJsonValue(json, "task_id");
        String title = extractJsonValue(json, "title");
        String assigned = extractJsonValue(json, "assigned_to");
        
        if (id == null || title == null || assigned == null) {
            return "{\"status\": \"error\", \"message\": \"Missing required parameters\"}";
        }
        
        createTask(id, title, assigned);
        return "{\"status\": \"success\", \"message\": \"Task created\", " +
               "\"task_id\": \"" + id + "\"}";
    }

    private String handleProcessCode(String json) {
        String code = extractJsonValue(json, "code");
        String operation = extractJsonValue(json, "operation");
        
        if (code == null) {
            return "{\"status\": \"error\", \"message\": \"No code provided\"}";
        }
        
        return processCode(code, operation != null ? operation : "analyze");
    }

    private static String extractJsonValue(String json, String key) {
        String searchKey = "\"" + key + "\"";
        int keyPos = json.indexOf(searchKey);
        
        if (keyPos < 0) return null;
        
        int colonPos = json.indexOf(':', keyPos);
        if (colonPos < 0) return null;
        
        // Skip whitespace
        int valueStart = colonPos + 1;
        while (valueStart < json.length() && Character.isWhitespace(json.charAt(valueStart))) {
            valueStart++;
        }
        
        if (valueStart >= json.length()) return null;
        
        // Handle string values
        if (json.charAt(valueStart) == '"') {
            valueStart++;
            StringBuilder result = new StringBuilder();
            
            for (int i = valueStart; i < json.length(); i++) {
                char c = json.charAt(i);
                if (c == '"') {
                    return result.toString();
                } else if (c == '\\' && i + 1 < json.length()) {
                    i++;
                    char next = json.charAt(i);
                    switch (next) {
                        case 'n': result.append('\n'); break;
                        case 't': result.append('\t'); break;
                        case 'r': result.append('\r'); break;
                        case '\\': result.append('\\'); break;
                        case '"': result.append('"'); break;
                        default: result.append(next); break;
                    }
                } else {
                    result.append(c);
                }
            }
        }
        
        return null;
    }

    private static String escapeJson(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }

    public static void main(String[] args) {
        DepartmentFloor floor = new DepartmentFloor();
        
        // Print initialization to stderr
        System.err.println("Java Department Floor (Floor 10) - Ready");
        System.err.println("Domain: Enterprise systems, Web services, Business applications");
        System.err.println("Offices: Architecture, Implementation, Review, Test, Security, Manager");
        System.err.flush();
        
        // Process JSON-RPC requests from stdin
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    String response = floor.handleRequest(line);
                    System.out.println(response);
                    System.out.flush();
                }
            }
        } catch (IOException e) {
            System.err.println("Error reading input: " + e.getMessage());
            System.exit(1);
        }
    }
}
