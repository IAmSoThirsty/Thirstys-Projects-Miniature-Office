/*
 * FLOOR 3 - C JURISDICTION
 * Department Floor Implementation
 * 
 * Domain: Low-level systems, Embedded systems, Device drivers, Performance-critical code
 * Architectural Law: Explicit > implicit, Manual memory management, Buffer safety paramount
 * Security Doctrine: No buffer overruns, bounds checking, safe string operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 8192
#define MAX_AGENTS 100
#define MAX_TASKS 100
#define MAX_NAME_LENGTH 256
#define MAX_CAPABILITIES 10
#define MAX_CODE_LENGTH 16384

/* Agent Structure - Service Agent */
typedef struct {
    char agent_id[64];
    char name[MAX_NAME_LENGTH];
    char role[MAX_NAME_LENGTH];
    char capabilities[MAX_CAPABILITIES][MAX_NAME_LENGTH];
    int capability_count;
    int active;
} Agent;

/* Task Structure - Data/Model Agent */
typedef struct {
    char task_id[64];
    char title[MAX_NAME_LENGTH];
    char status[64];  /* pending, in_progress, completed, failed */
    char assigned_to[64];
    char created_at[64];
    int active;
} Task;

/* Code Analysis Result - Operations Agent */
typedef struct {
    int lines;
    int functions;
    int includes;
    int structs;
    char language[32];
} CodeAnalysis;

/* Department Floor State */
typedef struct {
    int floor_number;
    char language[32];
    char domain[MAX_NAME_LENGTH];
    const char *offices[6];
    Agent agents[MAX_AGENTS];
    int agent_count;
    Task tasks[MAX_TASKS];
    int task_count;
} DepartmentFloor;

/* Initialize the department floor */
void init_floor(DepartmentFloor *floor) {
    floor->floor_number = 3;
    strncpy(floor->language, "c", sizeof(floor->language) - 1);
    strncpy(floor->domain, "Low-level systems, Embedded, Device drivers", 
            sizeof(floor->domain) - 1);
    
    floor->offices[0] = "Architecture Office";
    floor->offices[1] = "Implementation Office";
    floor->offices[2] = "Review Office";
    floor->offices[3] = "Test Office";
    floor->offices[4] = "Security Office";
    floor->offices[5] = "Manager Office";
    
    floor->agent_count = 0;
    floor->task_count = 0;
    
    /* Initialize all agents and tasks as inactive */
    for (int i = 0; i < MAX_AGENTS; i++) {
        floor->agents[i].active = 0;
    }
    for (int i = 0; i < MAX_TASKS; i++) {
        floor->tasks[i].active = 0;
    }
}

/* Safe string copy with bounds checking */
void safe_copy(char *dest, const char *src, size_t dest_size) {
    if (dest == NULL || src == NULL || dest_size == 0) return;
    strncpy(dest, src, dest_size - 1);
    dest[dest_size - 1] = '\0';
}

/* Get current ISO timestamp */
void get_timestamp(char *buffer, size_t size) {
    time_t now = time(NULL);
    struct tm *tm_info = gmtime(&now);
    strftime(buffer, size, "%Y-%m-%dT%H:%M:%SZ", tm_info);
}

/* Escape JSON string for safe output */
void escape_json_string(const char *input, char *output, size_t output_size) {
    size_t j = 0;
    output[0] = '\0';
    
    for (size_t i = 0; input[i] != '\0' && j < output_size - 2; i++) {
        if (input[i] == '"' || input[i] == '\\') {
            if (j < output_size - 3) {
                output[j++] = '\\';
                output[j++] = input[i];
            }
        } else if (input[i] == '\n') {
            if (j < output_size - 3) {
                output[j++] = '\\';
                output[j++] = 'n';
            }
        } else if (input[i] == '\t') {
            if (j < output_size - 3) {
                output[j++] = '\\';
                output[j++] = 't';
            }
        } else if ((unsigned char)input[i] >= 32) {
            output[j++] = input[i];
        }
    }
    output[j] = '\0';
}

/* Simple JSON parser - extract string value for a key */
int extract_json_string(const char *json, const char *key, char *value, size_t value_size) {
    char search_key[256];
    snprintf(search_key, sizeof(search_key), "\"%s\"", key);
    
    const char *key_pos = strstr(json, search_key);
    if (key_pos == NULL) return 0;
    
    const char *colon = strchr(key_pos, ':');
    if (colon == NULL) return 0;
    
    /* Skip whitespace and opening quote */
    const char *value_start = colon + 1;
    while (*value_start && isspace(*value_start)) value_start++;
    
    if (*value_start == '"') {
        value_start++;
        const char *value_end = value_start;
        while (*value_end && *value_end != '"') {
            if (*value_end == '\\' && *(value_end + 1)) {
                value_end += 2;
            } else {
                value_end++;
            }
        }
        
        size_t len = value_end - value_start;
        if (len >= value_size) len = value_size - 1;
        strncpy(value, value_start, len);
        value[len] = '\0';
        return 1;
    }
    
    return 0;
}

/* Add an agent to the floor - Service Agent functionality */
int add_agent(DepartmentFloor *floor, const char *agent_id, const char *name, 
              const char *role, const char *capabilities[], int cap_count) {
    if (floor->agent_count >= MAX_AGENTS) return 0;
    
    Agent *agent = &floor->agents[floor->agent_count];
    safe_copy(agent->agent_id, agent_id, sizeof(agent->agent_id));
    safe_copy(agent->name, name, sizeof(agent->name));
    safe_copy(agent->role, role, sizeof(agent->role));
    
    agent->capability_count = cap_count < MAX_CAPABILITIES ? cap_count : MAX_CAPABILITIES;
    for (int i = 0; i < agent->capability_count; i++) {
        safe_copy(agent->capabilities[i], capabilities[i], sizeof(agent->capabilities[i]));
    }
    
    agent->active = 1;
    floor->agent_count++;
    return 1;
}

/* Create a task - Data/Model Agent functionality */
int create_task(DepartmentFloor *floor, const char *task_id, const char *title, 
                const char *assigned_to) {
    if (floor->task_count >= MAX_TASKS) return 0;
    
    Task *task = &floor->tasks[floor->task_count];
    safe_copy(task->task_id, task_id, sizeof(task->task_id));
    safe_copy(task->title, title, sizeof(task->title));
    safe_copy(task->status, "pending", sizeof(task->status));
    safe_copy(task->assigned_to, assigned_to, sizeof(task->assigned_to));
    
    get_timestamp(task->created_at, sizeof(task->created_at));
    task->active = 1;
    floor->task_count++;
    return 1;
}

/* Analyze C code - Operations Agent functionality */
void analyze_code(const char *code, CodeAnalysis *analysis) {
    memset(analysis, 0, sizeof(CodeAnalysis));
    safe_copy(analysis->language, "c", sizeof(analysis->language));
    
    const char *ptr = code;
    int in_comment = 0;
    int in_block_comment = 0;
    
    while (*ptr) {
        /* Count lines */
        if (*ptr == '\n') {
            analysis->lines++;
            in_comment = 0;
        }
        
        /* Handle comments */
        if (!in_block_comment && *ptr == '/' && *(ptr + 1) == '/') {
            in_comment = 1;
        }
        if (*ptr == '/' && *(ptr + 1) == '*') {
            in_block_comment = 1;
        }
        if (in_block_comment && *ptr == '*' && *(ptr + 1) == '/') {
            in_block_comment = 0;
            ptr += 2;
            continue;
        }
        
        if (!in_comment && !in_block_comment) {
            /* Count function definitions */
            if (*ptr != ' ' && *ptr != '\t') {
                const char *temp = ptr;
                while (*temp && (isalnum(*temp) || *temp == '_' || *temp == '*')) temp++;
                while (*temp && isspace(*temp)) temp++;
                if (*temp == '(') {
                    /* Likely a function */
                    analysis->functions++;
                    ptr = temp;
                }
            }
            
            /* Count includes */
            if (strncmp(ptr, "#include", 8) == 0) {
                analysis->includes++;
            }
            
            /* Count structs */
            if (strncmp(ptr, "struct", 6) == 0 && 
                (ptr == code || !isalnum(*(ptr - 1)))) {
                analysis->structs++;
            }
        }
        
        ptr++;
    }
    
    if (analysis->lines == 0 && *code != '\0') {
        analysis->lines = 1;
    }
}

/* Print agent in JSON format */
void print_agent_json(const Agent *agent, int is_last) {
    char escaped_name[MAX_NAME_LENGTH * 2];
    char escaped_role[MAX_NAME_LENGTH * 2];
    
    escape_json_string(agent->name, escaped_name, sizeof(escaped_name));
    escape_json_string(agent->role, escaped_role, sizeof(escaped_role));
    
    printf("    {\n");
    printf("      \"agent_id\": \"%s\",\n", agent->agent_id);
    printf("      \"name\": \"%s\",\n", escaped_name);
    printf("      \"role\": \"%s\",\n", escaped_role);
    printf("      \"capabilities\": [");
    
    for (int i = 0; i < agent->capability_count; i++) {
        char escaped_cap[MAX_NAME_LENGTH * 2];
        escape_json_string(agent->capabilities[i], escaped_cap, sizeof(escaped_cap));
        printf("\"%s\"", escaped_cap);
        if (i < agent->capability_count - 1) printf(", ");
    }
    
    printf("]\n");
    printf("    }%s\n", is_last ? "" : ",");
}

/* Print task in JSON format */
void print_task_json(const Task *task, int is_last) {
    char escaped_title[MAX_NAME_LENGTH * 2];
    escape_json_string(task->title, escaped_title, sizeof(escaped_title));
    
    printf("    {\n");
    printf("      \"task_id\": \"%s\",\n", task->task_id);
    printf("      \"title\": \"%s\",\n", escaped_title);
    printf("      \"status\": \"%s\",\n", task->status);
    printf("      \"assigned_to\": \"%s\",\n", task->assigned_to);
    printf("      \"created_at\": \"%s\"\n", task->created_at);
    printf("    }%s\n", is_last ? "" : ",");
}

/* Get floor information */
void get_floor_info(const DepartmentFloor *floor) {
    char escaped_domain[MAX_NAME_LENGTH * 2];
    escape_json_string(floor->domain, escaped_domain, sizeof(escaped_domain));
    
    printf("{\n");
    printf("  \"status\": \"success\",\n");
    printf("  \"floor_number\": %d,\n", floor->floor_number);
    printf("  \"language\": \"%s\",\n", floor->language);
    printf("  \"domain\": \"%s\",\n", escaped_domain);
    printf("  \"offices\": [\n");
    
    for (int i = 0; i < 6; i++) {
        printf("    \"%s\"%s\n", floor->offices[i], i < 5 ? "," : "");
    }
    
    printf("  ],\n");
    printf("  \"agent_count\": %d,\n", floor->agent_count);
    printf("  \"task_count\": %d,\n", floor->task_count);
    printf("  \"agents\": [\n");
    
    for (int i = 0; i < floor->agent_count; i++) {
        if (floor->agents[i].active) {
            print_agent_json(&floor->agents[i], i == floor->agent_count - 1);
        }
    }
    
    printf("  ],\n");
    printf("  \"tasks\": [\n");
    
    for (int i = 0; i < floor->task_count; i++) {
        if (floor->tasks[i].active) {
            print_task_json(&floor->tasks[i], i == floor->task_count - 1);
        }
    }
    
    printf("  ]\n");
    printf("}\n");
}

/* Handle add_agent request */
void handle_add_agent(DepartmentFloor *floor, const char *json) {
    char agent_id[64], name[MAX_NAME_LENGTH], role[MAX_NAME_LENGTH];
    
    if (!extract_json_string(json, "agent_id", agent_id, sizeof(agent_id)) ||
        !extract_json_string(json, "name", name, sizeof(name)) ||
        !extract_json_string(json, "role", role, sizeof(role))) {
        printf("{\"status\": \"error\", \"message\": \"Missing required parameters\"}\n");
        return;
    }
    
    /* For simplicity, extract one capability */
    const char *capabilities[] = {"systems_programming", "memory_management", "optimization"};
    
    if (add_agent(floor, agent_id, name, role, capabilities, 3)) {
        char escaped_name[MAX_NAME_LENGTH * 2];
        escape_json_string(name, escaped_name, sizeof(escaped_name));
        printf("{\"status\": \"success\", \"message\": \"Agent %s added\"}\n", escaped_name);
    } else {
        printf("{\"status\": \"error\", \"message\": \"Failed to add agent\"}\n");
    }
}

/* Handle create_task request */
void handle_create_task(DepartmentFloor *floor, const char *json) {
    char task_id[64], title[MAX_NAME_LENGTH], assigned_to[64];
    
    if (!extract_json_string(json, "task_id", task_id, sizeof(task_id)) ||
        !extract_json_string(json, "title", title, sizeof(title)) ||
        !extract_json_string(json, "assigned_to", assigned_to, sizeof(assigned_to))) {
        printf("{\"status\": \"error\", \"message\": \"Missing required parameters\"}\n");
        return;
    }
    
    if (create_task(floor, task_id, title, assigned_to)) {
        char escaped_title[MAX_NAME_LENGTH * 2];
        escape_json_string(title, escaped_title, sizeof(escaped_title));
        printf("{\"status\": \"success\", \"message\": \"Task created\", \"task_id\": \"%s\"}\n", 
               task_id);
    } else {
        printf("{\"status\": \"error\", \"message\": \"Failed to create task\"}\n");
    }
}

/* Handle process_code request */
void handle_process_code(const char *json) {
    /* Extract code from JSON - simplified extraction */
    const char *code_start = strstr(json, "\"code\"");
    if (code_start == NULL) {
        printf("{\"status\": \"error\", \"message\": \"No code provided\"}\n");
        return;
    }
    
    char operation[64] = "analyze";
    extract_json_string(json, "operation", operation, sizeof(operation));
    
    /* Find the code content */
    const char *colon = strchr(code_start, ':');
    if (colon == NULL) {
        printf("{\"status\": \"error\", \"message\": \"Invalid code format\"}\n");
        return;
    }
    
    const char *value_start = colon + 1;
    while (*value_start && isspace(*value_start)) value_start++;
    
    if (*value_start != '"') {
        printf("{\"status\": \"error\", \"message\": \"Invalid code format\"}\n");
        return;
    }
    
    value_start++;
    char code[MAX_CODE_LENGTH];
    size_t i = 0;
    
    /* Extract code with escape handling */
    while (*value_start && *value_start != '"' && i < MAX_CODE_LENGTH - 1) {
        if (*value_start == '\\' && *(value_start + 1)) {
            value_start++;
            if (*value_start == 'n') code[i++] = '\n';
            else if (*value_start == 't') code[i++] = '\t';
            else if (*value_start == '\\') code[i++] = '\\';
            else if (*value_start == '"') code[i++] = '"';
            else code[i++] = *value_start;
            value_start++;
        } else {
            code[i++] = *value_start++;
        }
    }
    code[i] = '\0';
    
    if (strcmp(operation, "analyze") == 0) {
        CodeAnalysis analysis;
        analyze_code(code, &analysis);
        
        printf("{\n");
        printf("  \"status\": \"success\",\n");
        printf("  \"analysis\": {\n");
        printf("    \"lines\": %d,\n", analysis.lines);
        printf("    \"functions\": %d,\n", analysis.functions);
        printf("    \"includes\": %d,\n", analysis.includes);
        printf("    \"structs\": %d,\n", analysis.structs);
        printf("    \"language\": \"%s\"\n", analysis.language);
        printf("  }\n");
        printf("}\n");
    } else {
        printf("{\"status\": \"error\", \"message\": \"Unknown operation: %s\"}\n", operation);
    }
}

/* Handle incoming JSON-RPC request */
void handle_request(DepartmentFloor *floor, const char *json) {
    char method[64];
    
    if (!extract_json_string(json, "method", method, sizeof(method))) {
        printf("{\"status\": \"error\", \"message\": \"No method specified\"}\n");
        return;
    }
    
    if (strcmp(method, "get_info") == 0) {
        get_floor_info(floor);
    } else if (strcmp(method, "add_agent") == 0) {
        handle_add_agent(floor, json);
    } else if (strcmp(method, "create_task") == 0) {
        handle_create_task(floor, json);
    } else if (strcmp(method, "process_code") == 0) {
        handle_process_code(json);
    } else {
        printf("{\"status\": \"error\", \"message\": \"Unknown method: %s\"}\n", method);
    }
}

int main(void) {
    DepartmentFloor floor;
    init_floor(&floor);
    
    /* Print initialization to stderr */
    fprintf(stderr, "C Department Floor (Floor %d) - Ready\n", floor.floor_number);
    fprintf(stderr, "Domain: %s\n", floor.domain);
    fprintf(stderr, "Offices: ");
    for (int i = 0; i < 6; i++) {
        fprintf(stderr, "%s%s", floor.offices[i], i < 5 ? ", " : "\n");
    }
    fflush(stderr);
    
    /* Initialize default agents for all offices */
    const char *sys_caps[] = {"memory_management", "pointer_arithmetic", "buffer_safety"};
    add_agent(&floor, "sys_agent_1", "System Architect", "Architecture Office", sys_caps, 3);
    
    const char *impl_caps[] = {"low_level_coding", "optimization", "assembly"};
    add_agent(&floor, "impl_agent_1", "Implementation Engineer", "Implementation Office", impl_caps, 3);
    
    const char *review_caps[] = {"code_review", "buffer_overflow_detection", "static_analysis"};
    add_agent(&floor, "review_agent_1", "Review Specialist", "Review Office", review_caps, 3);
    
    const char *test_caps[] = {"unit_testing", "integration_testing", "valgrind"};
    add_agent(&floor, "test_agent_1", "Test Engineer", "Test Office", test_caps, 3);
    
    const char *sec_caps[] = {"vulnerability_scanning", "fuzzing", "exploit_mitigation"};
    add_agent(&floor, "sec_agent_1", "Security Analyst", "Security Office", sec_caps, 3);
    
    const char *mgr_caps[] = {"task_management", "resource_allocation", "escalation"};
    add_agent(&floor, "mgr_agent_1", "Floor Manager", "Manager Office", mgr_caps, 3);
    
    /* Process JSON-RPC requests from stdin */
    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), stdin) != NULL) {
        /* Remove trailing newline */
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }
        
        if (strlen(line) > 0) {
            handle_request(&floor, line);
            fflush(stdout);
        }
    }
    
    return 0;
}
