/*
 * FLOOR 26 - CUDA/GPU JURISDICTION
 * Department Floor Implementation
 * 
 * Domain: Parallel compute kernels, GPU acceleration, Massively parallel workloads
 * Architectural Law: Memory coalescing > compute, Thread safety paramount
 * Security Doctrine: Memory bounds checking, Race condition prevention, Kernel validation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cuda_runtime.h>

#define MAX_LINE 8192
#define MAX_CODE 16384
#define MAX_NAME 256

/* Agent structures */
typedef struct {
    char agent_id[64];
    char name[MAX_NAME];
    char role[64];
    int active;
} Agent;

typedef struct {
    char task_id[64];
    char title[MAX_NAME];
    char status[64];
    int active;
} Task;

/* Floor state */
typedef struct {
    int floor_number;
    const char *language;
    const char *domain;
    const char *offices[6];
    Agent agents[100];
    int agent_count;
    Task tasks[100];
    int task_count;
} DepartmentFloor;

/* Function prototypes */
void init_floor(DepartmentFloor *floor);
void handle_request(const char *json_str, DepartmentFloor *floor);
void send_response(int req_id, const char *result);
void send_error(int req_id, int code, const char *msg);
int extract_method(const char *json, char *method, size_t size);
int extract_id(const char *json);
int extract_string_param(const char *json, const char *key, char *value, size_t size);

/* CUDA Kernel Prototypes */
__global__ void vector_add_kernel(const float *a, const float *b, float *c, int n);
__global__ void matrix_multiply_kernel(const float *A, const float *B, float *C, 
                                       int M, int N, int K);

/* Initialize floor */
void init_floor(DepartmentFloor *floor) {
    floor->floor_number = 26;
    floor->language = "cuda";
    floor->domain = "Parallel compute kernels, GPU acceleration, Massively parallel workloads";
    
    floor->offices[0] = "Architecture Office";
    floor->offices[1] = "Implementation Office";
    floor->offices[2] = "Review Office";
    floor->offices[3] = "Test Office";
    floor->offices[4] = "Security Office";
    floor->offices[5] = "Manager Office";
    
    floor->agent_count = 0;
    floor->task_count = 0;
}

/* Main loop */
int main() {
    DepartmentFloor floor;
    init_floor(&floor);
    
    char line[MAX_LINE];
    while (fgets(line, sizeof(line), stdin)) {
        handle_request(line, &floor);
    }
    
    return 0;
}

/* Handle JSON-RPC request */
void handle_request(const char *json_str, DepartmentFloor *floor) {
    char method[64] = {0};
    int req_id = 0;
    
    if (strlen(json_str) == 0) return;
    
    extract_method(json_str, method, sizeof(method));
    req_id = extract_id(json_str);
    
    if (strcmp(method, "initialize") == 0) {
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"status\":\"initialized\","
               "\"floor\":26,\"language\":\"cuda\","
               "\"capabilities\":[\"gpu_compute\",\"parallel_kernels\",\"memory_management\"]}}\n",
               req_id);
        fflush(stdout);
    }
    else if (strcmp(method, "get_floor_info") == 0) {
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{"
               "\"floor_number\":26,"
               "\"language\":\"cuda\","
               "\"domain\":\"Parallel compute kernels, GPU acceleration, Massively parallel workloads\","
               "\"offices\":[\"Architecture Office\",\"Implementation Office\","
               "\"Review Office\",\"Test Office\",\"Security Office\",\"Manager Office\"],"
               "\"agents\":["
               "{\"agent_id\":\"cuda-service-1\",\"name\":\"Kernel Service Agent\","
               "\"role\":\"Service\",\"capabilities\":[\"kernel_analysis\",\"thread_validation\"]},"
               "{\"agent_id\":\"cuda-data-1\",\"name\":\"Memory Data Agent\","
               "\"role\":\"Data\",\"capabilities\":[\"memory_validation\",\"coalescing_check\"]},"
               "{\"agent_id\":\"cuda-ops-1\",\"name\":\"Operations Agent\","
               "\"role\":\"Operations\",\"capabilities\":[\"parallel_ops\",\"reduction_ops\"]}"
               "],"
               "\"architectural_laws\":["
               "\"Memory coalescing over compute complexity\","
               "\"Thread safety paramount\","
               "\"Minimize host-device transfers\","
               "\"Maximize occupancy\""
               "],"
               "\"security_doctrine\":["
               "\"Memory bounds checking\","
               "\"Race condition prevention\","
               "\"Kernel launch validation\","
               "\"Device memory leak prevention\""
               "]}}\n", req_id);
        fflush(stdout);
    }
    else if (strcmp(method, "analyze_code") == 0) {
        char code[MAX_CODE] = {0};
        extract_string_param(json_str, "code", code, sizeof(code));
        
        /* SERVICE AGENT: Analyze CUDA code */
        int lines = 0, kernels = 0, global_mem = 0, shared_mem = 0;
        const char *p = code;
        while (*p) {
            if (*p == '\n') lines++;
            p++;
        }
        if (lines == 0 && strlen(code) > 0) lines = 1;
        
        if (strstr(code, "__global__")) kernels++;
        if (strstr(code, "cudaMalloc")) global_mem++;
        if (strstr(code, "__shared__")) shared_mem++;
        
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{"
               "\"language\":\"cuda\","
               "\"lines\":%d,"
               "\"kernels\":%d,"
               "\"global_memory_ops\":%d,"
               "\"shared_memory_ops\":%d,"
               "\"agent\":\"Kernel Service Agent\"}}\n",
               req_id, lines, kernels, global_mem, shared_mem);
        fflush(stdout);
    }
    else if (strcmp(method, "list_agents") == 0) {
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"agents\":["
               "{\"agent_id\":\"cuda-service-1\",\"name\":\"Kernel Service Agent\",\"role\":\"Service\"},"
               "{\"agent_id\":\"cuda-data-1\",\"name\":\"Memory Data Agent\",\"role\":\"Data\"},"
               "{\"agent_id\":\"cuda-ops-1\",\"name\":\"Operations Agent\",\"role\":\"Operations\"}"
               "]}}\n", req_id);
        fflush(stdout);
    }
    else if (strcmp(method, "list_tasks") == 0) {
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"tasks\":[]}}\n", req_id);
        fflush(stdout);
    }
    else if (strcmp(method, "add_agent") == 0) {
        char agent_id[64] = {0}, name[MAX_NAME] = {0}, role[64] = {0};
        extract_string_param(json_str, "agent_id", agent_id, sizeof(agent_id));
        extract_string_param(json_str, "name", name, sizeof(name));
        extract_string_param(json_str, "role", role, sizeof(role));
        
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"status\":\"success\","
               "\"agent\":{\"agent_id\":\"%s\",\"name\":\"%s\",\"role\":\"%s\"}}}\n",
               req_id, agent_id, name, role);
        fflush(stdout);
    }
    else if (strcmp(method, "create_task") == 0) {
        char task_id[64] = {0}, title[MAX_NAME] = {0};
        extract_string_param(json_str, "task_id", task_id, sizeof(task_id));
        extract_string_param(json_str, "title", title, sizeof(title));
        
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"status\":\"success\","
               "\"task\":{\"task_id\":\"%s\",\"title\":\"%s\",\"status\":\"pending\"}}}\n",
               req_id, task_id, title);
        fflush(stdout);
    }
    else if (strcmp(method, "shutdown") == 0) {
        printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"status\":\"shutdown\"}}\n", req_id);
        fflush(stdout);
    }
    else {
        send_error(req_id, -32601, "Method not found");
    }
}

/* DATA/MODEL AGENT: Validate memory bounds for kernel launch */
int validate_kernel_bounds(int n, int threads_per_block, int *blocks, int *threads) {
    if (n <= 0 || threads_per_block <= 0 || threads_per_block > 1024) {
        return -1;
    }
    
    *threads = threads_per_block;
    *blocks = (n + threads_per_block - 1) / threads_per_block;
    
    if (*blocks <= 0 || *blocks > 65535) {
        return -2;
    }
    
    return 0;
}

/* OPERATIONS AGENT: Safe vector addition on GPU */
cudaError_t safe_vector_add(const float *h_a, const float *h_b, float *h_c, int n) {
    float *d_a = NULL, *d_b = NULL, *d_c = NULL;
    cudaError_t err = cudaSuccess;
    
    /* Validate input */
    if (h_a == NULL || h_b == NULL || h_c == NULL || n <= 0) {
        return cudaErrorInvalidValue;
    }
    
    /* Allocate device memory */
    err = cudaMalloc(&d_a, n * sizeof(float));
    if (err != cudaSuccess) goto cleanup;
    
    err = cudaMalloc(&d_b, n * sizeof(float));
    if (err != cudaSuccess) goto cleanup;
    
    err = cudaMalloc(&d_c, n * sizeof(float));
    if (err != cudaSuccess) goto cleanup;
    
    /* Copy to device */
    err = cudaMemcpy(d_a, h_a, n * sizeof(float), cudaMemcpyHostToDevice);
    if (err != cudaSuccess) goto cleanup;
    
    err = cudaMemcpy(d_b, h_b, n * sizeof(float), cudaMemcpyHostToDevice);
    if (err != cudaSuccess) goto cleanup;
    
    /* Launch kernel */
    int threads_per_block = 256;
    int blocks = (n + threads_per_block - 1) / threads_per_block;
    
    vector_add_kernel<<<blocks, threads_per_block>>>(d_a, d_b, d_c, n);
    
    err = cudaGetLastError();
    if (err != cudaSuccess) goto cleanup;
    
    err = cudaDeviceSynchronize();
    if (err != cudaSuccess) goto cleanup;
    
    /* Copy result back */
    err = cudaMemcpy(h_c, d_c, n * sizeof(float), cudaMemcpyDeviceToHost);
    
cleanup:
    if (d_a) cudaFree(d_a);
    if (d_b) cudaFree(d_b);
    if (d_c) cudaFree(d_c);
    
    return err;
}

/* SERVICE AGENT: Vector addition kernel */
__global__ void vector_add_kernel(const float *a, const float *b, float *c, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    
    /* Bounds check */
    if (idx < n) {
        c[idx] = a[idx] + b[idx];
    }
}

/* OPERATIONS AGENT: Matrix multiplication kernel with tiling */
__global__ void matrix_multiply_kernel(const float *A, const float *B, float *C,
                                       int M, int N, int K) {
    __shared__ float shared_A[16][16];
    __shared__ float shared_B[16][16];
    
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;
    
    float sum = 0.0f;
    
    /* Tiled multiplication for coalesced memory access */
    for (int tile = 0; tile < (K + 15) / 16; tile++) {
        /* Load tiles into shared memory with bounds checking */
        if (row < M && (tile * 16 + threadIdx.x) < K) {
            shared_A[threadIdx.y][threadIdx.x] = A[row * K + tile * 16 + threadIdx.x];
        } else {
            shared_A[threadIdx.y][threadIdx.x] = 0.0f;
        }
        
        if ((tile * 16 + threadIdx.y) < K && col < N) {
            shared_B[threadIdx.y][threadIdx.x] = B[(tile * 16 + threadIdx.y) * N + col];
        } else {
            shared_B[threadIdx.y][threadIdx.x] = 0.0f;
        }
        
        __syncthreads();
        
        /* Compute partial dot product */
        for (int k = 0; k < 16; k++) {
            sum += shared_A[threadIdx.y][k] * shared_B[k][threadIdx.x];
        }
        
        __syncthreads();
    }
    
    /* Write result with bounds checking */
    if (row < M && col < N) {
        C[row * N + col] = sum;
    }
}

/* Helper: Extract method from JSON */
int extract_method(const char *json, char *method, size_t size) {
    const char *key = "\"method\"";
    const char *p = strstr(json, key);
    if (!p) return 0;
    
    p = strchr(p, ':');
    if (!p) return 0;
    p++;
    
    while (*p && (*p == ' ' || *p == '\t' || *p == '\"')) p++;
    
    size_t i = 0;
    while (*p && *p != '\"' && *p != ',' && *p != '}' && i < size - 1) {
        method[i++] = *p++;
    }
    method[i] = '\0';
    
    return i > 0;
}

/* Helper: Extract id from JSON */
int extract_id(const char *json) {
    const char *key = "\"id\"";
    const char *p = strstr(json, key);
    if (!p) return 0;
    
    p = strchr(p, ':');
    if (!p) return 0;
    
    return atoi(p + 1);
}

/* Helper: Extract string parameter from JSON */
int extract_string_param(const char *json, const char *key, char *value, size_t size) {
    char search[128];
    snprintf(search, sizeof(search), "\"%s\"", key);
    
    const char *p = strstr(json, search);
    if (!p) return 0;
    
    p = strchr(p, ':');
    if (!p) return 0;
    p++;
    
    while (*p && (*p == ' ' || *p == '\t' || *p == '\"')) p++;
    
    size_t i = 0;
    while (*p && *p != '\"' && *p != ',' && *p != '}' && i < size - 1) {
        value[i++] = *p++;
    }
    value[i] = '\0';
    
    return i > 0;
}

/* Send error response */
void send_error(int req_id, int code, const char *msg) {
    printf("{\"jsonrpc\":\"2.0\",\"id\":%d,\"error\":{\"code\":%d,\"message\":\"%s\"}}\n",
           req_id, code, msg);
    fflush(stdout);
}
