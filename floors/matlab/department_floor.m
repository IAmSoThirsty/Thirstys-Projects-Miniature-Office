% FLOOR 25 - MATLAB/OCTAVE JURISDICTION
% Department Floor Implementation
%
% Domain: Numerical modeling, Matrix operations, Signal processing, Visualization
% Architectural Law: Vectorization > loops, Matrix dimensions explicit
% Security Doctrine: Dimension validation, NaN/Inf checking, Index bounds

function department_floor()
    % Main entry point - read JSON-RPC from stdin, write to stdout
    
    while true
        try
            line = input('', 's');
            if isempty(line)
                break;
            end
            handle_request(line);
        catch ME
            % End of input or error
            break;
        end
    end
end

function handle_request(json_str)
    % Parse and route JSON-RPC request
    
    try
        req = parse_json(json_str);
        
        if ~isfield(req, 'method') || ~isfield(req, 'id')
            send_error(0, -32600, 'Invalid Request');
            return;
        end
        
        method = req.method;
        req_id = req.id;
        
        % Route to appropriate handler
        switch method
            case 'initialize'
                handle_initialize(req_id);
            case 'get_floor_info'
                handle_get_floor_info(req_id);
            case 'add_agent'
                handle_add_agent(req, req_id);
            case 'create_task'
                handle_create_task(req, req_id);
            case 'analyze_code'
                handle_analyze_code(req, req_id);
            case 'list_agents'
                handle_list_agents(req_id);
            case 'list_tasks'
                handle_list_tasks(req_id);
            case 'shutdown'
                handle_shutdown(req_id);
                return;
            otherwise
                send_error(req_id, -32601, 'Method not found');
        end
    catch ME
        send_error(0, -32700, 'Parse error');
    end
end

function handle_initialize(req_id)
    % Initialize the floor
    
    result = struct();
    result.status = 'initialized';
    result.floor = 25;
    result.language = 'matlab';
    result.capabilities = {'matrix_operations', 'numerical_modeling', 'signal_processing'};
    
    send_response(req_id, result);
end

function handle_get_floor_info(req_id)
    % Return comprehensive floor information
    
    result = struct();
    result.floor_number = 25;
    result.language = 'matlab';
    result.domain = 'Numerical modeling, Matrix operations, Signal processing, Visualization';
    
    result.offices = {'Architecture Office', 'Implementation Office', ...
                      'Review Office', 'Test Office', ...
                      'Security Office', 'Manager Office'};
    
    % Define agents (Service, Data, Operations)
    agents = {};
    
    agent1 = struct();
    agent1.agent_id = 'matlab-service-1';
    agent1.name = 'Matrix Service Agent';
    agent1.role = 'Service';
    agent1.capabilities = {'vectorization', 'matrix_analysis', 'performance_optimization'};
    agents{1} = agent1;
    
    agent2 = struct();
    agent2.agent_id = 'matlab-data-1';
    agent2.name = 'Data Model Agent';
    agent2.role = 'Data';
    agent2.capabilities = {'dimension_validation', 'type_checking', 'data_structure_management'};
    agents{2} = agent2;
    
    agent3 = struct();
    agent3.agent_id = 'matlab-ops-1';
    agent3.name = 'Operations Agent';
    agent3.role = 'Operations';
    agent3.capabilities = {'linear_algebra', 'signal_processing', 'numerical_methods'};
    agents{3} = agent3;
    
    result.agents = agents;
    
    result.architectural_laws = {'Vectorization over loops', ...
                                   'Explicit matrix dimensions', ...
                                   'Preallocate arrays', ...
                                   'Avoid dynamic resizing'};
    
    result.security_doctrine = {'Dimension validation before operations', ...
                                 'NaN and Inf checking', ...
                                 'Index bounds verification', ...
                                 'Condition number validation'};
    
    send_response(req_id, result);
end

function handle_add_agent(req, req_id)
    % Add a new agent
    
    params = req.params;
    
    agent = struct();
    agent.agent_id = get_field(params, 'agent_id', 'unknown');
    agent.name = get_field(params, 'name', 'Unknown Agent');
    agent.role = get_field(params, 'role', 'General');
    
    result = struct();
    result.status = 'success';
    result.agent = agent;
    
    send_response(req_id, result);
end

function handle_create_task(req, req_id)
    % Create a new task
    
    params = req.params;
    
    task = struct();
    task.task_id = get_field(params, 'task_id', 'unknown');
    task.title = get_field(params, 'title', 'Untitled Task');
    task.status = 'pending';
    task.assigned_to = get_field(params, 'assigned_to', 'unassigned');
    
    result = struct();
    result.status = 'success';
    result.task = task;
    
    send_response(req_id, result);
end

function handle_analyze_code(req, req_id)
    % Analyze MATLAB/Octave code - SERVICE AGENT
    
    params = req.params;
    code = get_field(params, 'code', '');
    
    % Analyze code structure
    analysis = analyze_matlab_code(code);
    
    result = struct();
    result.language = 'matlab';
    result.lines = analysis.lines;
    result.functions = analysis.functions;
    result.matrices = analysis.matrices;
    result.vectorized_ops = analysis.vectorized_ops;
    result.agent = 'Matrix Service Agent';
    
    send_response(req_id, result);
end

function handle_list_agents(req_id)
    % List all agents
    
    agents = {};
    
    agent1 = struct('agent_id', 'matlab-service-1', 'name', 'Matrix Service Agent', 'role', 'Service');
    agent2 = struct('agent_id', 'matlab-data-1', 'name', 'Data Model Agent', 'role', 'Data');
    agent3 = struct('agent_id', 'matlab-ops-1', 'name', 'Operations Agent', 'role', 'Operations');
    
    agents{1} = agent1;
    agents{2} = agent2;
    agents{3} = agent3;
    
    result = struct();
    result.agents = agents;
    
    send_response(req_id, result);
end

function handle_list_tasks(req_id)
    % List all tasks
    
    result = struct();
    result.tasks = {};
    
    send_response(req_id, result);
end

function handle_shutdown(req_id)
    % Shutdown the floor
    
    result = struct();
    result.status = 'shutdown';
    
    send_response(req_id, result);
end

% ============================================================================
% SERVICE AGENT: Code Analysis
% ============================================================================

function analysis = analyze_matlab_code(code)
    % Analyze MATLAB code structure and patterns
    
    analysis = struct();
    
    % Count lines
    lines = split(code, newline);
    analysis.lines = length(lines);
    
    % Count functions
    analysis.functions = count_pattern(code, 'function');
    
    % Count matrix operations
    analysis.matrices = count_pattern(code, '\[') + count_pattern(code, 'zeros') + ...
                        count_pattern(code, 'ones') + count_pattern(code, 'eye');
    
    % Count vectorized operations
    analysis.vectorized_ops = count_pattern(code, '.*') + count_pattern(code, './') + ...
                              count_pattern(code, '.^') + count_pattern(code, '''');
end

function count = count_pattern(text, pattern)
    % Count occurrences of pattern in text
    
    count = 0;
    if isempty(text) || isempty(pattern)
        return;
    end
    
    matches = strfind(lower(text), lower(pattern));
    count = length(matches);
end

% ============================================================================
% DATA/MODEL AGENT: Dimension Validation
% ============================================================================

function is_valid = validate_matrix_dimensions(A, B, operation)
    % Validate matrix dimensions for operations
    
    is_valid = false;
    
    if isempty(A) || isempty(B)
        return;
    end
    
    [m1, n1] = size(A);
    [m2, n2] = size(B);
    
    switch operation
        case 'multiply'
            is_valid = (n1 == m2);
        case 'add'
            is_valid = (m1 == m2) && (n1 == n2);
        case 'subtract'
            is_valid = (m1 == m2) && (n1 == n2);
        otherwise
            is_valid = false;
    end
end

function is_valid = validate_vector_index(vec, idx)
    % Validate vector index bounds
    
    is_valid = false;
    
    if isempty(vec) || idx < 1
        return;
    end
    
    n = length(vec);
    is_valid = (idx <= n);
end

% ============================================================================
% OPERATIONS AGENT: Matrix Operations
% ============================================================================

function [C, status] = safe_matrix_multiply(A, B)
    % Safely multiply matrices with dimension validation
    
    status = 0;
    C = [];
    
    % Validate inputs
    if ~isnumeric(A) || ~isnumeric(B)
        status = -1;
        return;
    end
    
    % Check dimensions
    if ~validate_matrix_dimensions(A, B, 'multiply')
        status = -2;
        return;
    end
    
    % Perform multiplication
    try
        C = A * B;
        
        % Check for NaN or Inf
        if any(isnan(C(:))) || any(isinf(C(:)))
            status = -3;
        end
    catch
        status = -4;
    end
end

function [v, status] = safe_vector_access(vec, idx)
    % Safely access vector element with bounds checking
    
    status = 0;
    v = NaN;
    
    if ~validate_vector_index(vec, idx)
        status = -1;
        return;
    end
    
    v = vec(idx);
end

% ============================================================================
% JSON Utilities
% ============================================================================

function req = parse_json(json_str)
    % Simple JSON parser for basic structures
    
    % Try using built-in JSON decoder if available (MATLAB R2016b+)
    if exist('jsondecode', 'file')
        req = jsondecode(json_str);
        return;
    end
    
    % Fallback: manual parsing for basic JSON
    req = struct();
    
    % Extract method
    method_match = regexp(json_str, '"method"\s*:\s*"([^"]+)"', 'tokens');
    if ~isempty(method_match)
        req.method = method_match{1}{1};
    end
    
    % Extract id
    id_match = regexp(json_str, '"id"\s*:\s*(\d+)', 'tokens');
    if ~isempty(id_match)
        req.id = str2double(id_match{1}{1});
    else
        req.id = 0;
    end
    
    % Extract params if present
    if contains(json_str, '"params"')
        req.params = struct();
        
        % Extract simple string params
        param_matches = regexp(json_str, '"(\w+)"\s*:\s*"([^"]*)"', 'tokens');
        for i = 1:length(param_matches)
            key = param_matches{i}{1};
            value = param_matches{i}{2};
            if ~strcmp(key, 'method') && ~strcmp(key, 'jsonrpc')
                req.params.(key) = value;
            end
        end
    end
end

function send_response(req_id, result)
    % Send JSON-RPC response
    
    % Try using built-in JSON encoder if available
    if exist('jsonencode', 'file')
        response = struct();
        response.jsonrpc = '2.0';
        response.id = req_id;
        response.result = result;
        fprintf('%s\n', jsonencode(response));
    else
        % Fallback: manual JSON construction
        fprintf('{"jsonrpc":"2.0","id":%d,"result":', req_id);
        print_struct(result);
        fprintf('}\n');
    end
    
    % Flush output
    if exist('OCTAVE_VERSION', 'builtin')
        fflush(stdout);
    else
        drawnow;
    end
end

function send_error(req_id, error_code, error_msg)
    % Send JSON-RPC error response
    
    fprintf('{"jsonrpc":"2.0","id":%d,"error":{"code":%d,"message":"%s"}}\n', ...
            req_id, error_code, error_msg);
    
    if exist('OCTAVE_VERSION', 'builtin')
        fflush(stdout);
    else
        drawnow;
    end
end

function print_struct(s)
    % Print struct as JSON (simple implementation)
    
    fprintf('{');
    fields = fieldnames(s);
    for i = 1:length(fields)
        if i > 1
            fprintf(',');
        end
        fprintf('"%s":', fields{i});
        
        value = s.(fields{i});
        if ischar(value)
            fprintf('"%s"', value);
        elseif isnumeric(value) && isscalar(value)
            fprintf('%g', value);
        elseif iscell(value)
            fprintf('[');
            for j = 1:length(value)
                if j > 1
                    fprintf(',');
                end
                if isstruct(value{j})
                    print_struct(value{j});
                elseif ischar(value{j})
                    fprintf('"%s"', value{j});
                end
            end
            fprintf(']');
        elseif isstruct(value)
            print_struct(value);
        end
    end
    fprintf('}');
end

function value = get_field(s, field, default)
    % Safely get field from struct with default
    
    if isfield(s, field)
        value = s.(field);
    else
        value = default;
    end
end
