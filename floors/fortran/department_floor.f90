! FLOOR 24 - FORTRAN JURISDICTION
! Department Floor Implementation
!
! Domain: Scientific computation, Numerical analysis, HPC simulations
! Architectural Law: Numerical precision > convenience, Array bounds mandatory
! Security Doctrine: Array bounds checking, Numerical stability validation

program department_floor
    use iso_fortran_env, only: output_unit, error_unit, input_unit
    implicit none
    
    integer, parameter :: MAX_LINE = 8192
    integer, parameter :: MAX_AGENTS = 100
    integer, parameter :: MAX_TASKS = 100
    integer, parameter :: MAX_NAME = 256
    integer, parameter :: MAX_CODE = 16384
    
    character(len=MAX_LINE) :: line
    integer :: ios, request_id
    
    ! Initialize and run main loop
    call process_requests()
    
contains

    ! Main request processing loop
    subroutine process_requests()
        implicit none
        character(len=MAX_LINE) :: input_line
        integer :: io_status
        
        do
            ! Read from stdin
            read(input_unit, '(A)', iostat=io_status) input_line
            if (io_status /= 0) exit
            
            ! Process the request
            call handle_request(trim(adjustl(input_line)))
        end do
    end subroutine process_requests
    
    ! Handle a single JSON-RPC request
    subroutine handle_request(json_str)
        implicit none
        character(len=*), intent(in) :: json_str
        character(len=64) :: method
        integer :: req_id
        
        if (len_trim(json_str) == 0) return
        
        ! Extract method and id
        call extract_json_field(json_str, 'method', method)
        req_id = extract_json_int(json_str, 'id')
        
        ! Route to appropriate handler
        select case (trim(method))
            case ('initialize')
                call handle_initialize(req_id)
            case ('get_floor_info')
                call handle_get_floor_info(req_id)
            case ('add_agent')
                call handle_add_agent(json_str, req_id)
            case ('create_task')
                call handle_create_task(json_str, req_id)
            case ('analyze_code')
                call handle_analyze_code(json_str, req_id)
            case ('list_agents')
                call handle_list_agents(req_id)
            case ('list_tasks')
                call handle_list_tasks(req_id)
            case ('shutdown')
                call handle_shutdown(req_id)
                stop
            case default
                call send_error(req_id, -32601, 'Method not found')
        end select
    end subroutine handle_request
    
    ! Initialize response
    subroutine handle_initialize(req_id)
        implicit none
        integer, intent(in) :: req_id
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)') ',"result":{"status":"initialized",' // &
            '"floor":24,"language":"fortran",' // &
            '"capabilities":["scientific_compute","numerical_analysis","array_operations"]}}'
        flush(output_unit)
    end subroutine handle_initialize
    
    ! Get floor information
    subroutine handle_get_floor_info(req_id)
        implicit none
        integer, intent(in) :: req_id
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)') ',"result":{' // &
            '"floor_number":24,' // &
            '"language":"fortran",' // &
            '"domain":"Scientific computation, Numerical analysis, HPC simulations",' // &
            '"offices":["Architecture Office","Implementation Office",' // &
            '"Review Office","Test Office","Security Office","Manager Office"],' // &
            '"agents":[' // &
            '{"agent_id":"fortran-service-1","name":"Numerical Service Agent",' // &
            '"role":"Service","capabilities":["precision_control","rounding_analysis"]},' // &
            '{"agent_id":"fortran-data-1","name":"Array Data Agent",' // &
            '"role":"Data","capabilities":["array_validation","bounds_checking"]},' // &
            '{"agent_id":"fortran-ops-1","name":"Operations Agent",' // &
            '"role":"Operations","capabilities":["matrix_operations","numerical_methods"]}' // &
            '],' // &
            '"architectural_laws":[' // &
            '"Numerical precision over convenience",' // &
            '"Mandatory array bounds checking",' // &
            '"Explicit array dimensions",' // &
            '"Column-major ordering"' // &
            '],' // &
            '"security_doctrine":[' // &
            '"Array bounds validation",' // &
            '"Numerical stability checks",' // &
            '"Overflow detection",' // &
            '"Precision loss warnings"' // &
            ']}}'
        flush(output_unit)
    end subroutine handle_get_floor_info
    
    ! Add agent handler
    subroutine handle_add_agent(json_str, req_id)
        implicit none
        character(len=*), intent(in) :: json_str
        integer, intent(in) :: req_id
        character(len=64) :: agent_id, name, role
        
        call extract_json_field(json_str, 'agent_id', agent_id)
        call extract_json_field(json_str, 'name', name)
        call extract_json_field(json_str, 'role', role)
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)', advance='no') ',"result":{"status":"success","agent":{"agent_id":"'
        write(output_unit, '(A)', advance='no') trim(agent_id)
        write(output_unit, '(A)', advance='no') '","name":"'
        write(output_unit, '(A)', advance='no') trim(name)
        write(output_unit, '(A)', advance='no') '","role":"'
        write(output_unit, '(A)', advance='no') trim(role)
        write(output_unit, '(A)') '"}}}'
        flush(output_unit)
    end subroutine handle_add_agent
    
    ! Create task handler
    subroutine handle_create_task(json_str, req_id)
        implicit none
        character(len=*), intent(in) :: json_str
        integer, intent(in) :: req_id
        character(len=64) :: task_id, title
        
        call extract_json_field(json_str, 'task_id', task_id)
        call extract_json_field(json_str, 'title', title)
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)', advance='no') ',"result":{"status":"success","task":{"task_id":"'
        write(output_unit, '(A)', advance='no') trim(task_id)
        write(output_unit, '(A)', advance='no') '","title":"'
        write(output_unit, '(A)', advance='no') trim(title)
        write(output_unit, '(A)') '","status":"pending"}}}'
        flush(output_unit)
    end subroutine handle_create_task
    
    ! Analyze code - Scientific Computation Agent
    subroutine handle_analyze_code(json_str, req_id)
        implicit none
        character(len=*), intent(in) :: json_str
        integer, intent(in) :: req_id
        character(len=MAX_CODE) :: code
        integer :: lines, subroutines, arrays, do_loops
        
        call extract_json_field(json_str, 'code', code)
        
        ! Analyze code (Service Agent)
        call analyze_fortran_code(code, lines, subroutines, arrays, do_loops)
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)', advance='no') ',"result":{"language":"fortran","lines":'
        call write_integer(lines)
        write(output_unit, '(A)', advance='no') ',"subroutines":'
        call write_integer(subroutines)
        write(output_unit, '(A)', advance='no') ',"arrays":'
        call write_integer(arrays)
        write(output_unit, '(A)', advance='no') ',"do_loops":'
        call write_integer(do_loops)
        write(output_unit, '(A)') ',"agent":"Numerical Service Agent"}}'
        flush(output_unit)
    end subroutine handle_analyze_code
    
    ! List agents
    subroutine handle_list_agents(req_id)
        implicit none
        integer, intent(in) :: req_id
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)') ',"result":{"agents":[' // &
            '{"agent_id":"fortran-service-1","name":"Numerical Service Agent","role":"Service"},' // &
            '{"agent_id":"fortran-data-1","name":"Array Data Agent","role":"Data"},' // &
            '{"agent_id":"fortran-ops-1","name":"Operations Agent","role":"Operations"}' // &
            ']}}'
        flush(output_unit)
    end subroutine handle_list_agents
    
    ! List tasks
    subroutine handle_list_tasks(req_id)
        implicit none
        integer, intent(in) :: req_id
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)') ',"result":{"tasks":[]}}'
        flush(output_unit)
    end subroutine handle_list_tasks
    
    ! Shutdown handler
    subroutine handle_shutdown(req_id)
        implicit none
        integer, intent(in) :: req_id
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)') ',"result":{"status":"shutdown"}}'
        flush(output_unit)
    end subroutine handle_shutdown
    
    ! Error response
    subroutine send_error(req_id, error_code, error_msg)
        implicit none
        integer, intent(in) :: req_id, error_code
        character(len=*), intent(in) :: error_msg
        
        write(output_unit, '(A)', advance='no') '{"jsonrpc":"2.0","id":'
        call write_integer(req_id)
        write(output_unit, '(A)', advance='no') ',"error":{"code":'
        call write_integer(error_code)
        write(output_unit, '(A)', advance='no') ',"message":"'
        write(output_unit, '(A)', advance='no') trim(error_msg)
        write(output_unit, '(A)') '"}}'
        flush(output_unit)
    end subroutine send_error
    
    ! SERVICE AGENT: Analyze Fortran code for scientific computing patterns
    subroutine analyze_fortran_code(code, lines, subroutines, arrays, do_loops)
        implicit none
        character(len=*), intent(in) :: code
        integer, intent(out) :: lines, subroutines, arrays, do_loops
        integer :: i, code_len
        logical :: in_comment
        
        lines = 0
        subroutines = 0
        arrays = 0
        do_loops = 0
        code_len = len_trim(code)
        in_comment = .false.
        
        ! Count lines
        do i = 1, code_len
            if (code(i:i) == char(10)) lines = lines + 1
        end do
        if (lines == 0 .and. code_len > 0) lines = 1
        
        ! Count subroutines and functions
        subroutines = count_substring(code, 'subroutine') + &
                     count_substring(code, 'function')
        
        ! Count array declarations (dimension keyword)
        arrays = count_substring(code, 'dimension') + &
                count_substring(code, '::')
        
        ! Count DO loops
        do_loops = count_substring(code, 'do ') + &
                  count_substring(code, 'DO ')
    end subroutine analyze_fortran_code
    
    ! DATA/MODEL AGENT: Validate array bounds
    subroutine validate_array_bounds(array_size, index, is_valid)
        implicit none
        integer, intent(in) :: array_size, index
        logical, intent(out) :: is_valid
        
        is_valid = .false.
        if (index >= 1 .and. index <= array_size) then
            is_valid = .true.
        end if
    end subroutine validate_array_bounds
    
    ! OPERATIONS AGENT: Perform matrix operation with bounds checking
    subroutine safe_matrix_multiply(a, b, c, n, m, p, status)
        implicit none
        integer, intent(in) :: n, m, p
        real(8), intent(in) :: a(n,m), b(m,p)
        real(8), intent(out) :: c(n,p)
        integer, intent(out) :: status
        integer :: i, j, k
        
        status = 0
        
        ! Validate dimensions
        if (n <= 0 .or. m <= 0 .or. p <= 0) then
            status = -1
            return
        end if
        
        ! Perform multiplication with numerical stability
        c = 0.0d0
        do j = 1, p
            do i = 1, n
                do k = 1, m
                    c(i,j) = c(i,j) + a(i,k) * b(k,j)
                end do
            end do
        end do
    end subroutine safe_matrix_multiply
    
    ! Helper: Count substring occurrences
    function count_substring(text, pattern) result(count)
        implicit none
        character(len=*), intent(in) :: text, pattern
        integer :: count
        integer :: pos, text_len, pattern_len
        
        count = 0
        text_len = len_trim(text)
        pattern_len = len_trim(pattern)
        pos = 1
        
        do while (pos <= text_len - pattern_len + 1)
            if (text(pos:pos+pattern_len-1) == pattern) then
                count = count + 1
                pos = pos + pattern_len
            else
                pos = pos + 1
            end if
        end do
    end function count_substring
    
    ! Helper: Extract JSON field
    subroutine extract_json_field(json_str, key, value)
        implicit none
        character(len=*), intent(in) :: json_str, key
        character(len=*), intent(out) :: value
        character(len=512) :: search_str
        integer :: key_pos, start_pos, end_pos
        
        value = ''
        search_str = '"' // trim(key) // '"'
        key_pos = index(json_str, trim(search_str))
        
        if (key_pos == 0) return
        
        start_pos = index(json_str(key_pos:), ':')
        if (start_pos == 0) return
        start_pos = key_pos + start_pos
        
        ! Skip whitespace and opening quote
        do while (start_pos <= len(json_str))
            if (json_str(start_pos:start_pos) == '"') then
                start_pos = start_pos + 1
                exit
            end if
            start_pos = start_pos + 1
        end do
        
        ! Find closing quote
        end_pos = start_pos
        do while (end_pos <= len(json_str))
            if (json_str(end_pos:end_pos) == '"') exit
            end_pos = end_pos + 1
        end do
        
        if (end_pos > start_pos) then
            value = json_str(start_pos:end_pos-1)
        end if
    end subroutine extract_json_field
    
    ! Helper: Extract JSON integer
    function extract_json_int(json_str, key) result(value)
        implicit none
        character(len=*), intent(in) :: json_str, key
        integer :: value
        character(len=64) :: value_str
        integer :: ios
        
        call extract_json_field(json_str, key, value_str)
        read(value_str, *, iostat=ios) value
        if (ios /= 0) value = 0
    end function extract_json_int
    
    ! Helper: Write integer to output
    subroutine write_integer(n)
        implicit none
        integer, intent(in) :: n
        character(len=32) :: str
        
        write(str, '(I0)') n
        write(output_unit, '(A)', advance='no') trim(str)
    end subroutine write_integer

end program department_floor
