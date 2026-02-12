%%%-------------------------------------------------------------------
%%% @doc
%%% Floor coordinator for Floor 23
%%% Handles JSON-RPC requests
%%% @end
%%%-------------------------------------------------------------------
-module(floor).

-export([get_floor_info/0, handle_request/1]).

get_floor_info() ->
    Agents = agent_service:list_agents(),
    Tasks = task_manager:list_tasks(),
    AgentCount = agent_service:count_agents(),
    TaskCount = task_manager:count_tasks(),
    Offices = [maps:get(name, O) || O <- office:all_offices()],
    
    #{
        floor_number => 23,
        language => <<"erlang">>,
        domain => <<"Telecom-grade systems, High-availability services, Real-time processing">>,
        architectural_law => <<"Nine-nines reliability, Process supervision, Let it fail, Hot code loading">>,
        security_doctrine => <<"Process isolation, No shared state, Defensive programming, Fail-safe defaults">>,
        offices => Offices,
        agent_count => AgentCount,
        task_count => TaskCount,
        agents => Agents,
        tasks => Tasks
    }.

handle_request(#{<<"method">> := <<"get_info">>}) ->
    {ok, get_floor_info()};

handle_request(#{<<"method">> := <<"add_agent">>, <<"params">> := Params}) ->
    #{
        <<"agent_id">> := AgentId,
        <<"name">> := Name,
        <<"role">> := Role,
        <<"capabilities">> := Capabilities
    } = Params,
    
    case agent_service:add_agent(AgentId, Name, Role, Capabilities) of
        {ok, Agent} ->
            {ok, #{status => <<"success">>, agent => Agent}};
        {error, Reason} ->
            {error, #{status => <<"error">>, message => iolist_to_binary(io_lib:format("Failed to add agent: ~p", [Reason]))}}
    end;

handle_request(#{<<"method">> := <<"create_task">>, <<"params">> := Params}) ->
    #{
        <<"task_id">> := TaskId,
        <<"title">> := Title,
        <<"assigned_to">> := AssignedTo
    } = Params,
    
    case task_manager:create_task(TaskId, Title, AssignedTo) of
        {ok, Task} ->
            {ok, #{status => <<"success">>, task => Task}};
        {error, Reason} ->
            {error, #{status => <<"error">>, message => iolist_to_binary(io_lib:format("Failed to create task: ~p", [Reason]))}}
    end;

handle_request(#{<<"method">> := <<"process_code">>, <<"params">> := Params}) ->
    #{<<"code">> := Code, <<"operation">> := Operation} = Params,
    
    case Operation of
        <<"analyze">> ->
            case code_analyzer:analyze(Code) of
                {ok, Analysis} ->
                    {ok, #{status => <<"success">>, analysis => Analysis}};
                {error, Reason} ->
                    {error, #{status => <<"error">>, message => iolist_to_binary(io_lib:format("~p", [Reason]))}}
            end;
        
        <<"format">> ->
            case code_analyzer:format(Code) of
                {ok, Formatted} ->
                    {ok, #{status => <<"success">>, formatted => true, code => Formatted}};
                {error, Reason} ->
                    {error, #{status => <<"error">>, message => iolist_to_binary(io_lib:format("~p", [Reason]))}}
            end;
        
        <<"lint">> ->
            case code_analyzer:lint(Code) of
                {ok, Warnings} ->
                    {ok, #{status => <<"success">>, warnings => Warnings}};
                {error, Reason} ->
                    {error, #{status => <<"error">>, message => iolist_to_binary(io_lib:format("~p", [Reason]))}}
            end;
        
        _ ->
            {error, #{status => <<"error">>, message => <<"Unknown operation: ", Operation/binary>>}}
    end;

handle_request(#{<<"method">> := Method}) ->
    {error, #{status => <<"error">>, message => <<"Unknown method: ", Method/binary>>}};

handle_request(_) ->
    {error, #{status => <<"error">>, message => <<"Invalid request format">>}}.
