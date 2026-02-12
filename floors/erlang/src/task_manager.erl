%%%-------------------------------------------------------------------
%%% @doc
%%% Task manager gen_server
%%% Manages tasks with fault-tolerant design
%%% @end
%%%-------------------------------------------------------------------
-module(task_manager).
-behaviour(gen_server).

-export([start_link/0, create_task/3, get_task/1, list_tasks/0, count_tasks/0, update_status/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tasks = #{}}).

%%% Client API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_task(TaskId, Title, AssignedTo) ->
    gen_server:call(?MODULE, {create_task, TaskId, Title, AssignedTo}).

get_task(TaskId) ->
    gen_server:call(?MODULE, {get_task, TaskId}).

list_tasks() ->
    gen_server:call(?MODULE, list_tasks).

count_tasks() ->
    gen_server:call(?MODULE, count_tasks).

update_status(TaskId, Status) ->
    gen_server:call(?MODULE, {update_status, TaskId, Status}).

%%% Server Callbacks

init([]) ->
    error_logger:info_msg("Task manager started~n"),
    {ok, #state{}}.

handle_call({create_task, TaskId, Title, AssignedTo}, _From, State) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Timestamp = io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                              [Year, Month, Day, Hour, Min, Sec]),
    Task = #{
        task_id => TaskId,
        title => Title,
        status => <<"pending">>,
        assigned_to => AssignedTo,
        created_at => list_to_binary(lists:flatten(Timestamp))
    },
    NewTasks = maps:put(TaskId, Task, State#state.tasks),
    NewState = State#state{tasks = NewTasks},
    {reply, {ok, Task}, NewState};

handle_call({get_task, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} -> {reply, {ok, Task}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(list_tasks, _From, State) ->
    TaskList = maps:values(State#state.tasks),
    {reply, TaskList, State};

handle_call(count_tasks, _From, State) ->
    Count = maps:size(State#state.tasks),
    {reply, Count, State};

handle_call({update_status, TaskId, Status}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            UpdatedTask = Task#{status => Status},
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            NewState = State#state{tasks = NewTasks},
            {reply, {ok, UpdatedTask}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
