%%%-------------------------------------------------------------------
%%% @doc
%%% Code analyzer gen_server
%%% Analyzes Erlang code with telecom-grade patterns
%%% @end
%%%-------------------------------------------------------------------
-module(code_analyzer).
-behaviour(gen_server).

-export([start_link/0, analyze/1, format/1, lint/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%% Client API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

analyze(Code) ->
    gen_server:call(?MODULE, {analyze, Code}).

format(Code) ->
    gen_server:call(?MODULE, {format, Code}).

lint(Code) ->
    gen_server:call(?MODULE, {lint, Code}).

%%% Server Callbacks

init([]) ->
    error_logger:info_msg("Code analyzer started~n"),
    {ok, #state{}}.

handle_call({analyze, Code}, _From, State) ->
    Analysis = perform_analysis(Code),
    {reply, {ok, Analysis}, State};

handle_call({format, Code}, _From, State) ->
    % In production, would use proper Erlang formatter
    Formatted = string:trim(binary_to_list(Code)),
    {reply, {ok, list_to_binary(Formatted)}, State};

handle_call({lint, Code}, _From, State) ->
    Warnings = perform_linting(Code),
    {reply, {ok, Warnings}, State};

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

%%% Private Functions

perform_analysis(Code) when is_binary(Code) ->
    CodeStr = binary_to_list(Code),
    Lines = length(string:split(CodeStr, "\n", all)),
    Functions = count_occurrences(CodeStr, "-export("),
    Modules = count_occurrences(CodeStr, "-module("),
    GenServers = count_occurrences(CodeStr, "gen_server"),
    Processes = count_occurrences(CodeStr, "spawn") + count_occurrences(CodeStr, "spawn_link"),
    Supervisors = count_occurrences(CodeStr, "supervisor"),
    
    ReliabilityScore = calculate_reliability(GenServers, Processes, Supervisors),
    
    #{
        lines => Lines,
        functions => Functions,
        modules => Modules,
        gen_servers => GenServers,
        processes => Processes,
        supervisors => Supervisors,
        language => <<"erlang">>,
        reliability_score => ReliabilityScore
    }.

perform_linting(Code) when is_binary(Code) ->
    CodeStr = binary_to_list(Code),
    Warnings = [],
    
    W1 = case string:str(CodeStr, "exit(") of
        0 -> Warnings;
        _ -> [<<"Use proper OTP exit reasons, not raw exit/1">> | Warnings]
    end,
    
    W2 = case {string:str(CodeStr, "gen_server"), string:str(CodeStr, "supervisor")} of
        {0, 0} when length(string:split(CodeStr, "spawn", all)) > 1 ->
            [<<"Consider using gen_server or supervisor instead of raw processes">> | W1];
        _ -> W1
    end,
    
    W3 = case string:str(CodeStr, "catch") of
        0 -> W2;
        _ -> [<<"Avoid catch; use try-catch with proper error handling">> | W2]
    end,
    
    lists:reverse(W3).

count_occurrences(Str, Pattern) ->
    length(string:split(Str, Pattern, all)) - 1.

calculate_reliability(GenServers, Processes, Supervisors) ->
    % Telecom-grade: more gen_servers and supervisors = higher reliability
    Score = (GenServers * 20) + (Supervisors * 30) + (Processes * 5),
    erlang:min(100.0, float(Score)).
