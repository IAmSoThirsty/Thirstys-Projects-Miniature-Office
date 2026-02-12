%%%-------------------------------------------------------------------
%%% @doc
%%% Agent service gen_server
%%% Manages floor agents with telecom-grade reliability
%%% @end
%%%-------------------------------------------------------------------
-module(agent_service).
-behaviour(gen_server).

-export([start_link/0, add_agent/4, get_agent/1, list_agents/0, count_agents/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {agents = #{}}).

%%% Client API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_agent(AgentId, Name, Role, Capabilities) ->
    gen_server:call(?MODULE, {add_agent, AgentId, Name, Role, Capabilities}).

get_agent(AgentId) ->
    gen_server:call(?MODULE, {get_agent, AgentId}).

list_agents() ->
    gen_server:call(?MODULE, list_agents).

count_agents() ->
    gen_server:call(?MODULE, count_agents).

%%% Server Callbacks

init([]) ->
    error_logger:info_msg("Agent service started~n"),
    {ok, #state{}}.

handle_call({add_agent, AgentId, Name, Role, Capabilities}, _From, State) ->
    Agent = #{
        agent_id => AgentId,
        name => Name,
        role => Role,
        capabilities => Capabilities
    },
    NewAgents = maps:put(AgentId, Agent, State#state.agents),
    NewState = State#state{agents = NewAgents},
    {reply, {ok, Agent}, NewState};

handle_call({get_agent, AgentId}, _From, State) ->
    case maps:find(AgentId, State#state.agents) of
        {ok, Agent} -> {reply, {ok, Agent}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(list_agents, _From, State) ->
    AgentList = maps:values(State#state.agents),
    {reply, AgentList, State};

handle_call(count_agents, _From, State) ->
    Count = maps:size(State#state.agents),
    {reply, Count, State};

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
