%%%-------------------------------------------------------------------
%%% @doc
%%% Supervisor for Floor 23 services
%%% Implements telecom-grade fault tolerance
%%% @end
%%%-------------------------------------------------------------------
-module(department_floor_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    Children = [
        #{
            id => agent_service,
            start => {agent_service, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [agent_service]
        },
        #{
            id => task_manager,
            start => {task_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [task_manager]
        },
        #{
            id => code_analyzer,
            start => {code_analyzer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [code_analyzer]
        }
    ],
    
    {ok, {SupFlags, Children}}.
