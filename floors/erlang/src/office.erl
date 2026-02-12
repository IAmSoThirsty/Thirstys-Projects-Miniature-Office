%%%-------------------------------------------------------------------
%%% @doc
%%% Office definitions for Floor 23
%%% @end
%%%-------------------------------------------------------------------
-module(office).

-export([all_offices/0, get_office/1]).

all_offices() ->
    [
        #{
            type => architecture,
            name => <<"Architecture Office">>,
            description => <<"Telecom-grade system design and OTP architecture">>
        },
        #{
            type => implementation,
            name => <<"Implementation Office">>,
            description => <<"gen_server and supervisor implementation">>
        },
        #{
            type => review,
            name => <<"Review Office">>,
            description => <<"Code review with telecom patterns verification">>
        },
        #{
            type => test,
            name => <<"Test Office">>,
            description => <<"EUnit and Common Test testing">>
        },
        #{
            type => security,
            name => <<"Security Office">>,
            description => <<"Process isolation and nine-nines reliability audit">>
        },
        #{
            type => manager,
            name => <<"Manager Office">>,
            description => <<"Project coordination and rebar3 management">>
        }
    ].

get_office(Type) ->
    lists:keyfind(Type, 1, all_offices()).
