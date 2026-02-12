%%%-------------------------------------------------------------------
%%% @doc
%%% FLOOR 23 - ERLANG JURISDICTION
%%% Department Floor Main Entry Point
%%% 
%%% Domain: Telecom-grade systems, High-availability, Real-time processing
%%% Architectural Law: Nine-nines reliability, Let it fail, Hot code loading
%%% @end
%%%-------------------------------------------------------------------
-module(department_floor).

-export([main/1]).

main(_Args) ->
    % Start the application
    {ok, _} = application:ensure_all_started(department_floor),
    
    % Print startup info to stderr
    io:format(standard_error, "Erlang Department Floor (Floor 23) - Ready~n", []),
    io:format(standard_error, "Domain: Telecom-grade systems, High-availability services, Real-time processing~n", []),
    io:format(standard_error, "Architectural Law: Nine-nines reliability, Process supervision, Let it fail~n", []),
    io:format(standard_error, "Offices: Architecture, Implementation, Review, Test, Security, Manager~n", []),
    
    % Process requests from stdin
    process_requests().

process_requests() ->
    case io:get_line(standard_io, "") of
        eof ->
            ok;
        {error, Reason} ->
            io:format(standard_error, "Read error: ~p~n", [Reason]),
            process_requests();
        Line ->
            handle_line(string:trim(Line)),
            process_requests()
    end.

handle_line(Line) ->
    try
        Request = jsx:decode(list_to_binary(Line), [return_maps]),
        Response = case floor:handle_request(Request) of
            {ok, Result} -> Result;
            {error, Error} -> Error
        end,
        Json = jsx:encode(Response),
        io:format("~s~n", [Json])
    catch
        error:badarg ->
            Error = #{status => <<"error">>, message => <<"Invalid JSON">>},
            Json = jsx:encode(Error),
            io:format("~s~n", [Json]);
        Type:Reason ->
            Error = #{status => <<"error">>, 
                     message => iolist_to_binary(io_lib:format("Error: ~p:~p", [Type, Reason]))},
            Json = jsx:encode(Error),
            io:format("~s~n", [Json])
    end.
