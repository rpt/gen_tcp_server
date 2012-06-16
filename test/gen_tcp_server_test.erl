%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc Eunit test suite for gen_tcp_server.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tcp_server_test).

-export([handle_accept/1,
         handle_tcp/3]).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 1234).

%%%-----------------------------------------------------------------------------
%%% Test functions
%%%-----------------------------------------------------------------------------

connection_test() ->
    %% Disable error_logger output to the console
    error_logger:tty(false),

    %% Start the server
    {ok, Pid} = gen_tcp_server:start_link(?MODULE, ?PORT, [{reuseaddr, true}]),
    ?assert(is_process_alive(Pid)),

    %% Try to connect to it (should successed)
    ?assertMatch({ok, _}, gen_tcp:connect({127,0,0,1}, ?PORT, [])),

    %% Stop the server
    gen_tcp_server:stop(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

%%%-----------------------------------------------------------------------------
%%% Helper functions
%%%-----------------------------------------------------------------------------

handle_accept(_) ->
    {ok, state}.

handle_tcp(_, _, state) ->
    {ok, state}.
