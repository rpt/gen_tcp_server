%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc Supervisor for connection handlers.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tcp_server_sup).

-behaviour(supervisor).

%% Internal API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%%%-----------------------------------------------------------------------------
%%% Internal API functions
%%%-----------------------------------------------------------------------------

%% @private
%% @doc Start the handler supervisor.
-spec start_link(atom(), integer(), term()) -> term().
start_link(HandlerModule, Port, InitState) ->
    Opts = [binary, {packet, raw}, {active, once}, {reuseaddr, true}],
    {ok, LSocket} = gen_tcp:listen(Port, Opts),
    supervisor:start_link(?MODULE, [LSocket, HandlerModule, InitState]).

%%%-----------------------------------------------------------------------------
%%% Supervisor callbacks
%%%-----------------------------------------------------------------------------

%% @private
init(Args) ->
    HandlerSpec = {gen_tcp_server_handler,
                   {gen_tcp_server_handler, start_link, Args},
                   temporary, brutal_kill, worker, [gen_tcp_server_handler]},
    {ok, {{simple_one_for_one, 0, 1}, [HandlerSpec]}}.
