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

-include("gen_tcp_server.hrl").

%%%-----------------------------------------------------------------------------
%%% Internal API functions
%%%-----------------------------------------------------------------------------

%% @private
%% @doc Start the handler supervisor.
-spec start_link(atom(), integer(), term()) -> term().
start_link(HandlerModule, Port, UserOpts) ->
    supervisor:start_link(?MODULE, [HandlerModule, Port, UserOpts]).

%%%-----------------------------------------------------------------------------
%%% Supervisor callbacks
%%%-----------------------------------------------------------------------------

%% @private
init([HandlerModule, Port, UserOpts]) ->
    %% Open listening socket
    Opts = UserOpts ++ ?GEN_TCP_SERVER_OPTS,
    {ok, LSocket} = gen_tcp:listen(Port, Opts),

    HandlerSpec = {gen_tcp_server_handler,
                   {gen_tcp_server_handler, start_link, [LSocket,
                                                         HandlerModule]},
                   temporary, infinity, worker, [gen_tcp_server_handler]},
    {ok, {{simple_one_for_one, 0, 1}, [HandlerSpec]}}.
