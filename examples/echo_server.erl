%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc Example echo server module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(echo_server).

-behaviour(gen_tcp_server_handler).

%% API
-export([run/1]).

%% gen_tcp_server_handler callback
-export([handle/3]).

-record(state, {}).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

%% @doc Start a TCP echo server.
-spec run(integer()) -> ok
run(Port) ->
    gen_tcp_server:start(?MODULE, Port, #state{}).

%%%-----------------------------------------------------------------------------
%%% gen_tcp_server_handler callback
%%%-----------------------------------------------------------------------------

%% @private
handle(Socket, Data, State) ->
    gen_tcp:send(Socket, Data),
    {ok, State}.
