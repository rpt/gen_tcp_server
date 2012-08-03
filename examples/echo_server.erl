%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc Example echo server module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(echo_server).

-behaviour(gen_tcp_server).

%% API
-export([run/1]).

%% gen_tcp_server callbacks
-export([handle_accept/1,
         handle_tcp/3,
         handle_close/2]).

-record(state, {}).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

%% @doc Start a TCP echo server.
-spec run(integer()) -> ok.
run(Port) ->
    gen_tcp_server:start_link(?MODULE, Port).

%%%-----------------------------------------------------------------------------
%%% gen_tcp_server_handler callbacks
%%%-----------------------------------------------------------------------------

%% @private
handle_accept(_Socket) ->
    {ok, #state{}}.

%% @private
handle_tcp(Socket, Data, State) ->
    gen_tcp:send(Socket, Data),
    {ok, State}.

%% @private
handle_close(_Socket, _Reason) ->
    ok.
