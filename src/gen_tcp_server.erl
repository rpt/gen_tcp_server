%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc API module for the generic TCP server.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tcp_server).

%% API
-export([start_link/2,
         start_link/3]).

%%%-----------------------------------------------------------------------------
%%% gen_tcp_server callback definitions
%%%-----------------------------------------------------------------------------

-callback handle_accept(Socket :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_tcp(Socket :: term(), Data :: binary(), State :: term()) ->
    {ok, NewState :: term()} | {stop, Reason :: term()}.

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

%% @doc Start gen_tcp_server with default options.
-spec start_link(atom(), integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(HandlerModule, Port) ->
    start_link(HandlerModule, Port, []).

%% @doc Start gen_tcp_server with custom options for gen_tcp:listen.
-spec start_link(atom(), integer(), term()) -> {ok, pid()} | ignore |
                                               {error, term()}.
start_link(HandlerModule, Port, Opts) ->
    {ok, Pid} = gen_tcp_server_sup:start_link(HandlerModule, Port, Opts),
    {ok, _} = supervisor:start_child(Pid, []),
    {ok, Pid}.
