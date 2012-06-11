%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc API module for the generic TCP server.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tcp_server).

%% API
-export([start_link/3]).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

%% @doc Start gen_tcp_server.
-spec start_link(atom(), integer(), term()) -> {ok, pid()} | ignore |
                                               {error, term()}.
start_link(HandlerModule, Port, Opts) ->
    {ok, Pid} = gen_tcp_server_sup:start_link(HandlerModule, Port, Opts),
    {ok, _} = supervisor:start_child(Pid, []),
    {ok, Pid}.
