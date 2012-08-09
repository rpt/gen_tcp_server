%%------------------------------------------------------------------------------
%% Copyright 2012 Krzysztof Rutka
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%------------------------------------------------------------------------------

%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%% @copyright 2012 Krzysztof Rutka
%% @doc API module for the generic TCP server.
-module(gen_tcp_server).

%% API
-export([start_link/2,
         start_link/3,
         stop/1]).

%%------------------------------------------------------------------------------
%% gen_tcp_server callback definitions
%%------------------------------------------------------------------------------

-callback handle_accept(Socket :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_tcp(Socket :: term(), Data :: binary(), State :: term()) ->
    {ok, NewState :: term()} | {stop, Reason :: term()}.

-callback handle_close(Socket :: term(),
                       Reason :: normal | {tcp_error, term()} |
                                 {handle_accept_error, term()} |
                                 {handle_tcp_error, term()},
                       State :: term()) -> ok.

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Start gen_tcp_server with default options.
-spec start_link(atom(), integer()) -> {ok, Pid :: pid()} | ignore |
                                       {error, Reason :: term()}.
start_link(HandlerModule, Port) ->
    start_link(HandlerModule, Port, []).

%% @doc Start gen_tcp_server with custom options for gen_tcp:listen.
-spec start_link(atom(), integer(), [term()]) -> {ok, Pid :: pid()} | ignore |
                                                 {error, Reason :: term()}.
start_link(HandlerModule, Port, Opts) ->
    {ok, Pid} = gen_tcp_server_sup:start_link(HandlerModule, Port, Opts),
    N = case lists:keyfind(pool, 1, Opts) of
            false ->
                1;
            {pool, PoolSize} ->
                PoolSize
        end,
    [{ok, _} = supervisor:start_child(Pid, []) || _ <- lists:seq(1, N)],
    {ok, Pid}.

%% @doc Stop gen_tcp_server.
-spec stop(pid()) -> true.
stop(Pid) ->
    exit(Pid, normal).
