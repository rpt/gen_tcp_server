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
%% @doc Eunit test suite for gen_tcp_server.
-module(gen_tcp_server_tests).

-export([handle_accept/1,
         handle_tcp/3]).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 1234).
-define(TIMEOUT, 100).

%%------------------------------------------------------------------------------
%% Test functions
%%------------------------------------------------------------------------------

connection_test() ->
    init(),

    ?assertEqual({error, econnrefused}, try_to_connect()),

    start_the_server(),
    ?assert(is_server_running()),
    ?assertEqual(0, number_of_connections()),

    ?assertEqual(ok, try_to_connect()),

    ?assertEqual(1, number_of_connections()),

    ?assertEqual(ok, try_to_connect()),

    ?assertEqual(2, number_of_connections()),

    disconnect_client(),
    ?assertEqual(1, number_of_connections()),

    send_something(),

    stop_the_server(),
    ?assertNot(is_server_running()),

    ?assertEqual({error, econnrefused}, try_to_connect()).

%%------------------------------------------------------------------------------
%% gen_tcp_server callbacks
%%------------------------------------------------------------------------------

handle_accept(_) ->
    {ok, state}.

handle_tcp(_, _, state) ->
    {ok, state}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

init() ->
    %% Disable error_logger output to the console
    error_logger:tty(false),
    put(sockets, []).

start_the_server() ->
    {ok, Pid} = gen_tcp_server:start_link(?MODULE, ?PORT),
    put(pid, Pid),
    timer:sleep(?TIMEOUT).

stop_the_server() ->
    gen_tcp_server:stop(get(pid)),
    timer:sleep(?TIMEOUT).

is_server_running() ->
    is_process_alive(get(pid)).

number_of_connections() ->
    element(2, lists:keyfind(workers, 1,
                             supervisor:count_children(get(pid)))) - 1.

try_to_connect() ->
    case gen_tcp:connect({127,0,0,1}, ?PORT, []) of
        {ok, Socket} ->
            SocketList = get(sockets),
            put(sockets, [Socket | SocketList]),
            timer:sleep(?TIMEOUT),
            ok;
        Else ->
            Else
    end.

send_something() ->
    case lists:reverse(get(sockets)) of
        [Socket | _] ->
            ok = gen_tcp:send(Socket, <<"something">>),
            timer:sleep(?TIMEOUT)
    end.

disconnect_client() ->
    case lists:reverse(get(sockets)) of
        [Socket | Rest] ->
            put(sockets, lists:reverse(Rest)),
            gen_tcp:close(Socket),
            timer:sleep(?TIMEOUT)
    end.
