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
%% @doc Example echo server module.
-module(echo_server).

-behaviour(gen_tcp_server).

%% API
-export([run/1]).

%% gen_tcp_server callbacks
-export([handle_accept/1,
         handle_tcp/3,
         handle_close/3]).

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
handle_close(_Socket, _Reason, _State) ->
    ok.
