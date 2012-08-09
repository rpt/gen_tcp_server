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
%% @doc Supervisor for connection handlers.
%% @private
-module(gen_tcp_server_sup).

-behaviour(supervisor).

%% Internal API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-include("gen_tcp_server.hrl").

%%------------------------------------------------------------------------------
%% Internal API functions
%%------------------------------------------------------------------------------

%% @private
%% @doc Start the handler supervisor.
-spec start_link(atom(), integer(), term()) -> term().
start_link(HandlerModule, Port, UserOpts) ->
    supervisor:start_link(?MODULE, [HandlerModule, Port, UserOpts]).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

%% @private
init([HandlerModule, Port, UserOpts]) ->
    %% Open listening socket
    Opts = UserOpts ++ ?GEN_TCP_SERVER_OPTS,
    {ok, LSocket} = gen_tcp:listen(Port, remove_opts(Opts)),

    HandlerSpec = {gen_tcp_server_handler,
                   {gen_tcp_server_handler, start_link, [LSocket,
                                                         HandlerModule]},
                   temporary, infinity, worker, [gen_tcp_server_handler]},
    {ok, {{simple_one_for_one, 0, 1}, [HandlerSpec]}}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @doc Removes custom opts.
%% @private
remove_opts(Opts) ->
    remove_opts(Opts, Opts).

%% @private
remove_opts([], Opts) ->
    Opts;
remove_opts([{pool, _} | Rest], _Opts) ->
    remove_opts(Rest, Rest);
remove_opts([_ | Rest], Opts) ->
    remove_opts(Rest, Opts).
