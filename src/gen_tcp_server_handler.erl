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
%% @doc Connection handler module.
-module(gen_tcp_server_handler).

-behaviour(gen_server).

%% Internal API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          supervisor :: pid(),
          handler :: atom(),
          lsocket :: term(),
          socket :: term(),
          state :: term()
         }).

%%------------------------------------------------------------------------------
%% Internal API functions
%%------------------------------------------------------------------------------

%% @private
%% @doc Start gen_server.
-spec start_link(term(), atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(LSocket, HandlerModule) ->
    error_logger:info_msg("A new handler is waiting for a connection", []),
    gen_server:start_link(?MODULE, [self(), LSocket, HandlerModule], []).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

%% @private
init([Supervisor, LSocket, HandlerModule]) ->
    %% Timeout 0 will send a timeout message to the gen_server
    %% to handle gen_tcp:accept before any other message.
    {ok, #state{supervisor = Supervisor,
                handler = HandlerModule,
                lsocket = LSocket}, 0}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(timeout, #state{supervisor = Supervisor, handler = HandlerModule,
                            lsocket = LSocket} = State) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            error_logger:info_msg("Accepted a new connection from ~p",
                                  [Socket]),

            %% Start new child to wait for the next connection.
            supervisor:start_child(Supervisor, []),

            case HandlerModule:handle_accept(Socket) of
                {ok, HandlerState} ->
                    {noreply, State#state{socket = Socket,
                                          state = HandlerState}};
                {stop, Reason} ->
                    {stop, {handle_accept_error, Reason}, State};
                _ ->
                    {stop, {handle_accept_error, bad_return}, State}
            end;
        {error, Reason} ->
            {stop, {gen_tcp_accept_error, Reason}, State}
    end;
handle_info({tcp, Socket, Data}, #state{handler = HandlerModule,
                                        socket = Socket,
                                        state = HandlerState} = State) ->
    inet:setopts(Socket, [{active, once}]),

    case HandlerModule:handle_tcp(Socket, Data, HandlerState) of
        {ok, NewHandlerState} ->
            {noreply, State#state{state = NewHandlerState}};
        {error, Reason} ->
            {stop, {handle_tcp_error, Reason}, State};
        _ ->
            {stop, {handle_tcp_error, bad_return}, State}
    end;
handle_info({tcp_closed, Socket}, State) ->
    error_logger:info_msg("Socket ~p closed", [Socket]),
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    error_logger:error_msg("Error on socket ~p: ~p", [Socket, Reason]),
    {stop, {tcp_error, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{socket = Socket}) ->
    %% Close the sockets
    if
        Socket /= undefined ->
            gen_tcp:close(Socket);
        true ->
            ok
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
