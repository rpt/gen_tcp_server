%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc Connection handler module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tcp_server_handler).

-behaviour(gen_server).

%% Internal API
-export([start_link/3]).

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
          socket :: term(),
          state :: term()
         }).

%%%-----------------------------------------------------------------------------
%%% Internal API functions
%%%-----------------------------------------------------------------------------

%% @private
%% @doc Start gen_server.
-spec start_link(term(), atom(), term()) -> {ok, pid()} | ignore |
                                            {error, term()}.
start_link(LSocket, HandlerModule, InitState) ->
    error_logger:info_msg("A new handler is waiting for a connection", []),
    gen_server:start_link(?MODULE, [self(), LSocket,
                                    HandlerModule, InitState], []).

%%%-----------------------------------------------------------------------------
%%% gen_server callbacks
%%%-----------------------------------------------------------------------------

%% @private
init([Supervisor, LSocket, HandlerModule, InitState]) ->
    %% Timeout 0 will send a timeout message to the gen_server
    %% to handle gen_tcp:accept before any other message.
    {ok, #state{supervisor = Supervisor,
                handler = HandlerModule,
                socket = LSocket,
                state = InitState}, 0}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(timeout, #state{supervisor = Supervisor, handler = HandlerModule,
                            socket = LSocket, state = HandlerState} = State) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    error_logger:info_msg("Accepted a new connection from ~p", [Socket]),

    %% Start new child to wait for the next connection.
    supervisor:start_child(Supervisor, []),

    case HandlerModule:handle_accept(Socket, HandlerState) of
        {ok, NewHandlerState} ->
            {noreply, State#state{socket = Socket, state = NewHandlerState}};
        {stop, Reason} ->
            {stop, Reason, State}
    end;
handle_info({tcp, Socket, Data}, #state{handler = HandlerModule,
                                        socket = Socket,
                                        state = HandlerState} = State) ->
    inet:setopts(Socket, [{active, once}]),

    case HandlerModule:handle_tcp(Socket, Data, HandlerState) of
        {ok, NewHandlerState} ->
            {noreply, State#state{state = NewHandlerState}};
        {error, Reason} ->
            {stop, Reason, State};
        _ ->
            {stop, bad_handler, State}
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
    gen_tcp:close(Socket),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
