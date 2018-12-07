-module(aestratum_handler).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
          socket,
          transport,
          session
         }).

-define(IS_MSG(SM), ((SM =:= tcp) or (SM =:= ssl))).
-define(IS_CLOSE(SC), ((SC =:= tcp_closed) or (SC =:= ssl_closed))).
-define(IS_ERROR(SE), ((SE =:= tcp_error) or (SE =:= ssl_error))).

start_link(Ref, Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.



init({Ref, Socket, Transport, Opts}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, line},
                                    {keepalive, true}]),
    gen_server:enter_loop(
      ?MODULE, [], #state{socket = Socket, transport = Transport}).

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({SocketType, Socket, Data}, State) when ?IS_MSG(SocketType) ->
	{noreply, handle_socket_msg(Socket, Data, State)};
handle_info({SocketClose, Socket}, State) when ?IS_CLOSE(SocketClose) ->
	{stop, normal, handle_socket_close(Socket, State)};
handle_info({SocketError, Socket, Reason}, State) when ?IS_ERROR(SocketError) ->
	{stop, Reason, handle_socket_error(Socket, State)};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal

handle_socket_msg(Socket, Data, #state{socket = Socket,
                                       transport = Transport,
                                       session = Session} = State) ->
	Session1 = process_msg(Data, Session),
	Transport:setopts(Socket, [{active, once}]),
    State#state{session = Session1}.

handle_socket_close(_Socket, #state{socket = _Socket} = State) ->
    %% save in cache
    State.

handle_socket_error(_Socket, #state{socket = _Socket} = State) ->
    %% save in cache
    State.

process_msg(Data, State) ->
    aestratum_session:handle_msg(Data, client_req, State).
%
%    case aestratum_session:handle_event(Data, Session) of
%        {reply, ReplyData, Session1} ->
%        {noreply, Session1} ->
%        {stop, Reason, Session1} ->
%        {stop, Reason, Reply, Session1} ->
%    end.
