-module(aestratum_session).

-export([new/1,
         handle_event/2
        ]).

-export([mode/1,
         phase/1,
         req_id/1,
         last_req/1,
         timer/1
        ]).

-record(state, {
          mode,
          phase,
          req_id = 0,
          last_req,
          timer
        }).

-define(IS_TIMER(T), is_reference(T)).

new(Mode) ->
    #state{mode = Mode, phase = connected}.

% server: connected -> (configured) -> subscribed -> authorized -> disconnected
handle_event({conn, What}, State) ->
    handle_conn_event(What, State);
handle_event({chain, What}, State) ->
    %% TODO: set_target, notify
    handle_chain_event(What, State).

mode(#state{mode = Mode}) ->
    Mode.

phase(#state{phase = Phase}) ->
    Phase.

req_id(#state{req_id = ReqId}) ->
    ReqId.

last_req(#state{last_req = LastReq}) ->
    LastReq.

timer(#state{timer = Timer}) ->
    Timer.

handle_conn_event(init, #state{mode = server, phase = Phase} = State) ->
    %% Set a timer when a new connection is initiated to limit the wait time
    %% for subscribe or configure requests.
    {no_send, State#state{timer = set_timer(Phase)}};
handle_conn_event(init, #state{mode = client} = State) ->
    %% Set a timer when a new connectoin is initiated. In case there is no
    %% response, the client may retry to send the request again.
    send_configure_req(State);
handle_conn_event(RawMsg, #state{mode = Mode} = State) when is_binary(RawMsg) ->
    case aestratum_jsonrpc:decode(RawMsg) of
        {ok, Msg} ->
            case Mode of
                server -> recv_msg_on_server(Msg, State);
                client -> recv_msg_on_client(Msg, State)
            end;
        {error, Rsn} ->
            recv_msg_error(Mode, Rsn, State)
    end;
handle_conn_event(timeout, #state{mode = Mode} = State) ->
    case Mode of
        server -> server_timeout(State);
        client -> client_timeout(State)
    end;
handle_conn_event(close, State) ->
    %% TODO: reason, log
    {stop, State#state{phase = disconnected, timer = undefined}}.

handle_chain_event(_X, State) ->
    {no_send, State}.


server_timeout(#state{phase = Phase, timer = {_TRef, Phase}} = State) when
      (Phase =:= connected) or (Phase =:= configured) or (Phase =:= subscribed) ->
    %% The timer's phase is the same as the current phase, to the timeout
    %% applies and the connection to the client is closed.
    %% TODO: reason, log
    {stop, State#state{phase = disconnected, timer = undefined}};
server_timeout(State) ->
    {no_send, State}.

client_timeout(#state{phase = connected, timer = {_TRef, connected}} = State) ->
    %% TODO: also subscribe request is possible here
    send_configure_req(State);
client_timeout(#state{phase = configured, timer = {_TRef, configured}} = State) ->
    send_subscribe_req(State);
client_timeout(#state{phase = subscribed, timer = {_TRef, subscribed}} = State) ->
    send_authorize_req(State);
client_timeout(#state{phase = Phase} = State) when
      (Phase =:= connected) or (Phase =:= configured) or (Phase =:= subscribed) ->
    {no_send, State}.

recv_msg_on_server(#{type := req, method := configure} = Req,
                   #state{phase = connected} = State) ->
    send_configure_rsp(Req, State);
recv_msg_on_server(#{type := req, method := subscribe} = Req,
                   #state{phase = connected} = State) ->
    send_subscribe_rsp(Req, State);
recv_msg_on_server(#{type := req, method := subscribe} = Req,
                   #state{phase = configured} = State) ->
    send_subscribe_rsp(Req, State);
recv_msg_on_server(#{type := req, method := authorize} = Req,
                   #state{phase = subscribed} = State) ->
    send_authorize_rsp(Req, State);
recv_msg_on_server(#{type := req, method := submit} = Req,
                   #state{phase = authorized} = State) ->
    send_submit_rsp(Req, State);
recv_msg_on_server(_Msg, State) ->
    %% TODO
    Rsp = <<>>,
    {send, Rsp, State}.

recv_msg_on_client(#{type := rsp, id := Id} = Rsp,
                   #state{last_req = #{id := Id, method := Method}} = State) ->
    case aestratum_jsonrpc:validate_rsp(Method, Rsp) of
        {ok, Rsp1} ->
            recv_msg_on_client1(Rsp1, State);
        {error, Rsn} ->
            recv_msg_error(client, Rsn, State)
    end;
recv_msg_on_client(#{type := ntf} = _Ntf, State) ->
    {no_send, State}.

recv_msg_on_client1(#{method := configure, result := []},
                    #state{timer = Timer} = State) ->
    %% TODO: configure not supported
    cancel_timer(Timer),
    send_subscribe_req(State#state{phase = configured, timer = undefined});
recv_msg_on_client1(#{method := subscribe, result := [SessionId, ExtraNonce]},
                    #state{timer = Timer} = State) ->
    %% TODO: log successful subscribe
    %% TODO: save SessionId(?) and ExtraNonce
    cancel_timer(Timer),
    send_authorize_req(State#state{phase = subscribed, timer = undefined});
recv_msg_on_client1(#{method := authorize, result := true},
                    #state{timer = Timer} = State) ->
    %% TODO: log authorization success
    cancel_timer(Timer),
    {no_send, State#state{phase = authorized, timer = undefined}};
recv_msg_on_client1(#{method := authorize, result := false},
                    #state{timer = Timer} = State) ->
    %% TODO: log invalid user/password
    cancel_timer(Timer),
    {stop, State#state{phase = disconnected}};
recv_msg_on_client1(#{method := submit, result := true}, State) ->
    %% TODO: log successful submit
    {no_send, State};
recv_msg_on_client1(#{method := submit, result := false}, State) ->
    %% TODO: log unsuccessful submit
    {no_send, State};
recv_msg_on_client1(#{method := Method, reason := Rsn, msg := ErrMsg,
                      data := ErrData}, State) ->
    %% TODO: log error response
    %% TODO: maybe retry
    {no_send, State}.

recv_msg_error(Mode, Rsn, State) ->
    %% TODO: handle error
    {no_send, State}.

%% server -> client
send_configure_rsp(#{id := Id, params := []},
                   #state{phase = Phase, timer = Timer} = State) ->
    %% TODO: no configure params supported
    cancel_timer(Timer),
    RspMap = #{type => rsp, method => configure, id => Id, result => []},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    %% Set timer for subscribe request.
    {send, Rsp, State#state{phase = configure, timer = set_timer(configured)}}.

send_subscribe_rsp(#{id := Id, user_agent := UserAgent,
                     session_id := SessionId, host := Host, port := Port},
                   #state{phase = Phase, timer = Timer} = State) ->
    %% TODO: session resumption not supported (session cache)
    %% TODO: check SessionId, Host, Port
    cancel_timer(Timer),
    SessionId1 = null,
    ExtraNonce = <<"0123456789">>,
    RspMap = #{type => rsp, method => subscribe, id => Id,
               result => [SessionId1, ExtraNonce]},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    %% Set timer for authorize request.
    {send, Rsp, State#state{phase = subscribed, timer = set_timer(subscribed)}}.

send_authorize_rsp(#{id := Id, user := User, password := Password},
                   #state{timer = Timer} = State) ->
    %% TODO: check user/password against database
    cancel_timer(Timer),
    Authorized = true,
    RspMap = #{type => rsp, method => authorize, id => Id,
               result => Authorized},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    %% No need to set timer after authorization, there are no further
    %% expected requests within a time period. Submit requests do not
    %% require any timeout.
    {send, Rsp, State#state{phase = authorized, timer = undefined}}.

send_submit_rsp(#{id := Id, user := User, job_id := JobId,
                  miner_nonce := MinerNonce, pow := Pow}, State) ->
    %% TODO: read from the database based on JobId: BlockVersion, HeaderHash,
    %% Target, ExtraNonce and verify the submitted solution
    %% TODO: if successful, write the solution/share to the db and compute
    %% reward which will be paid later (at least 180 block)
    Submitted = true,
    RspMap = #{type => rsp, method => submit, id => Id, result => Submitted},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_configure_req(#state{phase = Phase, req_id = Id} = State) ->
    ReqMap = #{type => req, method => configure, id => Id, params => []},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap,
                            timer = set_timer(Phase)}}.

%% client -> server
send_subscribe_req(#state{phase = Phase, req_id = Id} = State) ->
    UserAgent = <<"ae/0.0.1">>,
    Host = <<"localhost">>,
    Port = 9999,
    ReqMap = #{type => req, method => subscribe, id => Id,
               user_agent => UserAgent, session_id => null, host => Host,
               port => Port},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap,
                            timer = set_timer(Phase)}}.

send_authorize_req(#state{phase = Phase, req_id = Id} = State) ->
    User = <<"ae_user">>,
    Password = <<0:32/unit:8>>,
    ReqMap = #{type => req, method => authorize, id => Id,
               user => User, password => Password},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap,
                            timer = set_timer(Phase)}}.

send_submit_req(#state{req_id = Id} = State) ->
    User = <<"ae_user">>,
    JobId = <<"0123456789abcdef">>,
    MinerNonce = <<"012356789">>,
    Pow = lists:seq(1, 42),
    ReqMap = #{type => req, method => submit, id => Id,
               user => User, job_id => JobId, miner_nonce => MinerNonce,
               pow => Pow},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap}}.

set_timer(Phase) ->
    Timeout = application:get_env(aestratum, timeout, 30000),
    TRef = erlang:send_after(Timeout, self(), timeout),
    {TRef, Phase}.

cancel_timer({TRef, _Phase}) when is_reference(TRef) ->
    erlang:cancel_timer(TRef).

next_id(Id) ->
    aestratum_utils:next_id(Id).

