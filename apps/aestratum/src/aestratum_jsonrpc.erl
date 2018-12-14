-module(aestratum_jsonrpc).

-include("aestratum_jsonrpc.hrl").

-export([decode/1,
         encode/1,
         validate_rsp/2
        ]).

%% TODO: dialyzer specs

-define(JSONRPC_VERSION, <<"2.0">>).

-define(IS_ID(Id), (Id >= ?ID_MIN) and (Id =< ?ID_MAX)).
-define(IS_HOST(Host), (Host =/= <<>>) and (byte_size(Host) =< ?HOST_MAX_SIZE)).
-define(IS_PORT(Port), (Port > ?PORT_MIN) and (Port =< ?PORT_MAX)).
-define(IS_USER(User), (User =/= <<>>) and (byte_size(User) =< ?USER_MAX_SIZE)).
-define(IS_USER_AGENT(Agent), (Agent =/= <<>>) and (byte_size(Agent) =< ?USER_AGENT_MAX_SIZE)).
-define(IS_SESSION_ID(Id), byte_size(Id) =:= ?SESSION_ID_SIZE).
-define(IS_PASSWORD(Password), byte_size(Password) =:= ?PASSWORD_SIZE).
-define(IS_TARGET(Target), byte_size(Target) =:= ?TARGET_SIZE).
-define(IS_JOB_ID(Id), byte_size(JobId) =:= ?JOB_ID_SIZE).
-define(IS_BLOCK_VERSION(Version), (Version >= ?BLOCK_VERSION_MIN) and (Version =< ?BLOCK_VERSION_MAX)).
-define(IS_HEADER_HASH(Hash), (byte_size(Hash) =:= ?HEADER_HASH_SIZE)).
-define(IS_EMPTY_JOB_QUEUE(EmptyJobQueue), is_boolean(EmptyJobQueue)).
-define(IS_WAIT_TIME(Time), (Time >= ?WAIT_TIME_MIN) and (Time =< ?WAIT_TIME_MAX)).
-define(IS_POW(Pow), is_list(Pow) and (length(Pow) =:= ?POW_SIZE)).
-define(IS_POW_NUMBER(N), (N >= ?POW_NUMBER_MIN) and (N =< ?POW_NUMBER_MAX)).
-define(IS_ERROR_MSG(Msg), (Msg =/= <<>>) and (byte_size(Msg) =< ?ERROR_MSG_MAX_SIZE)).

-define(LOWERCASE(Bin), aestratum_utils:lowercase(Bin)).

decode(RawMsg) when is_binary(RawMsg) ->
    %% raw_msg -> msg -> map
    Data0 = #{op => decode, raw_msg => RawMsg},
    case raw_msg_to_msg(Data0) of
        {ok, Data} ->
            run([fun check_version/1,
                 fun msg_to_map/1], Data);
        {error, Rsn} ->
            decode_error(Rsn, Data0)
    end.

encode(Map) when is_map(Map) ->
    %% map -> msg -> raw_msg
    run([fun map_to_msg/1,
         fun msg_to_raw_msg/1], #{op => encode, map => Map}).

validate_rsp(Method, #{result := Result} = Rsp) ->
    try
        ok = check_result(Method, Result),
        {ok, Rsp#{method => Method}}
    catch
        throw:{validation_error, Rsn} ->
            validation_error(Rsn, #{op => validate_rsp, map => Rsp})
    end;
validate_rsp(Method, #{error := Error} = Rsp) ->
    try
        ok = check_error(Error),
        [Code, Msg, Data] = Error,
        %% replace error with reason, msg, data
        Rsp1 = maps:without([error], Rsp),
        {ok, Rsp1#{method => Method, reason => error_code_to_reason(Code),
                   msg => Msg, data => Data}}
    catch
        throw:{validation_error, Rsn} ->
            validation_error(Rsn, #{op => validate_rsp, map => Rsp})
    end.

run(Funs, Data0) ->
    try
        {ok, lists:foldl(fun(Fun, Data) -> Fun(Data) end, Data0, Funs)}
    catch
        throw:{validation_error, Rsn} ->
            validation_error(Rsn, Data0);
        throw:{encode_error, Rsn} ->
            encode_error(Rsn, Data0);
        error:Rsn ->
            ST = erlang:get_stacktrace(),
            unexpected_error(Rsn, ST, Data0)
    end.

validation_error(Rsn, #{op := decode} = Data) ->
    lager:warning("Validation error, reason: ~p", [Rsn]),
    validation_error_(Rsn, id_from_msg(Data));
validation_error(Rsn, #{op := Op} = Data) when
      (Op =:= encode) or (Op =:= validate_rsp) ->
    lager:warning("Validation error, reason: ~p", [Rsn]),
    validation_error_(Rsn, id_from_map(Data)).

validation_error_(invalid_msg, Id) ->
    {error, {invalid_msg, Id}};
validation_error_({field, Field}, Id) when
      (Field =:= jsonrpc) or (Field =:= id) ->
    {error, {invalid_msg, Id}};
validation_error_({field, method}, Id) ->
    {error, {invalid_method, Id}};
validation_error_({param, Param}, Id) ->
    {error, {invalid_param, Param, Id}}.

decode_error(Rsn, #{op := decode, raw_msg := RawMsg}) ->
    lager:warning("Decode error, reason: ~p, message: ~p", [Rsn, RawMsg]),
    {error, parse_error}.

encode_error(Rsn, #{op := encode, map := Map}) ->
    lager:warning("Encode error, reason: ~p, map: ~p", [Rsn, Map]),
    {error, parse_error}.

unexpected_error(Rsn, ST, #{op := decode, raw_msg := RawMsg} = Data) ->
    lager:error(
      "Unexpected error, reason: ~p, message: ~p, stacktrace: ~9999p",
      [Rsn, RawMsg, ST]),
    {error, {unexpected_error, id_from_msg(Data)}};
unexpected_error(Rsn, ST, #{op := encode, map := Map} = Data) ->
    lager:error(
      "Unexpected error, reason: ~p, map: ~p, stacktrace: ~9999p",
      [Rsn, Map, ST]),
    {error, {unexpected_error, id_from_map(Data)}}.

id_from_msg(#{msg := #{<<"id">> := Id}}) when ?IS_ID(Id) -> Id;
id_from_msg(#{msg := #{<<"id">> := null}}) -> null;
id_from_msg(_Other) -> undefined.

id_from_map(#{map := #{id := Id}}) when ?IS_ID(Id) -> Id;
id_from_map(#{map := #{id := null}}) -> null;
id_from_map(_Other) -> undefined.

raw_msg_to_msg(#{raw_msg := RawMsg} = Data) ->
    try {ok, Data#{msg => jsx:decode(RawMsg, [return_maps])}}
    catch
        error:Rsn -> {error, Rsn}
    end.

msg_to_raw_msg(#{msg := Msg}) ->
    try list_to_binary([jsx:encode(Msg), $\n])
    catch
        error:Rsn -> throw({encode_error, Rsn})
    end.

check_version(#{msg := #{<<"jsonrpc">> := ?JSONRPC_VERSION}} = Data) ->
    Data;

check_version(#{msg := _Msg}) ->
    validation_exception({field, jsonrpc}).

%% Client requests
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.configure">>,
                      <<"params">> := Params}}) ->
    ok = check_configure_req(Id, Params),
    #{type => req, method => configure, id => Id, params => Params};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.subscribe">>,
                      <<"params">> := Params}}) ->
    ok = check_subscribe_req(Id, Params),
    [UserAgent, SessionId, Host, Port] = Params,
    #{type => req, method => subscribe, id => Id, user_agent => UserAgent,
      session_id => ?LOWERCASE(SessionId), host => Host,
      port => Port};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.authorize">>,
                      <<"params">> := Params}}) ->
    ok = check_authorize_req(Id, Params),
    [User, Password] = Params,
    #{type => req, method => authorize, id => Id, user => User,
      password => ?LOWERCASE(Password)};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.submit">>,
                      <<"params">> := Params}}) ->
    ok = check_submit_req(Id, Params),
    [User, JobId, MinerNonce, Pow] = Params,
    #{type => req, method => submit, id => Id, user => User,
      job_id => ?LOWERCASE(JobId), miner_nonce => ?LOWERCASE(MinerNonce),
      pow => Pow};
%% Server requests
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"client.reconnect">>,
                      <<"params">> := Params}}) ->
    ok = check_reconnect_req(Id, Params),
    [Host, Port, WaitTime] = Params,
    #{type => req, method => reconnect, id => Id, host => Host,
      port => Port, wait_time => WaitTime};
%% Server notifications
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.set_target">>,
                      <<"params">> := Params}}) ->
    ok = check_set_target_ntf(Id, Params),
    [Target] = Params,
    #{type => ntf, method => set_target, id => null,
      target => ?LOWERCASE(Target)};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"method">> := <<"mining.notify">>,
                      <<"params">> := Params}}) ->
    ok = check_notify_ntf(Id, Params),
    [JobId, BlockVersion, HeaderHash, EmptyJobQueue] = Params,
    #{type => ntf, method => notify, id => null, job_id => ?LOWERCASE(JobId),
      block_version => BlockVersion, header_hash => ?LOWERCASE(HeaderHash),
      empty_job_queue => EmptyJobQueue};
%% Responses
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"result">> := Result,
                      <<"error">> := null}}) when Result =/= null ->
    ok = check_id(int, Id),
    %% Result is not checked here, the check is done in
    %% validate_rsp_params/2. We don't have info about what
    %% response params are expected here. There is no info
    %% on what kind of response this is.
    #{type => rsp, id => Id, result => Result};
msg_to_map(#{msg := #{<<"id">> := Id,
                      <<"result">> := null,
                      <<"error">> := Error}}) when Error =/= null ->
    ok = check_id(int, Id),
    %% Error is not checked here, the check is done in
    %% validate_rsp_params/2. It could be done here though,
    %% but let's follow the behaviour of how it's done with
    %% the result.
    #{type => rsp, id => Id, error => Error};
%% Msg validation errors
msg_to_map(#{msg := #{<<"id">> := _Id,
                      <<"method">> := _Method,
                      <<"params">> := Params}}) when is_list(Params) ->
    validation_exception({field, method});
msg_to_map(_Data) ->
    validation_exception(invalid_msg).

map_to_msg(#{map := #{type := req, method := configure, id := Id,
                      params := Params}} = Data) ->
    ok = check_configure_req(Id, Params),
    to_req_msg(<<"mining.configure">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := subscribe, id := Id,
                      user_agent := UserAgent, session_id := SessionId,
                      host := Host, port := Port}} = Data) ->
    Params = [UserAgent, SessionId, Host, Port],
    ok = check_subscribe_req(Id, Params),
    to_req_msg(<<"mining.subscribe">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := authorize, id := Id,
                      user := User, password := Password}} = Data) ->
    Params = [User, Password],
    ok = check_authorize_req(Id, Params),
    to_req_msg(<<"mining.authorize">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := submit, id := Id,
                      user := User, job_id := JobId, miner_nonce := MinerNonce,
                      pow := Pow}} = Data) ->
    Params = [User, JobId, MinerNonce, Pow],
    ok = check_submit_req(Id, Params),
    to_req_msg(<<"mining.submit">>, Id, Params, Data);
map_to_msg(#{map := #{type := req, method := reconnect, id := Id,
                      host := Host, port := Port,
                      wait_time := WaitTime}} = Data) ->
    Params = [Host, Port, WaitTime],
    ok = check_reconnect_req(Id, Params),
    to_req_msg(<<"client.reconnect">>, Id, Params, Data);
map_to_msg(#{map := #{type := ntf, method := set_target,
                      target := Target}} = Data) ->
    ok = check_set_target_ntf(null, [Target]),
    to_ntf_msg(<<"mining.set_target">>, [Target], Data);
map_to_msg(#{map := #{type := ntf, method := notify, job_id := JobId,
                      block_version := BlockVersion, header_hash := HeaderHash,
                      empty_job_queue := EmptyJobQueue}} = Data) ->
    Params = [JobId, BlockVersion, HeaderHash, EmptyJobQueue],
    ok = check_notify_ntf(null, Params),
    to_ntf_msg(<<"mining.notify">>, Params, Data);
map_to_msg(#{map := #{type := rsp, method := Method, id := Id,
                      result := Result}} = Data) ->
    ok = check_id(int, Id),
    ok = check_result(Method, Result),
    to_result_rsp_msg(Id, Result, Data);
map_to_msg(#{map := #{type := rsp, id := Id, reason := Rsn} = Map} = Data) ->
    ErrorData = maps:get(data, Map, null),
    ok = check_id(int, Id),
    ok = check_error_data(ErrorData),
    ErrorParams = reason_to_error_params(Rsn, ErrorData),
    to_error_rsp_msg(Id, ErrorParams, Data);
map_to_msg(_Other) ->
    validation_exception(invalid_msg).

to_req_msg(Method, Id, Params, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"method">> => Method,
                   <<"id">> => Id, <<"params">> => Params}}.

to_ntf_msg(Method, Params, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"method">> => Method,
                   <<"id">> => null, <<"params">> => Params}}.

to_result_rsp_msg(Id, Result, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"id">> => Id,
                   <<"result">> => Result, <<"error">> => null}}.

to_error_rsp_msg(Id, Error, Data) ->
    Data#{msg => #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"id">> => Id,
                   <<"result">> => null, <<"error">> => Error}}.

check_configure_req(Id, []) ->
    ok = check_id(int, Id);
check_configure_req(_Id, _Params) ->
    validation_exception({param, configure_params}).

check_subscribe_req(Id, [UserAgent, SessionId, Host, Port]) ->
    ok = check_id(int, Id),
    ok = check_user_agent(UserAgent),
    ok = check_session_id(SessionId),
    ok = check_host(Host),
    ok = check_port(int, Port);
check_subscribe_req(_Id, _Params) ->
    validation_exception({param, subscribe_params}).

check_authorize_req(Id, [User, Password]) ->
    ok = check_id(int, Id),
    ok = check_user(User),
    ok = check_password(Password);
check_authorize_req(_Id, _Params) ->
    validation_exception({param, authorize_params}).

check_submit_req(Id, [User, JobId, MinerNonce, Pow]) ->
    ok = check_id(int, Id),
    ok = check_user(User),
    ok = check_job_id(JobId),
    ok = check_miner_nonce(MinerNonce),
    ok = check_pow(Pow);
check_submit_req(_Id, _Params) ->
    validation_exception({param, submit_params}).

check_reconnect_req(Id, [Host, Port, WaitTime]) ->
    ok = check_id(int, Id),
    ok = check_host(Host),
    ok = check_port(allow_null, Port),
    ok = check_wait_time(WaitTime);
check_reconnect_req(_Id, _Params) ->
    validation_exception({param, reconnect_params}).

check_set_target_ntf(Id, [Target]) ->
    ok = check_id(null, Id),
    ok = check_target(Target);
check_set_target_ntf(_Id, _Params) ->
    validation_exception({param, set_target_params}).

check_notify_ntf(Id, [JobId, BlockVersion, HeaderHash, EmptyJobQueue]) ->
    ok = check_id(null, Id),
    ok = check_job_id(JobId),
    ok = check_block_version(BlockVersion),
    ok = check_header_hash(HeaderHash),
    ok = check_empty_job_queue(EmptyJobQueue);
check_notify_ntf(_Id, _Params) ->
    validation_exception({param, notify_params}).

check_id(null, null) -> ok;
check_id(int, Id) when ?IS_ID(Id) -> ok;
check_id(_Type, _Id) -> validation_exception({field, id}).

check_user_agent(UserAgent) when ?IS_USER_AGENT(UserAgent) ->
    ok = check_valid_string(UserAgent, user_agent),
    case binary:split(UserAgent, <<"/">>) of
        [Client, Version] when Client =/= <<>>, Version =/= <<>> -> ok;
        _Other -> validation_exception({param, user_agent})
    end;
check_user_agent(_UserAgent) ->
    validation_exception({param, user_agent}).

check_session_id(null) ->
    ok;
check_session_id(SessionId) when ?IS_SESSION_ID(SessionId) ->
    ok = check_hex(SessionId, session_id);
check_session_id(_SessionId) ->
    validation_exception({param, session_id}).

check_host(null) ->
    ok;
check_host(Host) when ?IS_HOST(Host) ->
    ok = check_valid_string(Host, host);
check_host(_Host) ->
    validation_exception({param, host}).

check_port(allow_null, null) ->
    ok;
check_port(allow_null, Port) ->
    check_port_(Port);
check_port(int, Port) ->
    check_port_(Port).

check_port_(Port) when ?IS_PORT(Port) ->
    ok;
check_port_(_Port) ->
    validation_exception({param, port}).

check_user(User) when ?IS_USER(User) ->
    ok = check_valid_string(User, user);
check_user(_User) ->
    validation_exception({param, user}).

check_password(Password) when ?IS_PASSWORD(Password) ->
    ok = check_hex(Password, password);
check_password(_Password) ->
    validation_exception({param, password}).

check_target(Target) when ?IS_TARGET(Target) ->
    ok = check_hex(Target, target);
check_target(_Target) ->
    validation_exception({param, target}).

check_job_id(JobId) when ?IS_JOB_ID(JobId) ->
    ok = check_hex(JobId, job_id);
check_job_id(_JobId) ->
    validation_exception({param, job_id}).

check_block_version(BlockVersion) when ?IS_BLOCK_VERSION(BlockVersion) ->
    ok;
check_block_version(_BlockVersion) ->
    validation_exception({param, block_version}).

check_header_hash(HeaderHash) when ?IS_HEADER_HASH(HeaderHash) ->
    ok = check_hex(HeaderHash, header_hash);
check_header_hash(_HeaderHash) ->
    validation_exception({param, header_hash}).

check_empty_job_queue(EmptyJobQueue) when ?IS_EMPTY_JOB_QUEUE(EmptyJobQueue) ->
    ok;
check_empty_job_queue(_EmptyJobQueue) ->
    validation_exception({param, empty_job_queue}).

check_wait_time(WaitTime) when ?IS_WAIT_TIME(WaitTime) ->
    ok;
check_wait_time(_WaitTime) ->
    validation_exception({param, wait_time}).

%% TODO
check_miner_nonce(MinerNonce) when is_binary(MinerNonce) ->
    ok = check_hex(MinerNonce, miner_nonce);
check_miner_nonce(_MinerNonce) ->
    validation_exception({param, miner_nonce}).

%% TODO
check_extra_nonce(ExtraNonce) when is_binary(ExtraNonce) ->
    ok = check_hex(ExtraNonce, extra_nonce);
check_extra_nonce(_ExtraNonce) ->
    validation_exception({param, extra_nonce}).

check_pow(Pow) when ?IS_POW(Pow) ->
    case lists:all(fun(N) when ?IS_POW_NUMBER(N) -> true;
                      (_N) -> false
                   end, Pow) of
        true -> ok;
        false -> validation_exception({param, pow})
    end;
check_pow(_Pow) ->
    validation_exception({param, pow}).

%% Configure response: [] (no config params supported)
%% Subscribe response: [SessionId, ExtraNonce]
%% Authorize response: true | false
%% Submit response:    true | false
check_result(configure, []) ->
    ok;
check_result(subscribe, [SessionId, ExtraNonce]) ->
    ok = check_session_id(SessionId),
    ok = check_extra_nonce(ExtraNonce);
check_result(authorize, Result) when is_boolean(Result) ->
    ok;
check_result(submit, Result) when is_boolean(Result) ->
    ok;
check_result(configure, _Result) ->
    validation_exception({param, configure_params});
check_result(subscribe, _Result) ->
    validation_exception({param, subscribe_params});
check_result(authorize, _Result) ->
    validation_exception({param, authorize_params});
check_result(submit, _Result) ->
    validation_exception({param, submit_params}).

check_error([Code, Msg, Data]) ->
    ok = check_error_code(Code),
    ok = check_error_msg(Msg),
    ok = check_error_data(Data);
check_error(_Error) ->
    validation_exception({param, error_params}).

check_error_code(Code) ->
    _ = error_code_to_reason(Code),
    ok.

check_error_msg(Msg) when ?IS_ERROR_MSG(Msg) ->
    ok;
check_error_msg(_Msg) ->
    validation_exception({param, error_msg}).

%% TODO: size limit?
check_error_data(_Data) -> ok.

check_hex(Bin, Param) ->
    case aestratum_utils:is_hex(Bin) of
        true -> ok;
        false -> validation_exception({param, Param})
    end.

check_valid_string(Bin, Param) ->
    case aestratum_utils:is_valid_string(Bin) of
        true -> ok;
        false -> validation_exception({param, Param})
    end.

validation_exception(Rsn) ->
    throw({validation_error, Rsn}).

reason_to_error_params(parse_error, Data) ->
    [-32700, <<"Parse error">>, Data];
reason_to_error_params(invalid_msg, Data) ->
    [-32000, <<"Invalid request">>, Data];
reason_to_error_params(invalid_method, Data) ->
    [-32601, <<"Method not found">>, Data];
reason_to_error_params(invalid_param, Data)  ->
    [-32602, <<"Invalid params">>, Data];
reason_to_error_params(internal_error, Data) ->
    [-32603, <<"Internal error">>, Data];
reason_to_error_params(unknown_error, Data) ->
    [20, <<"Other/Unknown">>, Data];
reason_to_error_params(job_not_found, Data) ->
    [21, <<"Job not found">>, Data];
reason_to_error_params(duplicate_share, Data) ->
    [22, <<"Duplicate share">>, Data];
reason_to_error_params(low_difficulty_share, Data) ->
    [23, <<"Low difficulty share">>, Data];
reason_to_error_params(unauthorized_worker, Data) ->
    [24, <<"Unauthorized worker">>, Data];
reason_to_error_params(not_subscribed, Data) ->
    [25, <<"Not subscribed">>, Data];
reason_to_error_params(_Rsn, _Data) ->
    validation_exception({param, error_reason}).

error_code_to_reason(-32700) -> parse_error;
error_code_to_reason(-32000) -> invalid_msg;
error_code_to_reason(-32601) -> invalid_method;
error_code_to_reason(-32602) -> invalid_param;
error_code_to_reason(-32603) -> internal_error;
error_code_to_reason(20)     -> unknown_error;
error_code_to_reason(21)     -> job_not_found;
error_code_to_reason(22)     -> duplicate_share;
error_code_to_reason(23)     -> low_difficulty_share;
error_code_to_reason(24)     -> unauthorized_worker;
error_code_to_reason(25)     -> not_subscribed;
error_code_to_reason(_Code)  -> validation_exception({param, error_code}).

