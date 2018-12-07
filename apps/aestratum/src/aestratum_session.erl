-module(aestratum_session).

-export([handle_msg/2]).

handle_msg(RawMsg, _State) ->
    aestratum_jsonrpc:decode(RawMsg).

