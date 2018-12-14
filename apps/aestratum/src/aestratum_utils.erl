-module(aestratum_utils).

-include("aestratum_jsonrpc.hrl").

-export([is_hex/1,
         is_valid_string/1,
         lowercase/1,
         next_id/1
        ]).

is_hex(Bin) when is_binary(Bin) ->
    lists:all(fun(Byte) when Byte >= $0, Byte =< $9 -> true;
                 (Byte) when Byte >= $a, Byte =< $f -> true;
                 (Byte) when Byte >= $A, Byte =< $F -> true;
                 (_Byte) -> false end, binary_to_list(Bin)).

is_valid_string(Bin) when is_binary(Bin) ->
    lists:all(fun(Byte) when Byte =:= $\s -> false;
                 (Byte) when Byte =:= $\n -> false;
                 (Byte) when Byte =:= $\t -> false;
                 (Byte) when Byte =:= $\v -> false;
                 (Byte) when Byte =:= $\f -> false;
                 (Byte) when Byte =:= $\r -> false;
                 (_Byte) -> true end, binary_to_list(Bin)).

lowercase(Bin) when is_binary(Bin) ->
    string:lowercase(Bin);
lowercase(Other) ->
    Other.

next_id(Id) when is_integer(Id) ->
    (Id + 1) band ?ID_MAX.

