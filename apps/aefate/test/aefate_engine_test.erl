%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate engine
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_engine_test).

-include_lib("eunit/include/eunit.hrl").

excute_test() ->
    Chain = setup_chain(),
    #{accumulator := 42} =
        aefa_fate:run(
          make_call(<<"test">>, <<"id">>, [42]),
          Chain),

    Res2 = aefa_fate:run(
             make_call(<<"test">>, <<"jumps">>, []),
             Chain),
    io:format("Res2 ~p~n", [Res2]),
    #{accumulator := 2} =
        aefa_fate:run(
          make_call(<<"test">>, <<"inc">>, [0]),
          Chain),
    #{accumulator := 4} =
        aefa_fate:run(
          make_call(<<"test">>, <<"call">>, [0]),
          Chain),
    #{accumulator := 3} =
        aefa_fate:run(
          make_call(<<"test">>, <<"tailcall">>, [0]),
          Chain),
    #{accumulator := 6} =
        aefa_fate:run(
          make_call(<<"remote">>, <<"add_five">>, [1]),
          Chain),
    #{accumulator := 10} =
        aefa_fate:run(
          make_call(<<"test">>, <<"remote_call">>, [4]),
          Chain),
    #{accumulator := 9} =
        aefa_fate:run(
          make_call(<<"test">>, <<"remote_tailcall">>, [4]),
          Chain),
    ok.

make_call(Contract, Function, Arguments) ->
    #{ contract  => Contract
     , function  => Function
     , arguments => Arguments}.
    

setup_chain() ->
    #{ contracts => setup_contracts()}.


setup_contracts() ->
    #{ <<"test">> =>
           [ {<<"id">>, {[integer], integer}, [{0, [return]}]}
           , {<<"jumps">>, {[], integer},
              [{0, [ push_a_0
                   , {jump, 3}]}
              ,{1, [ nop ]}
              ,{2, [ nop
                   , return]}
              , {3, [ nop
                    , {jump, 1}]}]
             }
           , {<<"inc">>, {[integer],integer},
              [{0, [ inc_a_1_a
                   , inc_a_1_a
                   , return
                   ]}]
             }
           , {<<"call">>, {[integer],integer},
              [{0, [ inc_a_1_a
                   , {call_local, <<"inc">>}]}
              ,{1, [ inc_a_1_a
                   , return]}
              ]
             }
           , {<<"tailcall">>, {[integer],integer},
              [{0, [ inc_a_1_a
                   , {tailcall_local, <<"inc">>}]}
              ]
             }
           , { <<"remote_call">>
             , {[integer],integer}
             , [ {0, [ {call_remote, <<"remote">>, <<"add_five">>} ]}
               , {1, [ inc_a_1_a,
                       return]}
               ]
             }
           , { <<"remote_tailcall">>
             , {[integer],integer}
             , [ {0, [ {tailcall_remote, <<"remote">>, <<"add_five">>} ]}
               ]
             }
           ]
     , <<"remote">> =>
           [ {<<"add_five">>, {[integer], integer},
              [{0, [{add_a_i_a, 5}
                    , return]}]
             }
           ]
       }.

                       


    
