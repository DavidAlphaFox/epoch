%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate engine
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_engine_test).

-include_lib("eunit/include/eunit.hrl").

excute_test() ->
    #{accumulator := 42} =
        aefa_fate:run(
          make_call(<<"test">>, <<"id">>, [42]),
          setup_chain()),

    Res2 = aefa_fate:run(
             make_call(<<"test">>, <<"jumps">>, []),
             setup_chain()),
    io:format("Res2 ~p~n", [Res2]),
    #{accumulator := 2} =
        aefa_fate:run(
          make_call(<<"test">>, <<"inc">>, [1]),
          setup_chain()),
    ok.

make_call(Contract, Function, Arguments) ->
    #{ contract  => Contract
     , function  => Function
     , arguments => Arguments}.
    

setup_chain() ->
    #{ contracts => setup_contracts()}.


setup_contracts() ->
    #{ <<"test">> => [ {<<"id">>, [{0, [return]}]}
                     , {<<"jumps">>,
                        [{0, [ nop
                             , {jump, 3}]}
                        ,{1, [ nop ]}
                        ,{2, [ nop
                             , return]}
                       , {3, [ nop
                             , {jump, 1}]}]
                       }
                     , {<<"inc">>,
                        [{0, [ push_a_0
                             , inc_a_1_a
                             , inc_a_1_a
                             , return
                             ]}]
                       }
                     ]
       }.

                       


    
