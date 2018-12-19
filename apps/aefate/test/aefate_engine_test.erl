%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate engine
%%% @end
%%%-------------------------------------------------------------------

-module(aefate_engine_test).

-include_lib("eunit/include/eunit.hrl").

control_flow_test_() ->
    make_calls(control_flow()).

boolean_test_() ->
    make_calls(booleans()).

comp_test_() ->
    make_calls(comps()).


make_calls(ListOfCalls) ->
    Chain = setup_chain(),
    [{lists:flatten(io_lib:format("call(~p,~p,~p)->~p", [C,F,A,R])),
      fun() ->
              Call = make_call(C,F,A),
              #{accumulator := Res} = aefa_fate:run(Call, Chain),
              ?assertEqual(R, Res)

      end}
     || {C,F,A,R} <- ListOfCalls].



control_flow() ->
    [ {<<"test">>, <<"id">>, [42], 42}
    , {<<"test">>, <<"jumps">>, [], 0}
    , {<<"test">>, <<"inc">>, [0], 2}
    , {<<"test">>, <<"call">>, [0], 4}
    , {<<"test">>, <<"tailcall">>, [0], 3}
    , {<<"remote">>, <<"add_five">>, [1], 6}
    , {<<"test">>, <<"remote_call">>, [4],10}
    , {<<"test">>, <<"remote_tailcall">>, [4],9}
    ].

booleans() ->
    [ {<<"bool">>, <<"and">>, [true, true], true}
    , {<<"bool">>, <<"and">>, [true, false], false}
    , {<<"bool">>, <<"or">>,  [true, false], true}
    , {<<"bool">>, <<"not">>,  [true], false}
    , {<<"bool">>, <<"not">>,  [false], true}
    ].

comps() ->
    [ {<<"comp">>, <<"lt">>, [1, 2], true}
    , {<<"comp">>, <<"gt">>, [1, 2], false}
    , {<<"comp">>, <<"elt">>,  [2, 2], true}
    , {<<"comp">>, <<"egt">>,  [3, 2], true}
    , {<<"comp">>, <<"eq">>,  [4, 4], true}
    ].


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
     , <<"bool">> =>
           [ {<<"and">>
             , {[boolean, boolean], boolean}
             , [ {0, [ and_a_a_a
                     , return]}]}
           , {<<"or">>
             , {[boolean, boolean], boolean}
             , [ {0, [ or_a_a_a
                     , return]}]}
           , {<<"not">>
             , {[boolean, boolean], boolean}
             , [ {0, [ not_a_a
                     , return]}]}
           ]

     , <<"comp">> =>
           [ {<<"lt">>
             , {[integer, integer], boolean}
             , [ {0, [ lt_a_a_a
                     , return]}]}
           ,  {<<"gt">>
              , {[integer, integer], boolean}
              , [ {0, [ gt_a_a_a
                      , return]}]}
           ,  {<<"egt">>
              , {[integer, integer], boolean}
              , [ {0, [ egt_a_a_a
                      , return]}]}
           ,  {<<"elt">>
              , {[integer, integer], boolean}
              , [ {0, [ elt_a_a_a
                      , return]}]}
           ,  {<<"eq">>
              , {[integer, integer], boolean}
              , [ {0, [ eq_a_a_a
                      , return]}]}
           ,  {<<"neq">>
              , {[integer, integer], boolean}
              , [ {0, [ neq_a_a_a
                      , return]}]}
           ]

       }.

                       


    
