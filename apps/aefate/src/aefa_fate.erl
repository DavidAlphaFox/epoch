-module(aefa_fate).
-export([run/2]).

-export([get_trace/1]).

-include("aefa_data.hrl").

-ifdef(TEST).
-define(trace(I,S), S#{trace => [I|get_trace(S)]}).
-else.
-define(trace(I,S), S).
-endif.

run(What, Chain) ->
    EngineState = setup_engine(What, Chain),
    execute(EngineState).

execute(EngineState) ->
    Instructions = current_bb(EngineState),
    loop(Instructions, EngineState).

loop(Instructions, EngineState) ->
    case step(Instructions, EngineState) of
        {stop, FinalState} ->
            FinalState;
        {jump, BB, NewState} ->
            {NewInstructions, State2} = jump(BB, NewState),
            loop(NewInstructions, State2)
    end.

step([], EngineState) ->
    BB = next_bb_index(EngineState),
    {jump, BB, EngineState};
step([I|Is], EngineState0) ->
    ES = ?trace(I, EngineState0),
    case eval(I, ES) of
        {next, NewState} -> step(Is, NewState);
        {jump,_BB,_NewState} = Res -> Res;
        {stop, _NewState} = Res -> Res

    end.

eval(return, EngineState) ->
    {stop, EngineState};
eval({jump, BB}, EngineState) ->
    {jump, BB, EngineState};
eval(push_a_0, EngineState) ->
    {next, push_int(?MAKE_FATE_INTEGER(0), EngineState)};
eval(inc_a_1_a, EngineState) ->
    {next, inc_acc(EngineState)};
eval(nop, EngineState) ->
    {next, EngineState}.




%% -----------------------------------------------------------



setup_engine(#{ contract := Contract
              , function := Function
              , arguments := Arguments}, Chain) ->
    ES1 = new_engine_state(Chain),
    ES2 = set_function(Contract, Function, ES1),
    ES3 = push_arguments(Arguments, ES2),
    ES3.

set_function(Contract, Function, #{ chain := Chain} = ES) ->
    ContractCode = get_contract(Contract, Chain),
    ES1 = ES#{current_contract => Contract},
    ES2 = setup_functions(ContractCode, ES1),
    set_current_function(Function, ES2).

    

setup_functions(ContractCode, ES) ->
    lists:foldl(
      fun({FunctionName, BBs}, State) ->
              set_function_code(FunctionName, BBs, State)
      end,
      ES,
      ContractCode).

set_current_function(Function, ES) ->
    ES1 = ES#{current_function => Function},
    BBs = get_function_code(Function, ES1),
    lists:foldl(
      fun({BB, Instructions}, State) ->
              set_instructions(BB, Instructions, State)
      end,
      ES1,
      BBs).

set_instructions(BB, Is, #{bbs := BBs} = ES) ->
    NewBBs = maps:put(BB, Is, BBs),
    maps:put(bbs, NewBBs, ES).

set_function_code(Name, BBs, #{functions := Functions} = ES) ->
    NewFunctions = maps:put(Name, BBs, Functions),
    maps:put(functions, NewFunctions, ES).

get_function_code(Name, #{functions := Functions}) ->
    case maps:get(Name, Functions, void) of
        void ->  throw({error, {trying_to_call_function, Name}});
        Code -> Code
    end.
             
             
jump(BB, ES) ->
    NewES = set_current_bb_index(BB, ES),
    Instructions = current_bb(NewES),
    {Instructions, NewES}.

push_int(I,
         #{ accumulator := X
          , accumulator_stack := Stack } = ES) when ?IS_FATE_INTEGER(I) ->
    ES#{ accumulator => I
       , accumulator_stack => [X|Stack]}.

push_arguments(Args, #{ accumulator := X
                      , accumulator_stack := XS} = ES) ->
    push_arguments(lists:reverse(Args), X, XS, ES).

push_arguments([], Acc, Stack, ES) ->
    ES#{ accumulator := Acc
       , accumulator_stack := Stack};
push_arguments([A|As], Acc, Stack, ES ) -> 
    push_arguments(As, A, [Acc, Stack], ES).
     

inc_acc(#{accumulator := X} = ES) when ?IS_FATE_INTEGER(X) ->
    ES#{accumulator := ?MAKE_FATE_INTEGER(?FATE_INTEGER_VALUE(X)+1)}.

set_current_bb_index(BB, ES) ->
    ES#{ current_bb => BB }.

current_bb(#{ current_bb := BB} = ES) ->
    get_bb(BB, ES).

get_bb(BB, #{bbs := BBS}) ->
    case maps:get(BB, BBS, void) of
        void -> throw({error, {trying_to_reach_bb, BB}});
        Instrucitions -> Instrucitions
    end.
            

get_trace(#{trace := T}) -> T.

next_bb_index(#{ current_bb := BB}) ->
    %% TODO check BB + 1 exists.
    BB + 1.



new_engine_state(Chain) ->
    #{ current_bb => 0
     , bbs => #{}
     , memory => #{}
     , chain => Chain
     , trace => []
     , accumulator => ?FATE_VOID
     , accumulator_stack => []
     , functions =>  #{}
     , current_contract => ?FATE_VOID
     , current_function => ?FATE_VOID
     }.
             


%% ----------------------------

%% TODO: real chain interface
get_contract(ContractAddress, #{ contracts :=  Contracts} = _Chain) ->
    maps:get(ContractAddress, Contracts).

    
