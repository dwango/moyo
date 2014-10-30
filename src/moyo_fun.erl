%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 関数に関する処理を集めたユーティリティモジュール.
-module(moyo_fun).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         try_apply/3, try_apply/4,
         try_call/1, try_call/2,
         repeat/3
        ]).

-export_type([
              stack_item/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type stack_item() :: {Module :: module(),
                       Function :: atom(),
                       Arity :: arity() | (Args :: [term()]),
                       Location :: [{file, Filename :: string()} |
                                    {line, Line :: pos_integer()}]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 指定された関数を実行する. 実行中に例外が発生した場合は`{error, {'EXIT', {Class, Reason, Stacktrace}}}'を返す
-spec try_apply(module(), atom(), [term()]) -> FunctionResult | ErrorResult when
      FunctionResult :: term(),
      ErrorResult    :: {error, {'EXIT', {throw | error | exit, Reason::term(), [stack_item()]}}}.
try_apply(Module, Function, Args) ->
    try
        apply(Module, Function, Args)
    catch
        Class:Reason ->
            {error, {'EXIT', {Class, Reason, erlang:get_stacktrace()}}}
    end.

%% @doc 指定された関数を実行する. 実行中に例外が発生した場合は`ErrorResult'を返す
-spec try_apply(module(), atom(), [term()], ErrorResult) -> FunctionResult | ErrorResult when
      FunctionResult :: term(),
      ErrorResult    :: term().
try_apply(Module, Function, Args, ErrorResult) ->
    try
        apply(Module, Function, Args)
    catch
        _:_ -> ErrorResult
    end.

%% @doc 引数の関数を実行する. 実行中に例外が発生した場合は`{error, {'EXIT', {Class, Reason, Stacktrace}}}'を返す
-spec try_call(function()) -> FunctionResult | ErrorResult when
      FunctionResult :: term(),
      ErrorResult    :: {error, {'EXIT', {throw | error | exit, Reason::term(), [stack_item()]}}}.
try_call(Fun) ->
    try
        Fun()
    catch
        Class:Reason ->
            {error, {'EXIT', {Class, Reason, erlang:get_stacktrace()}}}
    end.

%% @doc 引数の関数を実行する. 実行中に例外が発生した場合は`ErrorResult'を返す
-spec try_call(function(), ErrorResult) -> FunctionResult | ErrorResult when
      FunctionResult :: term(),
      ErrorResult    :: term().
try_call(Fun, ErrorResult) ->
    try
        Fun()
    catch
        _:_ -> ErrorResult
    end.

%% @doc 指定した回数だけ関数を実行する. 関数には loop index in [0, N) が渡される
-spec repeat(Function, InitState::term(), MaxIndex::non_neg_integer()) -> FinalState::term() when
      Function :: fun((Index::non_neg_integer(), State::term())->NextState::term()).
repeat(Fun, State, N) ->
    repeat(Fun, State, 0, N).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec repeat(Function, InitState::term(), Index::non_neg_integer(), MaxIndex::non_neg_integer()) -> FinalState::term() when
      Function :: fun((Index::non_neg_integer(), State::term())->NextState::term()).
repeat(_, State, N, N) -> State;
repeat(Fun, State, LoopIndex, N) ->
    repeat(Fun, Fun(LoopIndex, State), LoopIndex+1, N).
