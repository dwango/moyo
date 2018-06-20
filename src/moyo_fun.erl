%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 関数に関する処理を集めたユーティリティモジュール.
-module(moyo_fun).

-include("moyo_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         try_apply/3, try_apply/4,
         try_call/1, try_call/2,
         repeat/3,
         apply_on_exit/4,
         fold_range/4, maybe_fold_range/4,
         map_range/3,
         composite_apply/2, composite_apply/1
        ]).

-export_type([
              stack_item/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API(Internal Function)
%%----------------------------------------------------------------------------------------------------------------------
-export([
         apply_on_exit_impl/4,
         apply_on_exit_receiver/4
        ]).


%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type stack_item() :: {Module :: module(),
                       Function :: atom(),
                       Arity :: arity() | (Args :: [term()]),
                       Location :: [{file, Filename :: string()} |
                                    {line, Line :: pos_integer()}]}.

-type computation_status() :: {ok, Response :: term()} | ok | {error, Reason :: term()} | error.
%% 合成関数を構成する関数一つ一つの返り値の定義.

-type computation() :: fun(() -> computation_status()) | fun((InputValue::term()) -> computation_status()).
%% 合成関数を構成する関数一つ一つの定義．個々の関数はarityを0又は1とし,computation_status()を返すものとする.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Pidsで指定したプロセスのうちの一つでも死んだら指定の関数を実行する.
-spec apply_on_exit([pid()], module(), atom(), [term()]) -> Executor::pid().
apply_on_exit(Pids = [_|_], Module, Function, Args) ->
    spawn_opt(?MODULE, apply_on_exit_impl, [Pids, Module, Function, Args], [{min_heap_size, 0}, {min_bin_vheap_size, 0}]);
apply_on_exit(Pids, Module, Function, Args) ->
    error(badarg, [Pids, Module, Function, Args]).


%% @doc 指定された関数を実行する. 実行中に例外が発生した場合は`{error, {'EXIT', {Class, Reason, Stacktrace}}}'を返す
-spec try_apply(module(), atom(), [term()]) -> FunctionResult | ErrorResult when
      FunctionResult :: term(),
      ErrorResult    :: {error, {'EXIT', {throw | error | exit, Reason::term(), [stack_item()]}}}.
try_apply(Module, Function, Args) ->
    try
        apply(Module, Function, Args)
    catch
        Class:Reason ?CAPTURE_STACKTRACE ->
            {error, {'EXIT', {Class, Reason, ?GET_STACKTRACE}}}
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
        Class:Reason ?CAPTURE_STACKTRACE ->
            {error, {'EXIT', {Class, Reason, ?GET_STACKTRACE}}}
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

%% @doc `{error, Reason}'を返した場合に途中で処理を中断し, 結果を返す {@link fold_range/4}
-spec maybe_fold_range(Fun, AccIn :: term(), From :: integer(), To :: integer()) -> {ok, Result :: term()} | {error, Reason} when
      Fun     :: fun((Index :: integer(), AccIn :: term()) -> {ok, AccOut :: term()} | {error, Reason}),
      Reason  :: term().
maybe_fold_range(Function, AccIn, From, To) when From =< To ->
    case Function(From, AccIn) of
        {ok, AccOut} -> maybe_fold_range(Function, AccOut, From + 1, To);
        Err          -> Err
    end;
maybe_fold_range(_, Acc, _, _) ->
    {ok, Acc}.

%% @doc 関数に loop X in [From, To] と直前の結果を渡して最後の結果を返す.
-spec fold_range( Function, AccIn::term(), From::integer(), To::integer() ) -> AccOut::term() when
    Function :: fun((Index::integer(), AccIn::term()) -> AccOut::term()).
fold_range(Function, Acc, From, To) when From =< To ->
    fold_range(Function, Function(From, Acc), From + 1, To);
fold_range(_, Acc, _, _) ->
    Acc.

%% @doc 関数に loop X in [From, To] を渡して各々の結果をリストで返す.
-spec map_range( Function, From::integer(), To::integer() ) -> [AccOut::term()] when
    Function :: fun((X::integer()) -> AccOut::term()).
map_range(Function, From, To) ->
    lists:reverse(fold_range(fun(X, AccIn) -> [Function(X)|AccIn] end, [], From, To)).

%% @doc 関数のリストを先頭から順に実行する.先頭の関数が引数をとる場合は,
%%      その引数を本関数の第二引数にリストで指定する.
%%      リスト中の各関数の返り値を,次に実行される関数の引数として渡して
%%      errorを吐くまで実行していく.
-spec composite_apply( FunctionList::[computation()], Arg::term() ) -> computation_status().
composite_apply(_FunctionList=[], Arg) -> {ok, Arg};
composite_apply(_FunctionList=[Function|RestFunctions], Arg) when is_function(Function, 1) ->
    next_composite_apply(RestFunctions, Function(Arg));
composite_apply(FunctionList, Arg) ->
    error(bad_arity, [FunctionList, Arg]).

%% @doc 関数のリストを先頭から順に実行する.ただし,先頭の関数は引数をとらない.
-spec composite_apply( FunctionList::[computation()] ) -> computation_status().
composite_apply(_FunctionList=[]) -> ok;
composite_apply(_FunctionList=[Function|RestFunctions]) when is_function(Function, 0) ->
    next_composite_apply(RestFunctions, Function());
composite_apply(FunctionList) ->
    error(bad_arity, [FunctionList]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec repeat(Function, InitState::term(), Index::non_neg_integer(), MaxIndex::non_neg_integer()) -> FinalState::term() when
      Function :: fun((Index::non_neg_integer(), State::term())->NextState::term()).
repeat(_, State, N, N) -> State;
repeat(Fun, State, LoopIndex, N) ->
    repeat(Fun, Fun(LoopIndex, State), LoopIndex+1, N).

-spec apply_on_exit_impl([pid()], module(), atom(), [term()]) -> Executor::pid().
apply_on_exit_impl(Pids, Module, Function, Args) ->
    RefList = [erlang:monitor(process, Pid) || Pid <- Pids],
    apply_on_exit_receiver(RefList, Module, Function, Args).


-spec apply_on_exit_receiver([reference()], module(), atom(), [term()]) -> Executor::pid().
apply_on_exit_receiver(RefList, Module, Function, Args) ->
    receive
        {'DOWN', Ref, process, _, _} ->
            case lists:member(Ref,RefList) of
                true -> apply(Module, Function, Args);
                false -> ?MODULE:apply_on_exit_receiver(RefList, Module, Function, Args)
            end;
        _ ->
            ?MODULE:apply_on_exit_receiver(RefList, Module, Function, Args) % DOWN以外のメッセージは無視
    end.

-spec next_composite_apply( RestFunctions::[computation()], Ret::computation_status() ) -> computation_status().
next_composite_apply(RestFunctions, Ret) ->
    case Ret of
        {ok, RetVal}                        -> composite_apply(RestFunctions, RetVal);
        ok                                  -> composite_apply(RestFunctions);
        {error, Error}                      -> {error, Error};
        error                               -> error
    end.
