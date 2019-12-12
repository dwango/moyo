%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.3409;0c
%%
%% @doc 並行処理の為のモジュール

-module(moyo_concurrent).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

-export([
         exec/1,
         exec/2,
         exec_sort/1,
         exec_sort/2,
         exec_map/1,
         exec_map/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc 並行に複数のコマンドを実行し、その結果を入力の順に返す.
%%
%% 1つでも結果がerrorだった場合, その1つのerror結果を呼び出し元に投げ, 他のプロセスは強制終了される.
-spec exec_sort([Input]) -> [RetValue :: term()] when
      Input :: {module(), Function :: atom(), Args :: [term()]}.
exec_sort(Inputs) ->
    exec_sort(Inputs, infinity).

-spec exec_sort([Input], Timeout) -> [RetValue :: term()] when
      Input   :: {module(), Function :: atom(), Args :: [term()]},
      Timeout :: timeout().
exec_sort(Inputs, Timeout) ->
    exec_impl(Inputs, Timeout, fun exec_order_by_input/1).

%% @doc 並行に複数のコマンドを実行する
%%
%% see: `exec(Input, infinity)'
-spec exec([Input]) -> [{Input, RetValue :: term()}] when
      Input :: {module(), Function :: atom(), Args :: [term()]}.
exec(Inputs) ->
    exec(Inputs, infinity).

%% @doc 並行に複数のコマンドを実行する
%% 返り値は実行が終了した順番で返される. <br />
%% また, 1つでも結果がerrorだった場合, その1つのerror結果を呼び出し元に投げ, 他のプロセスは強制終了される.
-spec exec([Input], Timeout) -> [{Input, RetValue :: term()}] when
      Input   :: {module(), Function :: atom(), Args :: [term()]},
      Timeout :: timeout().
exec(Inputs, Timeout) ->
    exec_impl(Inputs, Timeout, fun exec_order_by_finish/1).

%% @doc 並行に複数のコマンドを実行する
%%
%% see: `exec_map([Input], infinity)'
-spec exec_map([Input]) -> [RetValue :: term() | {'EXIT', Signal :: term()}] when
      Input   :: {module(), Function :: atom(), Args :: [term()]}.
exec_map(Inputs) ->
    exec_map(Inputs, infinity).

%% @doc 並行に複数のコマンドを実行する
%% 返り値は Inputs の順番で返される. <br />
%% 1つでも結果がerrorだった場合もすべての実行が完了を待ち, 結果のリストは Inputs の写像となる.
-spec exec_map([Input], Timeout) -> [RetValue :: term() | {'EXIT', Signal :: term()}] when
      Input   :: {module(), Function :: atom(), Args :: [term()]},
      Timeout :: timeout().
exec_map(Inputs, Timeout) ->
    exec_impl(Inputs, Timeout, fun exec_order_preserved_by_input/1).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc exec_order/execの実装部分. exec_order_by_finish / exec_order_by_inputを第3引数に指定する.
%%
%% 返り値は第3引数の返り値になる.
-spec exec_impl([Input], Timeout, fun(([Input]) -> Result)) -> Result when
      Input    :: {module(), Function :: atom(), Args :: [term()]},
      Timeout  :: timeout(),
      Result   :: [{Input, RetValue}] | [RetValue],
      RetValue :: term().
exec_impl(Inputs, Timeout, Func) ->
    Self = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
                                       Self ! {self(), Func(Inputs)}
                               end),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            receive {Pid, Result} -> Result end;
        {'DOWN', Ref, process, Pid, {Reason, Args}} -> error(Reason, Args);
        {'DOWN', Ref, process, Pid, Reason}         -> error(Reason)
    after Timeout ->
            _ = exit(Pid, shutdown),
            receive {'DOWN', Ref, process, Pid, _}  -> error(timeout) end
    end.

%% @doc Input要素の関数をプロセスを立てて実行し, その結果を終了順に返す.
-spec exec_order_by_finish([Input]) -> [{Input, RetValue :: term()}] when
      Input :: {module(), Function :: atom(), Args :: [term()]}.
exec_order_by_finish(Inputs) ->
    Self = self(),
    _ = [spawn_link(fun() ->
                            Self ! {{Module, Fun, Args}, apply(Module, Fun, Args)}
                    end)
         || {Module, Fun, Args} <- Inputs],
    [receive {Input, RetValue} -> {Input, RetValue} end || _ <- Inputs].

%% @doc Input要素の関数をプロセスを立てて実行し, 結果を入力順に返す.
-spec exec_order_by_input([Input]) -> [RetValue :: term()] when
      Input :: {module(), Function :: atom(), Args :: [term()]}.
exec_order_by_input(Inputs) ->
    Self = self(),
    _ = [spawn_link(fun() ->
                            Self ! {{Module, Fun, Args}, apply(Module, Fun, Args)}
                    end)
         || {Module, Fun, Args} <- Inputs],
    [receive {Input, RetValue} -> RetValue end || Input <- Inputs].

%% @doc Input要素の関数をプロセスを立てて実行し, 結果を入力順に返す. エラーが起きても停止しない.
-spec exec_order_preserved_by_input([Input]) -> [RetValue :: term() | {'EXIT', Signal :: term()}] when
      Input :: {module(), Function :: atom(), Args :: [term()]}.
exec_order_preserved_by_input(Inputs) ->
    Self = self(),
    process_flag(trap_exit, true),
    Indices = lists:seq(1, length(Inputs)),
    Pids = [spawn_link(fun() ->
                               Self ! {Index, {Module, Fun, Args}, apply(Module, Fun, Args)}
                       end)
            || {Index, {Module, Fun, Args}} <- lists:zip(Indices, Inputs)],
    [receive
         {Index, Input, Result} ->
             receive %% wait normal exit
                 {'EXIT', Pid, normal} -> Result
             end;
         {'EXIT', Pid, Signal} -> {'EXIT', Signal}
     end || {Index, {Pid, Input}} <- lists:zip(Indices, lists:zip(Pids, Inputs))].
