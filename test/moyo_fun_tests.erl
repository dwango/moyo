%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_fun_tests).
-export([apply_on_exit_callback/1]).

-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").

apply_on_exit_test_() ->
    {foreach,
     fun() ->
             ok = meck:new(moyo_fun, [no_link, passthrough])
     end,
     fun(_) ->
             _ = meck:unload(moyo_fun)
     end,
    [
     {
       "引数にPidが1つの場合",
      fun() ->
              Pid = spawn(erlang, tuple_size, [{1}]),
              {CallbackPid,Ref} = spawn_monitor(timer, sleep, [1000]),
              ExecutorPid = moyo_fun:apply_on_exit([Pid], ?MODULE, apply_on_exit_callback, [CallbackPid]),
              ?assertDown(Ref, _),
              ?ensureExited(ExecutorPid, shutdown),
              ?assertEqual(1, meck:num_calls(moyo_fun, try_apply, '_'))
      end},
     {"引数にPidが複数の場合",
      fun() ->
              Pids = [spawn(erlang, tuple_size, [{1}]) || _ <- lists:seq(1,10)],
              {CallbackPid, Ref} = spawn_monitor(timer, sleep, [1000]),
              ExecutorPid = moyo_fun:apply_on_exit(Pids, ?MODULE, apply_on_exit_callback, [CallbackPid]),
              ?assertDown(Ref, _),
              ?ensureExited(ExecutorPid, shutdown),
              ?assertEqual(1, meck:num_calls(moyo_fun, try_apply, '_'))
      end},
     {"外から殺してきちんと動作するか",
      fun() ->
              Pid = spawn(time, sleep, [infinity]),
              {CallbackPid, Ref} = spawn_monitor(timer, sleep, [1000]),
              ExecutorPid = moyo_fun:apply_on_exit([Pid], ?MODULE, apply_on_exit_callback, [CallbackPid]),
              ?assertEqual(true, is_process_alive(Pid)),
              exit(Pid, kill),
              ?assertDown(Ref, _),
              ?ensureExited(ExecutorPid, shutdown),
              ?assertEqual(1, meck:num_calls(moyo_fun, try_apply, '_'))
      end},
     {"Executorに変なメッセージを送っても反応して指定した関数が実行されないか",
      fun() ->
              Pid = spawn(timer, sleep, [infinity]),
              CallbackPid = spawn(timer, sleep, [infinity]),
              ExecutorPid = moyo_fun:apply_on_exit([Pid], ?MODULE, apply_on_exit_callback, [CallbackPid]),
              ExecutorPid ! badarg, % これでは反応しない
              ?ensureExited(CallbackPid, shutdown),
              ?ensureExited(Pid, shutdown),
              ?ensureExited(ExecutorPid, shutdown),
              ?assertEqual(0, meck:num_calls(moyo_fun, try_apply, '_'))
      end},
     {"Executorを殺したら実行されなくなるか",
      fun() ->
              Pid = spawn(timer, sleep, [infinity]),
              CallbackPid = spawn(timer, sleep, [infinity]),
              ExecutorPid = moyo_fun:apply_on_exit([Pid], ?MODULE, apply_on_exit_callback, [CallbackPid]),
              ?ensureExited(ExecutorPid, shutdown),
              ?assertEqual(true, is_process_alive(CallbackPid)),
              ?ensureExited(Pid, shutdown),
              ?ensureExited(CallbackPid, shutdown)
      end},
     {"Pidの指定がリストではなくPid単体",
      fun() ->
              Pid = spawn(erlang, tuple_size, [{1}]),
              ?assertError(badarg, moyo_fun:apply_on_exit(Pid, ?MODULE, apply_on_exit_callback, [callback_test])),
              ?ensureExited(Pid, shutdown)
      end},
     {"Pidの指定がリストでもないしPidでもない",
      fun() ->
              ?assertError(badarg, moyo_fun:apply_on_exit(badarg, ?MODULE, apply_on_exit_callback, [callback_test]))
      end}
    ]}.

%% 引数のPidのプロセスを殺す. moyo_fun:apply_on_exitで指定して呼び出す.
apply_on_exit_callback(Pid) ->
    %% moyo_fun:apply_on_exit_reciever/4の呼び出し回数で確認しようとしたがmeckのhistoryに追加されなかった(正常)
    %% なのでmoyo:fun:try_apply/3をここで呼び出した回数をテストでチェックするようにした
    %% moyo_fun:try_apply/3に依存することの影響を懸念したが同じモジュールなので構わないと判断した
    moyo_fun:try_apply(erlang, tuple_size, [{1}]),
    exit(Pid, reason).

try_apply_test_() ->
    [
     {"例外が発生しない関数の実行",
      fun () ->
              ?assertEqual(3, moyo_fun:try_apply(erlang, tuple_size, [{1,2,3}]))
      end},
     {"例外が発生する関数の実行",
      fun () ->
              ?assertMatch({error, {'EXIT', {error, badarg, _}}}, moyo_fun:try_apply(erlang, tuple_size, [1])) % 引数の型が不正
      end},
     {"例外が発生した場合の返り値を指定する",
      fun () ->
              ?assertMatch(something_wrong, moyo_fun:try_apply(erlang, tuple_size, [1], something_wrong)) % 引数の型が不正
      end}
    ].

try_call_test_() ->
    [
     {"例外が発生しない関数の実行",
      fun () ->
              ?assertEqual(3, moyo_fun:try_call(fun () -> erlang:tuple_size({1,2,3}) end))
      end},
     {"例外が発生する関数の実行",
      fun () ->
              ?assertMatch({error, {'EXIT', {error, function_clause, _}}}, moyo_fun:try_call(fun () -> moyo_list:shuffle(1) end)) % 引数の型が不正
      end},
     {"例外が発生した場合の返り値を指定する",
      fun () ->
              ?assertMatch(something_wrong, moyo_fun:try_call(fun () -> moyo_list:shuffle(1) end, something_wrong)) % 引数の型が不正
      end}
    ].

repeat_test_() ->
    [
     {"0から9までの和を求めるテスト",
      fun () ->
	      ?assertEqual(45, moyo_fun:repeat(fun (Index, Sum) -> Index + Sum end, 0, 10))
      end}
    ].

map_range_test_() ->
    [
     {"1から9までの平方を列挙するテスト(Start<End)",
      fun () ->
          ?assertEqual([1, 4, 9, 16, 25, 36, 49, 64, 81], moyo_fun:map_range(fun(X) -> X*X end, 1, 9))
      end},
     {"10の平方だけを求めるテスト(Start==End)",
      fun() ->
          ?assertEqual([100], moyo_fun:map_range(fun(X) -> X*X end, 10, 10))
      end},
     {"不正な範囲を与えるテスト(Start>End)",
      fun() ->
          ?assertEqual([], moyo_fun:map_range(fun(X) -> X*X end, 1, -1))
      end}
    ].

fold_range_test_() ->
    [
     {"1から4までの和を求めるテスト(Start<End)",
      fun () ->
          ?assertEqual(10, moyo_fun:fold_range(fun(A, B) -> A+B end, 0, 1, 4))
      end},
     {"初期値10に1だけを足すテスト(Start==End)",
      fun () ->
          ?assertEqual(11, moyo_fun:fold_range(fun(A, B) -> A+B end, 10, 1, 1))
      end},
     {"不正な範囲を与えるテスト(Start>End)",
      fun () ->
          ?assertEqual(3, moyo_fun:fold_range(fun(A, B) -> A+B end, 3, 1, -1))
      end}
    ].

maybe_fold_range_test_() ->
    [
     {"Start>Endの場合",
      fun() ->
              AccIn = input,
              ?assertEqual({ok, AccIn}, moyo_fun:maybe_fold_range(fun(_, _) -> {error, error} end, AccIn, 3, 1))
      end},
     {"errorで抜ける",
      fun() ->
              ?assertEqual({error, lists:sum(lists:seq(1, 4))},
                           moyo_fun:maybe_fold_range(fun(Index, Acc) when Index < 5 -> {ok, Index + Acc};
                                                        (_,     Acc) -> {error, Acc}
                                                     end, 0, 1, 10))
      end},
     {"最後まで実行される場合",
      fun() ->
              ?assertEqual({ok, lists:sum(lists:seq(1, 10))},
                           moyo_fun:maybe_fold_range(fun(Index, Acc) -> {ok, Index + Acc} end, 0, 1, 10))
      end}
    ].

composite_test_() ->
    [
     {"関数リストの要素数がゼロで第2引数が指定されない場合はokを返す",
      fun() ->
              ?assertEqual(ok, moyo_fun:composite_apply([]))
      end},
     {"関数リストの要素数がゼロで第2引数が指定される場合は{ok, 引数}を返す",
      fun() ->
              ?assertEqual({ok, data}, moyo_fun:composite_apply([], data))
      end},
     {"関数リストの先頭の関数が引数をとらない場合も実行できる",
      fun() ->
              Fun1 = fun() -> {ok, data} end,
              Fun2 = fun(data) -> {ok, result} end,
              ?assertEqual({ok, result}, moyo_fun:composite_apply([Fun1, Fun2]))
      end},
     {"関数リストの先頭の関数が引数をとる場合も実行できる",
      fun() ->
              Fun = fun(X) -> {ok, X} end,
              ?assertEqual({ok, result}, moyo_fun:composite_apply([Fun, Fun], result))
      end},
     {"関数リスト中(先頭以外)に引数をとらない関数が混ざる場合も実行できる",
      fun() ->
              Fun1 = fun(_X) -> ok end,
              Fun2 = fun() -> ok end,
              Fun3 = fun() -> {ok, data} end,
              ?assertEqual({ok, data}, moyo_fun:composite_apply([Fun1, Fun2, Fun3], data))
      end},
     {"関数リストの実行途中にerrorが発生した場合，そこで演算を終了させる",
      fun() ->
              Fun1  = fun(_X) -> ok end,
              Fun2  = fun() -> {ok, data} end,
              FunE1 = fun() -> error end,
              FunE2 = fun() -> {error, message} end,
              ?assertEqual(error, moyo_fun:composite_apply([Fun1, FunE1, Fun2], data)),
              ?assertEqual({error, message}, moyo_fun:composite_apply([Fun1, FunE2, Fun2], data))
      end},
     {"関数リスト中のある関数の返り値の個数と次に実行される関数のarityが異なる場合，"
      "bad_arityエラーで例外を発生する",
      fun() ->
              Fun1 = fun(X) -> {ok, X} end,
              Fun2 = fun() -> {ok, result} end,
              ?assertError(bad_arity, moyo_fun:composite_apply([Fun1, Fun2], data)),
              ?assertError(bad_arity, moyo_fun:composite_apply([Fun1, Fun2]))
      end},
     {"タプルを用いて複数の引数を与えることができる",
      fun() ->
              Fun1 = fun(X) -> {ok, {X, X}} end,
              Fun2 = fun({X,Y}) -> {ok, {X, Y}} end,
              ?assertEqual({ok, {data, data}}, moyo_fun:composite_apply([Fun1, Fun2], data))
      end}
    ].
