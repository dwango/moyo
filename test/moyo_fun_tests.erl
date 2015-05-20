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


