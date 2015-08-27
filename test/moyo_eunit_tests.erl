%% coding: latin-1
%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_eunit_tests).

-include("eunit.hrl").
%% 従来であれば
%% -include_lib("moyo/include/eunit.hrl").

moyo_eunit_test_() ->
    {foreach,
     fun()    -> process_flag(trap_exit, false) end,
     fun(Old) -> process_flag(trap_exit, Old) end,
     [
      {"assertMatch2が正しく動作する",
       fun() ->
               ?assertMatch2(_, hoge),
               ?assertMatch2(hoge, hoge),
               ?assertError({assertMatch_failed, _}, ?assertMatch2([], lists:seq(1,3)))
       end},
      {"assertMatch2の引数に関数を入れた時、2回実行されない",
       fun() ->
               Fun = fun() -> self() ! ok end,
               ?assertMatch2(ok, Fun()),

               ReceiveCounter = fun Counter(Count) -> receive ok -> Counter(Count + 1) after 0 -> Count end end,
               ?assertEqual(1, ReceiveCounter(0))
       end},
      {"assertTerminatedが正常に実行できる",
       fun() ->
               _ = process_flag(trap_exit, true),
               Pid = spawn_link(fun() -> exit(abort) end),
               ?assertTerminated(Pid, abort)
       end},
      {"assertTerminatedが2回使用できる",
       fun() ->
               _ = process_flag(trap_exit, true),

               Pid = spawn_link(fun() -> exit(abort) end),
               ?assertTerminated(Pid, abort),

               Pid2 = spawn_link(fun() -> exit(abort) end),
               ?assertTerminated(Pid2, abort)
       end},
      {"assertDownが正常に実行できる",
       fun() ->
               {Pid, Ref} = spawn_monitor(fun() -> exit(abort) end),
               ?assertDown(Ref, abort),
               ?assertNot(is_process_alive(Pid))
       end},
      {"assertDownが2回使用できる",
       fun() ->
               {Pid, Ref} = spawn_monitor(fun() -> exit(abort) end),
               ?assertDown(Ref, abort),
               ?assertNot(is_process_alive(Pid)),

               {Pid2, Ref2} = spawn_monitor(fun() -> exit(abort) end),
               ?assertDown(Ref2, abort),
               ?assertNot(is_process_alive(Pid2))
       end},
      {"ensureExitedが正常に実行できる",
       fun() ->
               Pid = spawn(fun() -> timer:sleep(infinity) end),
               ?ensureExited(Pid),
               ?assertNot(is_process_alive(Pid))
       end},
      {"ensureExitedでPidに関数を入れても2回実行されない",
       fun() ->
               ?ensureExited(spawn_link(fun() -> ?assertEqual(true, register(moyo_eunit_ensure_exited_test, self())), timer:sleep(infinity) end))
       end},
      {"プロセスの終了理由が指定できる",
       fun() ->
               Pid = spawn(fun() -> timer:sleep(infinity) end),
               Monitor = monitor(process, Pid),
               ?ensureExited(Pid, hoge),
               receive
                   {'DOWN', Monitor, _, _, Reason} -> ?assertEqual(hoge, Reason)
               after 0 -> ?assert(timeout)
               end
       end},
      {"ensureExitedはlinkしていても親プロセスが落ちない",
       fun() ->
               Pid = spawn_link(fun() -> timer:sleep(infinity) end),
               ?ensureExited(Pid),
               ?assertNot(is_process_alive(Pid))
       end},
      {"ensureExitedが2回使用できる",
       fun() ->
               Pid  = spawn(fun() -> timer:sleep(infinity) end),
               ?ensureExited(Pid),
               ?assertNot(is_process_alive(Pid)),

               Pid2 = spawn(fun() -> timer:sleep(infinity) end),
               ?ensureExited(Pid2),
               ?assertNot(is_process_alive(Pid2))
       end},
      {"assignMatchが正常に利用できる",
       fun() ->
               L = lists:seq(1, 5),
               ?assignMatch([H | _], L),
               ?assertEqual(H, 1),

               L2 = lists:seq(1, 0),
               ?assertError({assertMatch_failed, _}, ?assignMatch([_ | _], L2))
       end},
      {"assignMatchを使ったリテラル同士のマッチ",
       fun () ->
               ?assignMatch(1, 1),
               ?assignMatch(a, a),
               ?assignMatch(<<"a">>, <<"a">>),
               ?assignMatch([a], [a]),
               ?assignMatch({a}, {a})
       end},
      {"assignMatchが部分が単なる代入でなくても利用できる",
       fun() ->
               [1 | T] = lists:seq(1,5),
               ?assignMatch([1 | T], lists:seq(1,5)),
               ?assignMatch([1 | T2], lists:seq(1,5)),
               ?assertEqual(T, T2)
       end},
      {"assignMatchの引数に関数を入れた時、2回実行されない",
       fun() ->
               Fun = fun() -> self() ! ok end,
               ?assignMatch(ok, Fun()),

               ReceiveCounter = fun Counter(Count) -> receive ok -> Counter(Count + 1) after 0 -> Count end end,
               ?assertEqual(1, ReceiveCounter(0))
       end},
      {"assignMatchで変数が既に束縛されているときも利用できる",
       fun() ->
               H = 1,
               L = lists:seq(1, 5),
               ?assignMatch([H | _], L)
       end},
      {"自プロセスと指定のプロセスの間にリンクが貼られているかどうかを確認できる",
       fun () ->
               Pid = spawn(timer, sleep, [infinity]),
               ?assertError(_, ?assertLinked(Pid)), % not-linked,

               true = link(Pid),
               ?assertLinked(Pid) % linked
       end},
      {"指定のプロセス間にリンクが貼られているかどうかを確認できる",
       fun () ->
               Parent = self(),

               Pid1 = spawn(timer, sleep, [infinity]),
               Pid2 = spawn(timer, sleep, [infinity]),
               ?assertError(_, ?assertLinked(Pid1, Pid2)), % not-linked,

               Pid3 = spawn(fun () ->
                                    true = link(Pid1),
                                    Parent ! linked,
                                    timer:sleep(infinity)
                            end),
               receive linked -> ok end,
               ?assertLinked(Pid1, Pid3) % linked
       end}
     ]}.
