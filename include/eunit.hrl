%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc eunit用の追加マクロ
%%
%% このファイルをincludeすれば`eunit/include/eunit.hrl'でdefineされているマクロと合わせて使用ができる.

-ifndef(MOYO_EUNIT_HRL).
-define(MOYO_EUNIT_HRL, true).

-include_lib("eunit/include/eunit.hrl").

-ifdef(NOASSERT).
-define(assertMatch2(Guard, Expr), ok).
-else.
-define(assertMatch2(Guard, Expr),
        (fun() ->
                 try
                     Guard = Expr,
                     ok
                 catch
                     error:{badmatch, __MOYO_EUNIT_V} ->
                         erlang:error({assertMatch_failed,
                                       [{module, ?MODULE},
                                        {line, ?LINE},
                                        {expression, (??Expr)},
                                        {pattern, (??Guard)},
                                        {value, (__MOYO_EUNIT_V)}
                                       ]})
                 end
         end)()).
-endif.
-define(_assertMatch2(Guard, Expr), ?_test(?assertMatch2(Guard, Expr))).
%% ?assertMatch(_, Value) は書くことができない. <br />
%% assertマクロを書く際にこれがあると便利なので定義している <br />
%% 基本的な動作はassertMatchと同じ.

-ifdef(NOASSERT).
-define(assignMatch(Guard, Expr), Guard = Expr).
-else.
-define(assignMatch(Guard, Expr),
        begin
            Guard = (fun() ->
                             __MOYO_EUNIT_TMP = Expr,
                             ?assertMatch2(Guard, __MOYO_EUNIT_TMP),
                             __MOYO_EUNIT_TMP
                     end)()
        end).
-endif.
-define(_assignMatch(Guard, Expr), ?_test(?assignMatch(Guard, Expr))).
%% 以下の構文と同義のことが実現できる
%% ```
%% ?assertMatch({ok, _}, Ret),
%% {ok, Pid} = Ret
%% '''

-ifdef(NOASSERT).
-define(assertTerminated(Pid, Reason), ok).
-else.
-define(assertTerminated(Pid, Reason),
        (fun() ->
                 receive
                     {'EXIT', Pid, __MOYO_EUNIT_RetReason} ->
                         ?assertMatch2(Reason, __MOYO_EUNIT_RetReason)
                 after 50 ->
                         ?assert(timeout)
                 end
         end)()).
-endif.
-define(_assertTerminated(Pid, Reason), ?_test(?assertTerminated(Pid, Reason))).
%% プロセスから'EXIT'が一定時間以内に投げられることを確認する.
%%
%% ```
%% _   = process_flag(trap_exit, true),
%% Pid = spawn_link(fun hoge/0),
%% ?assertTerminated(Pid, _)
%% '''

-ifdef(NOASSERT).
-define(assertDown(Ref, Reason), ok).
-else.
-define(assertDown(Ref, Reason),
        (fun() ->
                 receive
                     {'DOWN', Ref, process, _, __MOYO_EUNIT_RetReason} ->
                         ?assertMatch2(Reason, __MOYO_EUNIT_RetReason)
                 after 50 ->
                         ?assert(timeout)
                 end
         end)()).
-endif.
-define(_assertDown(Ref, Reason), ?_test(?assertDown(Ref, Reason))).
%% 'DOWN'が一定時間以内に投げられることを確認する.
%%
%% ```
%% {_, Ref} = spawn_monitor(fun hoge/0),
%% ?assertDown(Ref, _)
%% '''

-ifdef(NOASSERT).
-define(ensureExited(Pid, Reason), ok).
-else.
-define(ensureExited(Pid, Reason),
        (fun() ->
                 __MOYO_EUNIT_Pid = Pid,
                 __MOYO_EUNIT_Old = process_flag(trap_exit, true),
                 __MOYO_EUNIT_Ref = monitor(process, __MOYO_EUNIT_Pid),
                 exit(__MOYO_EUNIT_Pid, Reason),
                 ?assertDown(__MOYO_EUNIT_Ref, _),
                 process_flag(trap_exit, __MOYO_EUNIT_Old)
         end)()).
-endif.
-define(_ensureExited(Pid, Reason), ?_test(?ensureExited(Pid, Reason))).
%% プロセスが生きていたら`Reason'という理由で終了させる.(`exit/2'を発行する)
%% 一定時間以内に終了できなかった場合はtest failureになる.
%%
%% ```
%% Pid = spwan(fun hoge/0),
%% ?ensureExited(Pid)
%% '''

-define(ensureExited(Pid), ?ensureExited(Pid, kill)).
-define(_ensureExited(Pid), ?_test(?ensureExited(Pid))).
%% @equiv ?ensureExited(Pid, kill)

-ifdef(NOASSERT).
-define(assertLinked(Pid1, Pid2), ok).
-else.
-define(assertLinked(Pid1, Pid2),
        (fun () ->
                 {_, __Links} = erlang:process_info(Pid2, links),
                 ?assert(lists:member(Pid1, __Links))
         end)()).
-endif.
-define(_assertLinked(Pid1, Pid2), ?_test(?assertLinked(Pid1, Pid2))).
%% 二つのプロセス(`Pid1'と`Pid2')の間にリンクが貼られているかを確認する.
%%
%% ```
%% Pid = spawn_link(fun hoge/0),
%% ?assertLinked(self(), Pid).
%% '''

-define(assertLinked(Pid), ?assertLinked(self(), Pid)).
-define(_assertLinked(Pid), ?_test(?assertLinked(Pid))).
%% @equiv ?assertLinked(self(), Pid)

-endif. % ifndef MOYO_EUNIT_HRL
