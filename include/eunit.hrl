%% @copyright 2013-2018 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc eunit用の追加マクロ
%%
%% このファイルをincludeすれば`eunit/include/eunit.hrl'でdefineされているマクロと合わせて使用ができる.
%%
%% timeoutは3000にしている. (eunitのtimeoutが5sなので, それ以下の適当な値)

-ifndef(MOYO_EUNIT_HRL).
-define(MOYO_EUNIT_HRL, true).

-include_lib("eunit/include/eunit.hrl").

-ifdef(NOASSERT).
-define(assertMatch2(Guard, Expr), ok).
-else.
-define(assertMatch2(Guard, Expr),
        (fun() ->
                 try
                     MOYO_ASSERT___MATCH2_TMP = Expr,

                     %% `Guard'内に変数が含まれる場合に、OTP18で"unused変数警告"が出力されるのを抑制するために、二度`Guard'を使っている。
                     %% (一度目で束縛された変数が、二度目は使われるので警告がでなくなる)
                     %%
                     %% なお`Guard = Guard = MOYO_ASSERT___MATCH2_TMP'のようにまとめて書くと、`Guard'にバイナリリテラルが含まれる場合に、
                     %% OTP18で別のコンパイラ警告が出てしまうので、別々に分けている。
                     Guard = MOYO_ASSERT___MATCH2_TMP,
                     Guard = MOYO_ASSERT___MATCH2_TMP,
                     ok
                 catch
                     error:{badmatch, MOYO_EUNIT___V} ->
                         erlang:error({assertMatch_failed,
                                       [{module, ?MODULE},
                                        {line, ?LINE},
                                        {expression, (??Expr)},
                                        {pattern, (??Guard)},
                                        {value, (MOYO_EUNIT___V)}
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
                             MOYO_EUNIT___TMP = Expr,
                             ?assertMatch2(Guard, MOYO_EUNIT___TMP),
                             MOYO_EUNIT___TMP
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
                     {'EXIT', Pid, MOYO_EUNIT___RetReason} ->
                         ?assertMatch2(Reason, MOYO_EUNIT___RetReason)
                 after 3000 ->
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
                     {'DOWN', Ref, process, _, MOYO_EUNIT___RetReason} ->
                         ?assertMatch2(Reason, MOYO_EUNIT___RetReason)
                 after 3000 ->
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
%%
-ifdef(NOASSERT).
-define(assertContain(Element, List), ok).
-else.
-define(assertContain(Element, List),
        (fun() ->
                 case is_list(List) andalso lists:member(Element, List) of
                     true ->
                         ok;
                     false ->
                         Expression = lists:flatten(io_lib:format("~p contains ~p", [List, Element])),
                         erlang:error({assert,
                                       [{module, ?MODULE},
                                        {line, ?LINE},
                                        {expression, Expression},
                                        {expected, true},
                                        {value, false}]})
                 end
         end)()).
-endif.
-define(_assertContain(Element, List), ?_test(?assertContain(Element, List))).
%% ListがElementを含むことを確認する
%%
%% ```
%% ?assertContain(1, [2,1,3])
%% '''

-ifdef(NOASSERT).
-define(ensureExited(Pid, Reason), ok).
-else.
-define(ensureExited(Pid, Reason),
        (fun() ->
                 MOYO_EUNIT___Pid = Pid,
                 MOYO_EUNIT___Old = process_flag(trap_exit, true),
                 MOYO_EUNIT___Ref = monitor(process, MOYO_EUNIT___Pid),
                 exit(MOYO_EUNIT___Pid, Reason),
                 ?assertDown(MOYO_EUNIT___Ref, _),
                 process_flag(trap_exit, MOYO_EUNIT___Old)
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
                 {_, MOYO__Links} = erlang:process_info(Pid2, links),
                 ?assert(lists:member(Pid1, MOYO__Links))
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
