%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

-module(moyo_concurrent_tests).

-include_lib("../include/eunit.hrl").

-on_load(init/0).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

exec_test_() ->
    {spawn,
     [
      {"timeoutを指定できる",
       fun() ->
               ?assertError(timeout, moyo_concurrent:exec([{timer, sleep, [infinity]}], 1))
       end},
      {"複数実行",
       fun() ->
               %% Input, Expected
               TestData = [{{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[3,4,5]]}, [5,4,3]},
                           {{lists, member,  [1, [1,2,3]]}, true}
                          ],
               Assoc  = moyo_concurrent:exec([Input || {Input, _} <- TestData]),
               %% 結果が求める値になっているかのチェック
               [?assertEqual(Expected, moyo_assoc:fetch(Input, Assoc))
                || {Input, Expected} <- TestData]
       end},
      {"errorになるものがある場合",
       fun() ->
               Inputs = [{lists, reverse, [[1,2,3]]},
                         {erlang, error, [hoge]}],
               ?assertError(hoge, moyo_concurrent:exec(Inputs))
       end},
      {"throwになるものがある場合",
       fun() ->
               Inputs = [{lists, reverse, [[1,2,3]]},
                         {erlang, throw, [hoge]}],
               ?assertError({nocatch, hoge}, moyo_concurrent:exec(Inputs))
       end},
      {"exitになるものがある場合",
       fun() ->
               Inputs = [{lists, reverse, [[1,2,3]]},
                         {erlang, exit, [hoge]}],
               ?assertError(hoge, moyo_concurrent:exec(Inputs))
       end}
     ]}.

exec_sort_test_() ->
    {spawn,
     [
      {"timeoutを指定できる",
       fun() ->
               ?assertError(timeout, moyo_concurrent:exec_sort([{timer, sleep, [infinity]}], 1))
       end},
      {"複数実行",
       fun() ->
               %% Input, Expected
               TestData = [{{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[3,4,5]]}, [5,4,3]},
                           {{lists, member,  [1, [1,2,3]]}, true}
                          ],
               %% 結果の一致
               ExpectedList = [Expected || {_, Expected} <- TestData],
               ?assertEqual(ExpectedList, moyo_concurrent:exec_sort([Input || {Input, _} <- TestData]))
       end},
      {"errorになるものがある場合",
       fun() ->
               Inputs = [{lists, reverse, [[1,2,3]]},
                         {erlang, error, [hoge]}],
               ?assertError(hoge, moyo_concurrent:exec_sort(Inputs))
       end},
      {"throwになるものがある場合",
       fun() ->
               Inputs = [{lists, reverse, [[1,2,3]]},
                         {erlang, throw, [hoge]}],
               ?assertError({nocatch, hoge}, moyo_concurrent:exec_sort(Inputs))
       end},
      {"exitになるものがある場合",
       fun() ->
               Inputs = [{lists, reverse, [[1,2,3]]},
                         {erlang, exit, [hoge]}],
               ?assertError(hoge, moyo_concurrent:exec_sort(Inputs))
       end}
     ]}.

exec_map_test_() ->
    {spawn,
     [
      {"timeoutを指定できる",
       fun() ->
               ?assertError(timeout, moyo_concurrent:exec([{timer, sleep, [infinity]}], 1))
       end},
      {"複数実行",
       fun() ->
               %% Input, Expected
               TestData = [{{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[3,4,5]]}, [5,4,3]},
                           {{lists, member,  [1, [1,2,3]]}, true}
                          ],
               Actual  = moyo_concurrent:exec_map([Input || {Input, _} <- TestData]),
               ?assertEqual(Actual, [E || {_, E} <- TestData])
       end},
      {"Input が同じ場合",
       fun() ->
               %% Input, Expected
               TestData = [{{lists, reverse, [[a,b,c]]}, [c,b,a]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[1,2,3]]}, [3,2,1]},
                           {{lists, reverse, [[x,y,z]]}, [z,y,x]}],
               Actual = moyo_concurrent:exec_map([Input || {Input, _} <- TestData]),
               ?assertEqual(Actual, [E || {_, E} <- TestData])
       end},
      {"error, throw, exit になるものがある場合",
       fun() ->
               Input = [{lists, reverse, [[1,2,3]]},
                        {lists, reverse, [[3,4,5]]},
                        {erlang, error, [hoge]},
                        {erlang, throw, [fuga]},
                        {erlang, exit, [piyo]}],
               Result = moyo_concurrent:exec_map(Input),
               %% order sensitive
               ?assertEqual([3, 2, 1], lists:nth(1, Result)),
               ?assertEqual([5, 4, 3], lists:nth(2, Result)),
               ?assertMatch2({'EXIT', {hoge, _}}, lists:nth(3, Result)),
               ?assertMatch2({'EXIT', {{nocatch, fuga}, _}}, lists:nth(4, Result)),
               ?assertMatch2({'EXIT', piyo}, lists:nth(5, Result))
       end},
      {"signal が突き抜けない",
       fun() ->
               Input = [{erlang, apply, [fun() ->
                                                 spawn_link(fun() -> exit(self(), kill) end),
                                                 ok
                                         end, []]}],
               Result = moyo_concurrent:exec_map(Input),
               ?assertMatch2([ok], Result)
       end}
     ]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = error_logger:tty(false), % 実行時のノイズとなるので、ログ出力は抑制する
    ok.
