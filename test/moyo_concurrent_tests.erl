%% coding: latin-1
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

-module(moyo_concurrent_tests).

-include_lib("eunit/include/eunit.hrl").

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
