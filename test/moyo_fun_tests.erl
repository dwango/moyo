%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_fun_tests).

-include_lib("eunit/include/eunit.hrl").

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
