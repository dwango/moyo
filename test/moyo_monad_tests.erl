%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_monad_tests).

-include_lib("eunit/include/eunit.hrl").

maybe_constructor_test_() ->
    [
     {"maybe, maybe_funを作る",
      fun () ->
              ?assertEqual(nothing, moyo_monad:maybe(nothing)),
              ?assertEqual({just, 123}, moyo_monad:maybe(123)),
              ?assertEqual(nothing, moyo_monad:maybe_fun(nothing)),
              SomeFun = fun(A, B)->A+B end,
              ?assertEqual({just, SomeFun}, moyo_monad:maybe_fun(SomeFun)),
              ?assertError({invalid_function, _}, moyo_monad:maybe_fun(123))
      end}
    ].

apply_maybe_test_() ->
    [
     {"nothingを適用 -> default valueが返ってくる",
      fun () ->
              ?assertEqual(defaultValue, moyo_monad:apply_maybe(nothing, [1,2], defaultValue))
      end},
     {"関数を適用 -> 適用結果が返ってくる",
      fun () ->
              Add = fun(A, B) -> A+B end,
              ?assertEqual(3, moyo_monad:apply_maybe({just, Add}, [1,2], defaultValue))
      end},
     {"関数じゃないものを適用 -> error",
      fun () ->
              ?assertError({invalid_function, _}, moyo_monad:apply_maybe({just, 777}, [1,2], defaultValue))
      end},
     {"引数の個数が異なる関数を適用 -> error",
      fun () ->
              SomeFun = fun(A, B, C) -> A+B*C end,
              ?assertError({invalid_function, _}, moyo_monad:apply_maybe({just, SomeFun}, [1,2], defaultValue))
      end},
     {"{just,X}でもnothingでもないものを適用 -> error",
      fun () ->
              ?assertError({invalid_maybe, _}, moyo_monad:apply_maybe(888, [1,2], defaultValue))
      end}
    ].
