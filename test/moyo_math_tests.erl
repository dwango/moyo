%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_mathモジュールのユニットテスト.
-module(moyo_math_tests).

-include_lib("eunit/include/eunit.hrl").

ceil_test_() ->
    [
     {"1.0を切り上げ",
      ?_assertEqual(1, moyo_math:ceil(1.0))},
     {"0.5を切り上げ",
      ?_assertEqual(1, moyo_math:ceil(0.5))},
     {"0.0を切り上げ",
      ?_assertEqual(0, moyo_math:ceil(0.0))},
     {"-0.5を切り上げ",
      ?_assertEqual(0, moyo_math:ceil(-0.5))},
     {"-1.0を切り上げ",
      ?_assertEqual(-1, moyo_math:ceil(-1.0))},
     {"整数をを切り上げ",
      ?_assertEqual(-2, moyo_math:ceil(-2))}
    ].

floor_test_() ->
    [
     {"1.0を切り下げ",
      ?_assertEqual(1, moyo_math:floor(1.0))},
     {"0.5を切り下げ",
      ?_assertEqual(0, moyo_math:floor(0.5))},
     {"0.0を切り下げ",
      ?_assertEqual(0, moyo_math:floor(0.0))},
     {"-0.5を切り下げ",
      ?_assertEqual(-1, moyo_math:floor(-0.5))},
     {"-1.0を切り下げ",
      ?_assertEqual(-1, moyo_math:floor(-1.0))},
     {"整数をを切り下げ",
      ?_assertEqual(-2, moyo_math:floor(-2))}
    ].

gcm_test_() ->
    [
     {"最大公約数を求める",
      fun () ->
              Result = moyo_math:gcd(366, 12),
              ?assertEqual(6, Result)
      end},

     {"約数を求める時片方が0",
      fun () ->
              ?assertEqual(1234, moyo_math:gcd(0, 1234)),
              ?assertEqual(5678, moyo_math:gcd(5678, 0))
      end},

     {"約数を求める時両方が0",
      fun () -> ?assertThrow(both_0_error, moyo_math:gcd(0, 0)) end},

     {"約数を求める元の数が負",
      fun () ->
              %% 第一引数が負
              Result1 = moyo_math:gcd(-12, 9),
              ?assertEqual(3, Result1),

              %% 第二引数が負
              Result2 = moyo_math:gcd(45, -36),
              ?assertEqual(9, Result2),

              %% 第三引数が負
              Result2 = moyo_math:gcd(45, -36),
              Result3 = moyo_math:gcd(-72, -56),
              ?assertEqual(8, Result3)
      end}
    ].

pow_int_test_() ->
    [
     {"aのn乗を返す",
      fun () ->
                %% nは整数
                ?assertEqual(1, moyo_math:pow_int(2, 0)),
                ?assertEqual(2, moyo_math:pow_int(2, 1)),
                ?assertEqual(4, moyo_math:pow_int(2, 2)),
                ?assertEqual(8, moyo_math:pow_int(2, 3)),
                ?assertEqual(16, moyo_math:pow_int(2, 4)),
                ?assertEqual(32, moyo_math:pow_int(2, 5)),
                ?assertEqual(3, moyo_math:pow_int(3, 1)),
                ?assertEqual(9, moyo_math:pow_int(3, 2)),
                ?assertEqual(78125, moyo_math:pow_int(5, 7)),
                ?assertEqual(96889010407, moyo_math:pow_int(7, 13)),
                ?assertEqual(3211838877954855105157369, moyo_math:pow_int(13, 22)),

                %% nは負数
                ?assertError(function_clause, moyo_math:pow_int(3, -1)),

                %% nはfloat型
                ?assertError(function_clause, moyo_math:pow_int(3, 0.5))
      end}
    ].

div_test_() ->
    [
     {"整数の除算を行い商と剰余を求める",
      fun () ->
              ?assertEqual({1, 0}, moyo_math:divmod(10, 10)),
              ?assertEqual({3, 1}, moyo_math:divmod(10, 3)),
              ?assertEqual({12499999887, 339506163}, moyo_math:divmod(12345678901234567890, 987654321)),
              ?assertEqual({-3, -1}, moyo_math:divmod(-10, 3)),
              ?assertEqual({-3, 1}, moyo_math:divmod(10, -3)),
              ?assertEqual({3, -1}, moyo_math:divmod(-10, -3)),
              ?assertEqual({0, 0}, moyo_math:divmod(0, 3))
      end},
     {"0除算を行うと例外が投げられる",
      fun () ->
              ?assertError(badarith, moyo_math:divmod(10, 0)),
              ?assertError(badarith, moyo_math:divmod(0, 0))
      end}
    ].

random_sequence_test_() ->
    [
     {"指定したタイプ毎にランダムの文字列を返し、毎回異なる",
      fun () ->
              rand:seed(exs64, {0, 0, 0}),
              ?assertEqual(<<"MHfDhiSQxxFVWKSBCoIr">>, moyo_math:random_sequence(20, [{symbol, alphabetical}])),
              ?assertEqual(<<"PsSmPLDJexwnbeqvSMzR">>, moyo_math:random_sequence(20, [{symbol, alphabetical}])),
              ?assertEqual(<<"15767488576692587821">>, moyo_math:random_sequence(20, [{symbol, numeric}])),
              ?assertEqual(<<"97958138928329877399">>, moyo_math:random_sequence(20, [{symbol, numeric}])),
              ?assertEqual(<<"DZmObX6ugXOwATd87YzG">>, moyo_math:random_sequence(20, [{symbol, alphanumeric}])),
              ?assertEqual(<<"cZGW3oKb4jzY6JANkWAE">>, moyo_math:random_sequence(20, [{symbol, alphanumeric}]))
      end},
     {"指定した長さで指定した文字しか存在しない文字列を返す",
      fun () ->
              Length = 1000,
              ?assertMatch({_,[{0, Length}]}, re:run(moyo_math:random_sequence(Length, [{symbol, alphabetical}]), "[a-zA-Z]+")),
              ?assertMatch({_,[{0, Length}]}, re:run(moyo_math:random_sequence(Length, [{symbol, numeric}]), "[0-9]+")),
              ?assertMatch({_,[{0, Length}]}, re:run(moyo_math:random_sequence(Length, [{symbol, alphanumeric}]), "[a-zA-Z0-9]+"))
      end},
     {"Symbolを省略し、デフォルト値alphabeticalで利用できる",
      fun () ->
              rand:seed(exs64, {0, 0, 0}),
              R1 = moyo_math:random_sequence(20, [{symbol, alphabetical}]),
              rand:seed(exs64, {0, 0, 0}),
              R2 = moyo_math:random_sequence(20),
              ?assertEqual(R1, R2)
      end}
    ].
