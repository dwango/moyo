%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_fracのテストモジュール.
-module(moyo_frac_tests).

-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    [
     {"分数を生成する",
      fun () ->
              Frac = moyo_frac:new(3, 4),

              %% 複数の方法で検証
              ?assertEqual({3, 4},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(3,         moyo_frac:num(Frac)),
              ?assertEqual(4,         moyo_frac:denom(Frac)),
              ?assertEqual(<<"3/4">>, moyo_frac:to_binary(Frac))
      end},

     {"分子に負の整数を与えて分数を生成",
      fun () ->
              Frac = moyo_frac:new(-9, 2),

              %% 複数の方法で検証
              ?assertEqual({-9, 2},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(-9,         moyo_frac:num(Frac)),
              ?assertEqual(2,          moyo_frac:denom(Frac)),
              ?assertEqual(<<"-9/2">>, moyo_frac:to_binary(Frac))
      end},

     {"分母に負の整数を与えて分数を生成",
      fun () ->
              Frac = moyo_frac:new(4, -7),

              %% 複数の方法で検証
              ?assertEqual({-4, 7},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(-4,         moyo_frac:num(Frac)),
              ?assertEqual(7,          moyo_frac:denom(Frac)),
              ?assertEqual(<<"-4/7">>, moyo_frac:to_binary(Frac))
      end},

     {"分母分子に負の数を与えて分数を生成",
      fun () ->
              Frac = moyo_frac:new(-11, -5),

              %% 複数の方法で検証
              ?assertEqual({11, 5},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(11,         moyo_frac:num(Frac)),
              ?assertEqual(5,          moyo_frac:denom(Frac)),
              ?assertEqual(<<"11/5">>, moyo_frac:to_binary(Frac))
      end},

     {"分数生成時に約分が行われる",
      fun () ->
              Frac = moyo_frac:new(21, 123),

              %% 複数の方法で検証
              ?assertEqual({7, 41},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(7,           moyo_frac:num(Frac)),
              ?assertEqual(41,          moyo_frac:denom(Frac)),
              ?assertEqual(<<"7/41">>, moyo_frac:to_binary(Frac))
      end},

     {"符号の調整をした時に約分が行われることを確認",
      fun () ->
              Frac = moyo_frac:new(36, -42),

              %% 複数の方法で検証
              ?assertEqual({-6, 7},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(-6,         moyo_frac:num(Frac)),
              ?assertEqual(7,          moyo_frac:denom(Frac)),
              ?assertEqual(<<"-6/7">>, moyo_frac:to_binary(Frac))
      end},

     {"分数生成時, 分子に0が与えられた場合",
      fun () ->
              Frac = moyo_frac:new(0, 23),

              %% 複数の方法で検証
              ?assertEqual({0, 1},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(0,         moyo_frac:num(Frac)),
              ?assertEqual(1,         moyo_frac:denom(Frac)),
              ?assertEqual(<<"0/1">>, moyo_frac:to_binary(Frac))
      end},

     {"分子に0, 分母に負の整数で分数を生成",
      fun () ->
              Frac = moyo_frac:new(0, -45),

              %% 複数の方法で検証
              ?assertEqual({0, 1},    moyo_frac:to_tuple(Frac)),
              ?assertEqual(0,         moyo_frac:num(Frac)),
              ?assertEqual(1,         moyo_frac:denom(Frac)),
              ?assertEqual(<<"0/1">>, moyo_frac:to_binary(Frac))
      end},

     {"分母に0が渡されたときに例外が投げられる",
      fun () ->
              ?assertThrow(denominator_0_error, moyo_frac:new(1, 0))
      end}
    ].

from_integer_test_() ->
    [
     {"整数を分数に変換する",
      fun () -> ?assertEqual(moyo_frac:new(4, 1), moyo_frac:from_integer(4)) end},

     {"負の整数を分数に変換する",
      fun () -> ?assertEqual(moyo_frac:new(-5, 1), moyo_frac:from_integer(-5)) end}
    ].

from_float_test_() ->
    [
     {"小数を分数に変換する",
      ?_assertEqual(moyo_frac:new(7070651414971679, 2251799813685248), moyo_frac:from_float(3.14))},

     {"負の小数を分数に変換する",
      ?_assertEqual(moyo_frac:new(-382563899846521, 140737488355328), moyo_frac:from_float(-2.71828))},

     {"1.0を分数に変換する",
      ?_assertEqual(moyo_frac:new(1, 1), moyo_frac:from_float(1.0))},

     {"1より大きい最小の数(0x3ff0 0000 0000 0001)を分数に変換する",
      ?_assertEqual(moyo_frac:new(4503599627370497, 4503599627370496), moyo_frac:from_float(1.0000000000000002))},

     {"正の最小の非正規化数(0x0000 0000 0000 0001)を分数に変換する",
      fun () ->
              <<Float/float>> = <<0:63, 1:1>>,
              %% to_floatできないので, moyo_frac:new/2の値が本当にあっているのかは確かめていない.
              ?assertEqual(moyo_frac:new(4503599627370497, 404804506614621236704990693437834614099113299528284236713802716054860679135990693783920767402874248990374155728633623822779617474771586953734026799881477019843034848553132722728933815484186432682479535356945490137124014966849385397236206711298319112681620113024717539104666829230461005064372655017292012526615415482186989568), moyo_frac:from_float(Float))
      end},

     {"正の最小の正規化数(0x0010 0000 0000 0000)を分数に変換する",
      fun () ->
              <<Float/float>> = <<16#001:12, 0:52>>,
              ?assertEqual(moyo_frac:new(1, 44942328371557897693232629769725618340449424473557664318357520289433168951375240783177119330601884005280028469967848339414697442203604155623211857659868531094441973356216371319075554900311523529863270738021251442209537670585615720368478277635206809290837627671146574559986811484619929076208839082406056034304), moyo_frac:from_float(Float))
      end},

     {"負の最大の正規化数(0x8010 0000 0000 0000)を分数に変換する",
      fun () ->
              <<Float/float>> = <<1:1, 1:11, 0:52>>,
              ?assertEqual(moyo_frac:new(-1, 44942328371557897693232629769725618340449424473557664318357520289433168951375240783177119330601884005280028469967848339414697442203604155623211857659868531094441973356216371319075554900311523529863270738021251442209537670585615720368478277635206809290837627671146574559986811484619929076208839082406056034304), moyo_frac:from_float(Float))
      end},

     {"倍精度浮動小数点数の最大値(0x7fef ffff ffff ffff)を分数に変換する",
      fun () ->
              <<Float/float>> = <<16#7fe:12, 16#fffffffffffffff:52>>,
              ?assertEqual(moyo_frac:new(179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368, 1), moyo_frac:from_float(Float))
      end},

     {"倍精度浮動小数点数の最小値(0xffef ffff ffff ffff)を分数に変換する",
      fun () ->
              <<Float/float>> = <<16#ffe:12, 16#fffffffffffffff:52>>,
              ?assertEqual(moyo_frac:new(-179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368, 1), moyo_frac:from_float(Float))
      end},

     {"0.0を分数に変換する",
      ?_assertEqual(moyo_frac:new(0, 1), moyo_frac:from_float(0.0))}
    ].

num_test_() ->
    [
     {"分子を返す",
      fun () -> ?assertEqual(3, moyo_frac:num(moyo_frac:new(3, 5))) end},

     {"負の分数の場合, 分子を返すと負であることの確認",
      fun () -> ?assertEqual(-7, moyo_frac:num(moyo_frac:new(-7, 9))) end}
    ].

denom_test_() ->
    [
     {"分母を返す",
      fun () -> ?assertEqual(7, moyo_frac:denom(moyo_frac:new(8, 7))) end},

     {"分母を返す(作成時分母は負)",
      fun () -> ?assertEqual(2, moyo_frac:denom(moyo_frac:new(5, -2))) end}
    ].

to_tuple_test_() ->
    [
     {"分数をタプルに変換",
      fun () -> ?assertEqual({4, 5}, moyo_frac:to_tuple(moyo_frac:new(4, 5))) end},

     {"分母が負の数の分数をタプルに変換",
      fun () -> ?assertEqual({-2, 3}, moyo_frac:to_tuple(moyo_frac:new(2, -3))) end}
    ].

to_integer_test_() ->
    [
     {"整数部分を返す",
      fun () -> ?assertEqual(2, moyo_frac:to_integer(moyo_frac:new(13, 5))) end},

     {"負の分数の整数部分を返す",
      fun () -> ?assertEqual(-4, moyo_frac:to_integer(moyo_frac:new(-29, 6))) end}
    ].

to_float_test_() ->
    [
     {"浮動小数点数に変換",
      fun () -> ?assertEqual(4/9, moyo_frac:to_float(moyo_frac:new(4, 9))) end},

     {"負の分数の時に浮動小数点数に変換",
      fun () -> ?assertEqual(-8/5, moyo_frac:to_float(moyo_frac:new(-8, 5))) end},

     {"浮動小数点数に変換(割り切れる数で)",
      fun () -> ?assertEqual(0.5, moyo_frac:to_float(moyo_frac:new(1, 2))) end}
    ].

from_binary_test_() ->
    [
     {"バイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(41, 152), moyo_frac:from_binary(<<"123/456">>)) end},

     {"分子が負のバイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(-3, 2), moyo_frac:from_binary(<<"-3/2">>)) end},

     {"分母が負のバイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(-4, 5), moyo_frac:from_binary(<<"4/-5">>)) end},

     {"分母分子が負のバイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(9, 7), moyo_frac:from_binary(<<"-9/-7">>)) end},

     {"整数のバイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(5, 1), moyo_frac:from_binary(<<"5">>)) end},

     {"負整数のバイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(-5, 1), moyo_frac:from_binary(<<"-5">>)) end},

     {"小数のバイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(1, 4), moyo_frac:from_binary(<<"0.25">>)) end},

     {"負小数のバイナリを分数に変換",
      fun () -> ?assertEqual(moyo_frac:new(-1, 4), moyo_frac:from_binary(<<"-0.25">>)) end}

    ].

from_float_binary_test_() ->
    [
     {"小数のバイナリ文字列を分数に変換",
      ?_assertEqual(moyo_frac:new(6283, 2000), moyo_frac:from_float_binary(<<"3.1415">>))},

     {"負の小数のバイナリ文字列を分数に変換",
      ?_assertEqual(moyo_frac:new(-67957, 25000), moyo_frac:from_float_binary(<<"-2.71828">>))},

     {"小数点後に複数の0が付いている場合のバイナリ文字列を分数に変換",
      ?_assertEqual(moyo_frac:new(30, 1), moyo_frac:from_float_binary(<<"30.00">>))},

     {"小数でない場合はerrorが投げられる",
      ?_assertError({badmatch, [<<"13">>]}, moyo_frac:from_float_binary(<<"13">>))},

     {"整数部がない場合にerrorが投げられる",
      ?_assertError(badarg, moyo_frac:from_float_binary(<<".16">>))},

     {"小数部がない場合にerrorが投げられる",
      ?_assertError(badarg, moyo_frac:from_float_binary(<<"21.">>))}
    ].

to_binary_test_() ->
    [
     {"分数をバイナリに変換",
      fun () -> ?assertEqual(<<"263/41">>, moyo_frac:to_binary(moyo_frac:new(789, 123))) end},

     {"負の分数をバイナリに変換",
      fun () -> ?assertEqual(<<"-329/218">>, moyo_frac:to_binary(moyo_frac:new(-987, 654))) end}
    ].

add_test_() ->
    [
     {"足し算(分数+分数).",
      fun () ->
              A = moyo_frac:new(2, 5),
              B = moyo_frac:new(1, 10),
              ?assertEqual(moyo_frac:new(5, 10), moyo_frac:add(A, B))
      end},

     {"足し算(分数+浮動小数点数)",
      fun () ->
              A = moyo_frac:new(1, 2),
              B = 1.5,
              ?assertEqual(moyo_frac:new(2, 1), moyo_frac:add(A, B))
      end},

     {"足し算(分数+整数)",
      fun () ->
              A = moyo_frac:new(2, 5),
              B = 2,
              ?assertEqual(moyo_frac:new(12, 5), moyo_frac:add(A, B))
      end},

     {"足し算(浮動小数点数+分数).",
      fun () ->
              A = 0.5,
              B = moyo_frac:new(1, 2),
              ?assertEqual(moyo_frac:new(1, 1), moyo_frac:add(A, B))
      end},

     {"足し算(浮動小数点数+浮動小数点数)",
      fun () ->
              A = 0.4,
              B = 0.1,
              ?assertEqual(moyo_frac:new(1, 2), moyo_frac:add(A, B))
      end},

     {"足し算(浮動小数点数+整数)",
      fun () ->
              A = 0.5,
              B = 1,
              ?assertEqual(moyo_frac:new(3, 2), moyo_frac:add(A, B))
      end},

     {"足し算(整数+分数).",
      fun () ->
              A = 2,
              B = moyo_frac:new(1, 3),
              ?assertEqual(moyo_frac:new(7, 3), moyo_frac:add(A, B))
      end},

     {"足し算(整数+浮動小数点数)",
      fun () ->
              A = 2,
              B = 0.5,
              ?assertEqual(moyo_frac:new(5, 2), moyo_frac:add(A, B))
      end},

     {"足し算(整数+整数)",
      fun () ->
              A = 2,
              B = 1,
              ?assertEqual(moyo_frac:new(3, 1), moyo_frac:add(A, B))
      end}
    ].

sub_test_() ->
    [
     {"引き算(分数-分数)",
      fun () ->
              A = moyo_frac:new(1, 2),
              B = moyo_frac:new(1, 6),
              ?assertEqual(moyo_frac:new(1, 3), moyo_frac:sub(A, B))
      end},

     {"左項が負の引き算",
      fun () ->
              A = moyo_frac:new(-1, 3),
              B = moyo_frac:new(1, 6),
              ?assertEqual(moyo_frac:new(-1, 2), moyo_frac:sub(A, B))
      end},

     {"右項が負の引き算",
      fun () ->
              A = moyo_frac:new(1, 6),
              B = moyo_frac:new(1, -3),
              ?assertEqual(moyo_frac:new(1, 2), moyo_frac:sub(A, B))
      end},

     {"引き算(分数-浮動小数点数)",
      fun () ->
              A = moyo_frac:new(1, 2),
              B = 1.5,
              ?assertEqual(moyo_frac:new(-1, 1), moyo_frac:sub(A, B))
      end},

     {"引き算(分数-整数)",
      fun () ->
              A = moyo_frac:new(2, 5),
              B = 2,
              ?assertEqual(moyo_frac:new(-8, 5), moyo_frac:sub(A, B))
      end},

     {"引き算(浮動小数点数-分数).",
      fun () ->
              A = 0.5,
              B = moyo_frac:new(1, 2),
              ?assertEqual(moyo_frac:new(0, 1), moyo_frac:sub(A, B))
      end},

     {"引き算(浮動小数点数-浮動小数点数)",
      fun () ->
              A = 1.3,
              B = 0.8,
              ?assertEqual(moyo_frac:new(1, 2), moyo_frac:sub(A, B))
      end},

     {"引き算(浮動小数点数-整数)",
      fun () ->
              A = 0.5,
              B = 1,
              ?assertEqual(moyo_frac:new(-1, 2), moyo_frac:sub(A, B))
      end},

     {"引き算(整数-分数).",
      fun () ->
              A = 2,
              B = moyo_frac:new(1, 3),
              ?assertEqual(moyo_frac:new(5, 3), moyo_frac:sub(A, B))
      end},

     {"引き算(整数-浮動小数点数)",
      fun () ->
              A = 2,
              B = 0.5,
              ?assertEqual(moyo_frac:new(3, 2), moyo_frac:sub(A, B))
      end},

     {"引き算(整数-整数)",
      fun () ->
              A = 2,
              B = 1,
              ?assertEqual(moyo_frac:new(1, 1), moyo_frac:sub(A, B))
      end},

     {"左項が負の引き算(浮動小数点数-浮動小数点数)",
      fun () ->
              A = moyo_frac:new(-1, 3),
              B = moyo_frac:new(1, 6),
              ?assertEqual(moyo_frac:new(-1, 2), moyo_frac:sub(A, B))
      end},

     {"右項が負の引き算(浮動小数点数-浮動小数点数)",
      fun () ->
              A = moyo_frac:new(1, 6),
              B = moyo_frac:new(1, -3),
              ?assertEqual(moyo_frac:new(1, 2), moyo_frac:sub(A, B))
      end},

     {"左項が負の引き算(整数-分数)",
      fun () ->
              A = -1,
              B = moyo_frac:new(-1, 6),
              ?assertEqual(moyo_frac:new(-5, 6), moyo_frac:sub(A, B))
      end},

     {"右項が負の引き算(整数-分数)",
      fun () ->
              A = 1,
              B = moyo_frac:new(-1, 6),
              ?assertEqual(moyo_frac:new(7, 6), moyo_frac:sub(A, B))
      end},

     {"左項が負の引き算(浮動小数点数-分数)",
      fun () ->
              A = -0.25,
              B = moyo_frac:new(1, 4),
              ?assertEqual(moyo_frac:new(-1, 2), moyo_frac:sub(A, B))
      end},

     {"右項が負の引き算(浮動小数点数-分数)",
      fun () ->
              A = 0.5,
              B = moyo_frac:new(-1, 4),
              ?assertEqual(moyo_frac:new(3, 4), moyo_frac:sub(A, B))
      end},

     {"左項が負の引き算(分数-整数)",
      fun () ->
              A = moyo_frac:new(-6, 4),
              B = 1,
              ?assertEqual(moyo_frac:new(-10, 4), moyo_frac:sub(A, B))
      end},

     {"右項が負の引き算(分数-整数)",
      fun () ->
              A = moyo_frac:new(6, 4),
              B = -1,
              ?assertEqual(moyo_frac:new(5, 2), moyo_frac:sub(A, B))
      end},

     {"左項が負の引き算(分数-浮動小数点数)",
      fun () ->
              A = moyo_frac:new(-6, 4),
              B = 0.5,
              ?assertEqual(moyo_frac:new(-2, 1), moyo_frac:sub(A, B))
      end},

     {"右項が負の引き算(分数-浮動小数点数)",
      fun () ->
              A = moyo_frac:new(6, 4),
              B = -0.5,
              ?assertEqual(moyo_frac:new(2, 1), moyo_frac:sub(A, B))
      end}
    ].

mul_test_() ->
    [
     {"掛け算する",
      fun () ->
              A = moyo_frac:new(43, 32),
              B = moyo_frac:new(4, 15),
              ?assertEqual(moyo_frac:new(43, 120), moyo_frac:mul(A, B))
      end},

     {"左項が負の数の掛け算する",
      fun () ->
              A = moyo_frac:new(-14, 29),
              B = moyo_frac:new(21, 28),
              ?assertEqual(moyo_frac:new(-21, 58), moyo_frac:mul(A, B))
      end},

     {"掛け算(分数*浮動小数点数)",
      fun () ->
              A = moyo_frac:new(1, 2),
              B = 1.5,
              ?assertEqual(moyo_frac:new(3, 4), moyo_frac:mul(A, B))
      end},

     {"掛け算(分数*整数)",
      fun () ->
              A = moyo_frac:new(2, 5),
              B = 2,
              ?assertEqual(moyo_frac:new(4, 5), moyo_frac:mul(A, B))
      end},

     {"掛け算(浮動小数点数*分数).",
      fun () ->
              A = 0.5,
              B = moyo_frac:new(1, 2),
              ?assertEqual(moyo_frac:new(1, 4), moyo_frac:mul(A, B))
      end},

     {"掛け算(浮動小数点数*浮動小数点数)",
      fun () ->
              A = 3.0,
              B = 0.5,
              ?assertEqual(moyo_frac:new(3, 2), moyo_frac:mul(A, B))
      end},

     {"掛け算(浮動小数点数*整数)",
      fun () ->
              A = 0.5,
              B = 3,
              ?assertEqual(moyo_frac:new(3, 2), moyo_frac:mul(A, B))
      end},

     {"掛け算(整数*分数).",
      fun () ->
              A = 2,
              B = moyo_frac:new(1, 3),
              ?assertEqual(moyo_frac:new(2, 3), moyo_frac:mul(A, B))
      end},

     {"掛け算(整数*浮動小数点数)",
      fun () ->
              A = 2,
              B = 0.5,
              ?assertEqual(moyo_frac:new(1, 1), moyo_frac:mul(A, B))
      end},

     {"掛け算(整数*整数)",
      fun () ->
              A = 2,
              B = 1,
              ?assertEqual(moyo_frac:new(2, 1), moyo_frac:mul(A, B))
      end},

     {"左項が負の掛け算(浮動小数点数*浮動小数点数)",
      fun () ->
              A = -1.5,
              B = 0.5,
              ?assertEqual(moyo_frac:new(-3, 4), moyo_frac:mul(A, B))
      end},

     {"左項が負の掛け算(整数*分数)",
      fun () ->
              A = -3,
              B = 0.5,
              ?assertEqual(moyo_frac:new(-3, 2), moyo_frac:mul(A, B))
      end},

     {"左項が負の掛け算(浮動小数点数*分数)",
      fun () ->
              A = -0.5,
              B = moyo_frac:new(4, 5),
              ?assertEqual(moyo_frac:new(-2, 5), moyo_frac:mul(A, B))
      end},

     {"左項が負の掛け算(分数*整数)",
      fun () ->
              A = moyo_frac:new(-4, 5),
              B = 2,
              ?assertEqual(moyo_frac:new(-8, 5), moyo_frac:mul(A, B))
      end},

     {"左項が負の掛け算(分数*浮動小数点数)",
      fun () ->
              A = moyo_frac:new(-4, 5),
              B = 0.5,
              ?assertEqual(moyo_frac:new(-2, 5), moyo_frac:mul(A, B))
      end}
    ].

divide_test_() ->
    [
     {"割り算",
      fun () ->
              A = moyo_frac:new(14, 9),
              B = moyo_frac:new(2, 3),
              ?assertEqual(moyo_frac:new(7, 3), moyo_frac:divide(A, B))
      end},

    {"右項が負の数の割り算",
     fun () ->
             A = moyo_frac:new(8, 21),
             B = moyo_frac:new(-5, 7),
             ?assertEqual(moyo_frac:new(-8, 15), moyo_frac:divide(A, B))
     end},

     {"割り算(分数/浮動小数点数)",
      fun () ->
              A = moyo_frac:new(1, 2),
              B = 1.5,
              ?assertEqual(moyo_frac:new(1, 3), moyo_frac:divide(A, B))
      end},

     {"割り算(分数/整数)",
      fun () ->
              A = moyo_frac:new(2, 5),
              B = 2,
              ?assertEqual(moyo_frac:new(1, 5), moyo_frac:divide(A, B))
      end},

     {"割り算(浮動小数点数/分数).",
      fun () ->
              A = 0.5,
              B = moyo_frac:new(1, 2),
              ?assertEqual(moyo_frac:new(1, 1), moyo_frac:divide(A, B))
      end},

     {"割り算(浮動小数点数/浮動小数点数)",
      fun () ->
              A = 0.4,
              B = 0.1,
              ?assertEqual(moyo_frac:new(4, 1), moyo_frac:divide(A, B))
      end},

     {"割り算(浮動小数点数/整数)",
      fun () ->
              A = 0.5,
              B = 2,
              ?assertEqual(moyo_frac:new(1, 4), moyo_frac:divide(A, B))
      end},

     {"割り算(整数/分数).",
      fun () ->
              A = 2,
              B = moyo_frac:new(1, 3),
              ?assertEqual(moyo_frac:new(6, 1), moyo_frac:divide(A, B))
      end},

     {"割り算(整数/浮動小数点数)",
      fun () ->
              A = 2,
              B = 0.5,
              ?assertEqual(moyo_frac:new(4, 1), moyo_frac:divide(A, B))
      end},

     {"割り算(整数/整数)",
      fun () ->
              A = 1,
              B = 2,
              ?assertEqual(moyo_frac:new(1, 2), moyo_frac:divide(A, B))
      end},

     {"右項が負の割り算(浮動小数点数/浮動小数点数)",
     fun () ->
             A = 1.5,
             B = -0.5,
             ?assertEqual(moyo_frac:new(-3, 1), moyo_frac:divide(A, B))
     end},

     {"右項が負の割り算(整数/分数)",
     fun () ->
             A = 2,
             B = moyo_frac:new(-3, 2),
             ?assertEqual(moyo_frac:new(-4, 3), moyo_frac:divide(A, B))
     end},

     {"右項が負の割り算(浮動小数点数/分数)",
     fun () ->
             A = 1.5,
             B = moyo_frac:new(-3, 2),
             ?assertEqual(moyo_frac:new(-1, 1), moyo_frac:divide(A, B))
     end},

     {"右項が負の割り算(分数/整数)",
     fun () ->
             A = moyo_frac:new(5, 2),
             B = -2,
             ?assertEqual(moyo_frac:new(-5, 4), moyo_frac:divide(A, B))
     end},

     {"右項が負の割り算(分数-浮動小数点数)",
     fun () ->
             A = moyo_frac:new(5, 2),
             B = -1.5,
             ?assertEqual(moyo_frac:new(-5, 3), moyo_frac:divide(A, B))
     end}
    ].

is_fraction_test_() ->
    [
     {"is_fraction(fraction)", ?_assertEqual(true,  moyo_frac:is_fraction(moyo_frac:new(1,2)))},
     {"is_fraction(integer)",  ?_assertEqual(false, moyo_frac:is_fraction(3))},
     {"is_fraction(float)",    ?_assertEqual(false, moyo_frac:is_fraction(4.5))},
     {"is_fraction(atom)",     ?_assertEqual(false, moyo_frac:is_fraction(atom))},
     {"is_fraction(tuple)",    ?_assertEqual(false, moyo_frac:is_fraction({item1, item2}))},
     {"is_fraction(list)",     ?_assertEqual(false, moyo_frac:is_fraction([item1, item2]))},
     {"is_fraction(string)",   ?_assertEqual(false, moyo_frac:is_fraction("6/7"))},
     {"is_fraction(binary)",   ?_assertEqual(false, moyo_frac:is_fraction(<<"8/9">>))}
    ].

comp_test_() ->
    [
     {"左値が大きい場合の比較",
      fun () ->
              A = moyo_frac:new(239, 209),
              B = moyo_frac:new(459, 490),
              ?assertEqual(1, moyo_frac:comp(A, B))
      end},

     {"右値が大きい場合の比較",
      fun () ->
              A = moyo_frac:new(34, 120),
              B = moyo_frac:new(21, 49),
              ?assertEqual(-1, moyo_frac:comp(A, B))
      end},

     {"値が同じ",
      fun () ->
              A = moyo_frac:new(12, 20),
              B = moyo_frac:new(3, 5),
              ?assertEqual(0, moyo_frac:comp(A, B))
      end},

     {"片方が負",
      fun () ->
              A = moyo_frac:new(34, 120),
              B = moyo_frac:new(-21, 49),
              ?assertEqual(1, moyo_frac:comp(A, B))
      end},

     {"両方が負",
      fun () ->
              A = moyo_frac:new(-34, 120),
              B = moyo_frac:new(-21, 49),
              ?assertEqual(1, moyo_frac:comp(A, B))
      end}
    ].

max_test_() ->
    [
     {"Aの方が大きい",
      fun () ->
              A = moyo_frac:new(5, 3),
              B = moyo_frac:new(7, 6),
              ?assertEqual(A, moyo_frac:max(A, B))
      end},

     {"Bの方が大きい",
      fun () ->
              A = moyo_frac:new(3, 4),
              B = moyo_frac:new(5, 6),
              ?assertEqual(B, moyo_frac:max(A, B))
      end},

     {"両方が同じ大きさ",
      fun () ->
              M = A = B = moyo_frac:new(5, 9),
              ?assertEqual(M, moyo_frac:max(A, B))
      end}
    ].

min_test_() ->
    [
     {"Aの方が小さい",
      fun () ->
              A = moyo_frac:new(4, 9),
              B = moyo_frac:new(5, 6),
              ?assertEqual(A, moyo_frac:min(A, B))
      end},

     {"Bの方が小さい",
      fun () ->
              A = moyo_frac:new(9, 2),
              B = moyo_frac:new(4, 3),
              ?assertEqual(B, moyo_frac:min(A, B))
      end},

     {"両方が同じ値",
      fun () ->
              M = A = B = moyo_frac:new(4, 5),
              ?assertEqual(M, moyo_frac:min(A, B))
      end}
    ].
