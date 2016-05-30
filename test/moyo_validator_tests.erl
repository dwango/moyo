%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_validatorモジュールのユニットテスト
-module(moyo_validator_tests).

-include_lib("eunit/include/eunit.hrl").

validate_integer_test_() ->
    [
     {"integer",
      fun () ->
              Int = 20130725,

              ?assertEqual({ok, Int}, moyo_validator:validate(Int, integer))
      end},

     {"負数",
      fun () ->
              NegInt = -19890101,

              ?assertEqual({ok, NegInt}, moyo_validator:validate(NegInt, integer))
      end},

     {"0",
      fun () ->
              Zero = 0,

              ?assertEqual({ok, Zero}, moyo_validator:validate(Zero, integer))
      end},

     {"空オプション",
      fun () ->
              Int = 170000,

              ?assertEqual({ok, Int}, moyo_validator:validate(Int, integer, []))
      end},

     {"空制限",
      fun () ->
              Int = 365,

              ?assertEqual({ok, Int}, moyo_validator:validate(Int, {integer, []}))
      end},

     {"非負整数かどうかをチェック",
      fun () ->
              NonNegInt = 634,

              ?assertEqual({ok, NonNegInt},
                  moyo_validator:validate(NonNegInt, {integer, [non_negative]}))
      end},

     {"非負整数指定時に入力が負数",
      fun () ->
              NegInt = -6173,

              ?assertMatch({error, _}, moyo_validator:validate(NegInt, {integer, [non_negative]}))
      end},

     {"偶数であることの確認",
      fun () ->
              Int = 12,

              ?assertEqual({ok, Int}, moyo_validator:validate(Int, {integer, [even]}))
      end},

     {"偶数であることの確認(入力が奇数)",
      fun () ->
              Int = 123,

              ?assertMatch({error, _}, moyo_validator:validate(Int, {integer, [even]}))
      end},

     {"奇数であることの確認",
      fun () ->
              Int = 21,

              ?assertEqual({ok, Int}, moyo_validator:validate(Int, {integer, [odd]}))
      end},

     {"奇数であることの確認(入力が偶数)",
      fun () ->
              Int = 210,

              ?assertMatch({error, _}, moyo_validator:validate(Int, {integer, [odd]}))
      end},

     {"範囲指定",
      fun () ->
              Int = 1234,

              ?assertEqual({ok, Int},
                  moyo_validator:validate(Int, {integer, [{range, 123, 12345}]}))
      end},

     {"下端",
      fun () ->
              Int = 1234,

              ?assertEqual({ok, Int},
                  moyo_validator:validate(Int, {integer, [{range, 1234, 12345}]}))
      end},

     {"上端",
      fun () ->
              Int = 1234,

              ?assertEqual({ok, Int},
                  moyo_validator:validate(Int, {integer, [{range, 123, 1234}]}))
      end},

     {"min =:= max",
      fun () ->
              Int = 1234,

              ?assertEqual({ok, Int},
                  moyo_validator:validate(Int, {integer, [{range, 1234, 1234}]}))
      end},

     {"integer, binary_in",
      fun () ->
              Int    = 551,
              BinInt = <<"551">>,

              ?assertEqual({ok, Int}, moyo_validator:validate(BinInt, integer, [binary_in]))
      end},

     {"integerではない",
      fun () ->
              Input = '1',

              ?assertMatch({error, _}, moyo_validator:validate(Input, integer))
      end},

     {"Bitsビットの符号付整数",
      fun () ->
              ok = lists:foreach(fun ({Spec, {Min, Max}}) ->
                                         ?assertMatch({error, _},   moyo_validator:validate(Min - 1, {integer, [Spec]})),
                                         ?assertEqual({ok,    Min}, moyo_validator:validate(Min,     {integer, [Spec]})),
                                         ?assertEqual({ok,    Max}, moyo_validator:validate(Max,     {integer, [Spec]})),
                                         ?assertMatch({error, _},   moyo_validator:validate(Max + 1, {integer, [Spec]}))
                                 end,
                                 [
                                  {{signed,   8},  {-128,                 127}},
                                  {{signed,   16}, {-32768,               32767}},
                                  {{signed,   32}, {-2147483648,          2147483647}},
                                  {{signed,   64}, {-9223372036854775808, 9223372036854775807}},
                                  {{unsigned, 8},  {0,                    255}},
                                  {{unsigned, 16}, {0,                    65535}},
                                  {{unsigned, 32}, {0,                    4294967295}},
                                  {{unsigned, 64}, {0,                    18446744073709551615}}
                                 ]),
              ?assertMatch({error, _}, moyo_validator:validate(0, {integer, [{signed,   0}]})),
              ?assertMatch({ok,    0}, moyo_validator:validate(0, {integer, [{signed,   1}]})),
              ?assertMatch({error, _}, moyo_validator:validate(0, {integer, [{unsigned, 0}]})),
              ?assertMatch({ok,    0}, moyo_validator:validate(0, {integer, [{unsigned, 1}]}))
      end},

     % 以下、ホワイトボックステスト
     {"制限を複数指定時、前の制限で失敗したときの挙動",
      fun () ->
              NegInt = -6173,

              ?assertMatch({error, _},
                  moyo_validator:validate(NegInt, {integer, [non_negative, {range, -65536, 65535}]}))
      end},

     {"未定義の制限を指定",
      fun () ->
              Int = 867265,

              ?assertMatch({error, _},
                  moyo_validator:validate(Int, {integer, [negative]}))
      end},

     {"正数かどうかをチェック",
      fun () ->
              PosInt = 48903,
              ?assertEqual({ok, PosInt}, moyo_validator:validate(PosInt, {integer, [positive]})),

              NegInt = -34927,
              ?assertMatch({error, _}, moyo_validator:validate(NegInt, {integer, [positive]})),

              Zero = 0,
              ?assertMatch({error, _}, moyo_validator:validate(Zero, {integer, [positive]}))
      end},

     {"負数かどうかをチェック",
      fun () ->
              NegInt = -439820,
              ?assertEqual({ok, NegInt}, moyo_validator:validate(NegInt, {integer, [negative]})),

              PosInt = 489032167598,
              ?assertMatch({error, _}, moyo_validator:validate(PosInt, {integer, [negative]})),

              Zero = 0,
              ?assertMatch({error, _}, moyo_validator:validate(Zero, {integer, [negative]}))
      end},

     {"more制限のテスト",
      fun () ->
              Int1 = 3490827,
              ?assertEqual({ok, Int1}, moyo_validator:validate(Int1, {integer, [{more, 93458}]})),

              Int2 = 93458,
              ?assertMatch({error, _}, moyo_validator:validate(Int2, {integer, [{more, Int2}]})),

              Int3 = 4389,
              ?assertMatch({error, _}, moyo_validator:validate(Int3, {integer, [{more, 93458}]}))
      end},

     {"less制限のテスト",
      fun () ->
              Int1 = 3489,
              ?assertEqual({ok, Int1}, moyo_validator:validate(Int1, {integer, [{less, 389232}]})),

              Int2 = 389232,
              ?assertMatch({error, _}, moyo_validator:validate(Int2, {integer, [{less, Int2}]})),

              Int3 = 62819011,
              ?assertMatch({error, _}, moyo_validator:validate(Int3, {integer, [{less, 389232}]}))
      end},

     {"複数制限のバリデート: 100より大きい偶数",
      fun () ->
              Int = 182907432,

              ?assertEqual({ok, Int}, moyo_validator:validate(Int, {integer, [{more, 100}, even]}))
      end},

     {"複数制限のバリデート: 100より大きい偶数(入力値は奇数)",
      fun () ->
              Int = 1289647821,

              ?assertMatch({error, _}, moyo_validator:validate(Int, {integer, [{more, 100}, even]}))
      end}
    ].

validate_float_test_() ->
    [
     {"float",
      fun () ->
              Float = 20130725.12,

              ?assertEqual({ok, Float}, moyo_validator:validate(Float, float))
      end},
     {"integerは許可されない",
      fun () ->
              Int = 20130725,

              ?assertMatch({error, {not_match, _, _}}, moyo_validator:validate(Int, float))
      end},
     {"非負小数",
      fun () ->
              NonNegFloat = 634.12,

              ?assertEqual({ok, NonNegFloat},
                  moyo_validator:validate(NonNegFloat, {float, [non_negative]}))
      end},
     {"非負小数指定時に入力が負数",
      fun () ->
              NegFloat = -6173.12,

              ?assertMatch({error, {not_match, _, _}}, moyo_validator:validate(NegFloat, {float, [non_negative]}))
      end},
     {"範囲指定",
      fun () ->
              Float = 1234.0,

              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {float, [{range, 123.0, 12345.0}]})),

              %% 範囲指定は整数でも可能
              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {float, [{range, 123, 12345}]}))
      end},
     {"下端",
      fun () ->
              Float = 1234.0,

              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {float, [{range, 1234.0, 12345.0}]}))
      end},
     {"上端",
      fun () ->
              Float = 1234.0,

              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {float, [{range, 123.0, 1234.0}]}))
      end},
     {"float, binary_in",
      fun () ->
              Float    = 551.12,
              BinFloat = <<"551.12">>,

              ?assertEqual({ok, Float}, moyo_validator:validate(BinFloat, float, [binary_in]))
      end},
     {"float, binary_in: 入力が整数形式の場合はerrorを返す",
      fun () ->
              BinFloat = <<"551">>,

              ?assertMatch({error, _}, moyo_validator:validate(BinFloat, float, [binary_in]))
      end},
     {"float, allow_integer",
      fun () ->
              Int = 718929087,
              Float = float(Int),

              ?assertEqual({ok, Float}, moyo_validator:validate(Int, float, [allow_integer]))
      end},
     {"正数かどうかをチェック",
      fun () ->
              PosInt = 2398.13,
              ?assertEqual({ok, PosInt}, moyo_validator:validate(PosInt, {float, [positive]})),

              NegInt = -6231.5786,
              ?assertMatch({error, _}, moyo_validator:validate(NegInt, {float, [positive]})),

              Zero = 0.0,
              ?assertMatch({error, _}, moyo_validator:validate(Zero, {float, [positive]}))
      end},
     {"負数かどうかをチェック",
      fun () ->
              NegInt = -3097.198,
              ?assertEqual({ok, NegInt}, moyo_validator:validate(NegInt, {float, [negative]})),

              PosInt = 39087.23,
              ?assertMatch({error, _}, moyo_validator:validate(PosInt, {float, [negative]})),

              Zero = 0.0,
              ?assertMatch({error, _}, moyo_validator:validate(Zero, {float, [negative]}))
      end},
     {"more制限のテスト",
      fun () ->
              Int1 = 389471.1323,
              ?assertEqual({ok, Int1}, moyo_validator:validate(Int1, {float, [{more, 29399.1892}]})),

              Int2 = 29399.1892,
              ?assertMatch({error, _}, moyo_validator:validate(Int2, {float, [{more, Int2}]})),

              Int3 = 27836.17826,
              ?assertMatch({error, _}, moyo_validator:validate(Int3, {float, [{more, 29399.1892}]}))
      end},
     {"less制限のテスト",
      fun () ->
              Int1 = 198.1926,
              ?assertEqual({ok, Int1}, moyo_validator:validate(Int1, {float, [{less, 89231.1}]})),

              Int2 = 89231.1,
              ?assertMatch({error, _}, moyo_validator:validate(Int2, {float, [{less, Int2}]})),

              Int3 = 1928619.13679,
              ?assertMatch({error, _}, moyo_validator:validate(Int3, {float, [{less, 89231.1}]}))
      end}
    ].

validate_number_test_() ->
    [
     {"number: integer",
      fun () ->
              Int = 20130725,

              ?assertEqual({ok, Int}, moyo_validator:validate(Int, number))
      end},
     {"number: float",
      fun () ->
              Float = 20130725.12,

              ?assertMatch({ok, Float}, moyo_validator:validate(Float, number))
      end},
     {"非負",
      fun () ->
              NonNegFloat = 634.12,

              ?assertEqual({ok, NonNegFloat},
                  moyo_validator:validate(NonNegFloat, {number, [non_negative]}))
      end},
     {"非負指定時に入力が負数",
      fun () ->
              NegFloat = -6173.12,

              ?assertMatch({error, {not_match, _, _}}, moyo_validator:validate(NegFloat, {number, [non_negative]}))
      end},
     {"範囲指定",
      fun () ->
              Float = 1234.0,

              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {number, [{range, 123.0, 12345.0}]})),

              %% 範囲指定は整数でも可能
              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {number, [{range, 123, 12345}]}))
      end},
     {"下端",
      fun () ->
              Float = 1234.0,

              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {number, [{range, 1234.0, 12345.0}]}))
      end},
     {"上端",
      fun () ->
              Float = 1234.0,

              ?assertEqual({ok, Float},
                  moyo_validator:validate(Float, {number, [{range, 123.0, 1234.0}]}))
      end},
     {"number, binary_in: float",
      fun () ->
              Float    = 551.12,
              BinFloat = <<"551.12">>,

              ?assertEqual({ok, Float}, moyo_validator:validate(BinFloat, number, [binary_in]))
      end},
     {"number, binary_in: integer",
      fun () ->
              Int = 551,
              BinInt = <<"551">>,

              ?assertMatch({ok, Int}, moyo_validator:validate(BinInt, number, [binary_in]))
      end},
     {"number, binary_in: string",
      fun () ->
              ?assertMatch({error, _}, moyo_validator:validate(<<"hoge">>, number, [binary_in]))
      end},
     {"正数かどうかをチェック",
      fun () ->
              PosInt = 2398.13,
              ?assertEqual({ok, PosInt}, moyo_validator:validate(PosInt, {number, [positive]})),

              NegInt = -6231.5786,
              ?assertMatch({error, _}, moyo_validator:validate(NegInt, {number, [positive]})),

              Zero = 0.0,
              ?assertMatch({error, _}, moyo_validator:validate(Zero, {number, [positive]}))
      end},
     {"負数かどうかをチェック",
      fun () ->
              NegInt = -3097.198,
              ?assertEqual({ok, NegInt}, moyo_validator:validate(NegInt, {number, [negative]})),

              PosInt = 39087.23,
              ?assertMatch({error, _}, moyo_validator:validate(PosInt, {number, [negative]})),

              Zero = 0.0,
              ?assertMatch({error, _}, moyo_validator:validate(Zero, {number, [negative]}))
      end},
     {"more制限のテスト",
      fun () ->
              Int1 = 389471.1323,
              ?assertEqual({ok, Int1}, moyo_validator:validate(Int1, {number, [{more, 29399.1892}]})),

              Int2 = 29399.1892,
              ?assertMatch({error, _}, moyo_validator:validate(Int2, {number, [{more, Int2}]})),

              Int3 = 27836.17826,
              ?assertMatch({error, _}, moyo_validator:validate(Int3, {number, [{more, 29399.1892}]}))
      end},
     {"less制限のテスト",
      fun () ->
              Int1 = 198.1926,
              ?assertEqual({ok, Int1}, moyo_validator:validate(Int1, {number, [{less, 89231.1}]})),

              Int2 = 89231.1,
              ?assertMatch({error, _}, moyo_validator:validate(Int2, {number, [{less, Int2}]})),

              Int3 = 1928619.13679,
              ?assertMatch({error, _}, moyo_validator:validate(Int3, {number, [{less, 89231.1}]}))
      end}
    ].

validate_string_test_() ->
    [
     {"string",
      fun () ->
              String = "aki",

              ?assertEqual({ok, String}, moyo_validator:validate(String, string))
      end},

     {"empty string",
      fun () ->
              EmpStr = "",

              ?assertEqual({ok, EmpStr}, moyo_validator:validate(EmpStr, string))
      end},

     {"string, max_length",
      fun () ->
              String = "kabukiza",

              ?assertEqual({ok, String},
                  moyo_validator:validate(String, {string, [{max_length, 10}]}))
      end},

     {"string, max_length, 上端",
      fun () ->
              String = "meiziza",

              ?assertEqual({ok, String},
                  moyo_validator:validate(String, {string, [{max_length, 7}]}))
      end},

     {"string, min_length",
      fun () ->
              String = "kabukiza",

              ?assertEqual({ok, String},
                  moyo_validator:validate(String, {string, [{min_length, 5}]})),
              ?assertMatch({error,{not_match, {_, {_, _}}, {_, String}}},
                  moyo_validator:validate(String, {string, [{min_length, 9}]}))
      end},

     {"string, regexp",
      fun () ->
              String  = "Dwango Mobile",
              Pattern = "Dwango",

              ?assertEqual({ok, String},
                  moyo_validator:validate(String, {string, [{regexp, Pattern}]}))
      end},

     {"string, ascii",
      fun () ->
              Ascii = "Dwango Mobile",
              ?assertEqual({ok, Ascii}, moyo_validator:validate(Ascii, {string, [ascii]})),

              Utf8 = "ドワンゴモバイル",
              ?assertMatch({error, _}, moyo_validator:validate(Utf8, {string, [ascii]}))
      end},

     {"string, not_empty",
      fun () ->
              String = "Not Empty",
              ?assertEqual({ok, String}, moyo_validator:validate(String, {string, [not_empty]})),

              Empty = "",
              ?assertMatch({error, _}, moyo_validator:validate(Empty, {string, [not_empty]}))
      end},

     {"string, binary_in",
      fun () ->
              String = "String Data",
              BinStr = <<"String Data">>,

              ?assertEqual({ok, String}, moyo_validator:validate(BinStr, string, [binary_in]))
      end}
    ].

validate_binary_test_() ->
    [
     {"binary",
      fun () ->
              Binary = <<"binary mojiretsu">>,

              ?assertEqual({ok, Binary}, moyo_validator:validate(Binary, binary))
      end},

     {"empty binary",
      fun () ->
              EmpBin = <<"">>,

              ?assertEqual({ok, EmpBin}, moyo_validator:validate(EmpBin, binary))
      end},

     {"binary, max_length",
      fun () ->
              Binary = <<"niconico">>,

              ?assertEqual({ok, Binary},
                  moyo_validator:validate(Binary, {binary, [{max_length, 10}]}))
      end},

     {"binary, max_length, 上端",
      fun () ->
              Binary = <<"niconama">>,

              ?assertEqual({ok, Binary},
                  moyo_validator:validate(Binary, {binary, [{max_length, 8}]}))
      end},

     {"binary, min_length",
      fun () ->
              Binary = <<"niconico">>,

              ?assertEqual({ok, Binary},
                  moyo_validator:validate(Binary, {binary, [{min_length, 5}]})),
              ?assertMatch({error,{not_match, {_, {_, _}}, {_, Binary}}},
                  moyo_validator:validate(Binary, {binary, [{min_length, 9}]}))
      end},

     {"binary, regexp",
      fun () ->
              Binary  = <<"Qteras">>,
              Pattern = <<"teras$">>,

              ?assertEqual({ok, Binary},
                  moyo_validator:validate(Binary, {binary, [{regexp, Pattern}]}))
      end},

     {"binary, ascii",
      fun () ->
              Ascii = <<"Qteras">>,
              ?assertEqual({ok, Ascii}, moyo_validator:validate(Ascii, {binary, [ascii]})),

              Utf8 = <<"キテラス">>,
              ?assertMatch({error, _}, moyo_validator:validate(Utf8, {binary, [ascii]}))
      end},

     {"binary, not_empty",
      fun () ->
              Binary = <<"Not Empty">>,
              ?assertEqual({ok, Binary}, moyo_validator:validate(Binary, {binary, [not_empty]})),

              Empty = <<>>,
              ?assertMatch({error, _}, moyo_validator:validate(Empty, {binary, [not_empty]}))
      end},

     {"binary, binary_in",
      fun () ->
              Binary = <<"Delivery">>,

              ?assertEqual({ok, Binary}, moyo_validator:validate(Binary, binary, [binary_in]))
      end}
    ].

validate_any_test_() ->
    [
     {"anyの場合は、どんな値が渡されても常に成功する",
      fun () ->
              ?assertEqual({ok, 10}, moyo_validator:validate(10, any)),
              ?assertEqual({ok, atom}, moyo_validator:validate(atom, any)),
              ?assertEqual({ok, [1, {one, two}, self()]}, moyo_validator:validate([1, {one, two}, self()], any))
      end}
    ].

validate_boolean_test_() ->
    [
     {"true",
      fun () ->
              True = true,

              ?assertEqual({ok, True}, moyo_validator:validate(True, boolean))
      end},

     {"false",
      fun () ->
              False = false,

              ?assertEqual({ok, False}, moyo_validator:validate(False, boolean))
      end},

     {"boolean, binary_in",
      fun () ->
              Boolean = true,
              BinBool = <<"true">>,

              ?assertEqual({ok, Boolean},
                  moyo_validator:validate(BinBool, boolean, [binary_in]))
      end},

     {"boolean, int_to_bool, 1",
      fun () ->
              Boolean = true,
              IntBool = 1,

              ?assertEqual({ok, Boolean},
                  moyo_validator:validate(IntBool, boolean, [int_to_bool]))
      end},

     {"boolean, int_to_bool, 0",
      fun () ->
              Boolean = false,
              IntBool = 0,

              ?assertEqual({ok, Boolean},
                  moyo_validator:validate(IntBool, boolean, [int_to_bool]))
      end},

     {"boolean, int_to_bool, 2",
      fun () ->
              IntBool = 2,

              ?assertMatch({error, _},
                  moyo_validator:validate(IntBool, boolean, [int_to_bool]))
      end},

     {"boolean, binary_in -> int_to_bool",
      fun () ->
              Boolean    = true,
              BinIntBool = <<"1">>,

              ?assertEqual({ok, Boolean},
                  moyo_validator:validate(BinIntBool, boolean, [binary_in, int_to_bool]))
      end}
    ].

validate_atom_test_() ->
    [
     {"atom",
      fun () ->
              Atom = astro,

              ?assertEqual({ok, Atom}, moyo_validator:validate(Atom, atom))
      end},

     {"atom, single quote",
      fun () ->
              Atom = 'Uran',

              ?assertEqual({ok, Atom}, moyo_validator:validate(Atom, atom))
      end},

     {"atom, binary_in",
      fun () ->
              Atom    = jetto,
              BinAtom = <<"jetto">>,

              ?assertEqual({ok, Atom}, moyo_validator:validate(BinAtom, atom, [binary_in]))
      end},

     %% 現在、以下のような挙動をする
     {"int_to_boolを使ってatom型をバリデーション",
      fun () ->
              Atom  = true,
              Input = 1,

              ?assertEqual({ok, Atom}, moyo_validator:validate(Input, atom, [int_to_bool]))
      end}
    ].

validate_datetime_test_() ->
    DateTime    = {{2014, 7, 1}, {1, 2, 3}},
    Iso8601     = <<"2014-07-01T10:02:03+09:00">>,
    UnixTime    = 1404144123,
    UnixTimeBin = <<"1404144123">>,
    [
     {"match",
      fun() ->
              Value = {{2014, 6, 2}, {0, 0, 0}},
              ?assertEqual({ok, Value}, moyo_validator:validate(Value, datetime))
      end},
     {"not match",
      fun() ->
              Value = [atom, "string", <<"2014-02-01T02:04:05">>],
              [?assertMatch({error, _}, moyo_validator:validate(X, datetime)) || X <- Value]
      end},
     {"datetime to_datetime",
      fun() ->
              TValue = [DateTime, Iso8601, UnixTime],
              FValue = [UnixTimeBin],
              [?assertEqual({ok, DateTime}, moyo_validator:validate(X, datetime, [to_datetime])) || X <- TValue],
              [?assertMatch({error, _},     moyo_validator:validate(X, datetime, [to_datetime])) || X <- FValue]
      end},
     {"datetime {to_datetime, unixtime}",
      fun() ->
              TValue = [UnixTime],
              FValue = [DateTime, Iso8601, UnixTimeBin],
              [?assertEqual({ok, DateTime}, moyo_validator:validate(X, datetime, [{to_datetime, unixtime}])) || X <- TValue],
              [?assertMatch({error, _},     moyo_validator:validate(X, datetime, [{to_datetime, unixtime}])) || X <- FValue]
      end},
     {"datetime {to_datetime, iso8601}",
      fun() ->
              TValue = [Iso8601],
              FValue = [DateTime, UnixTime, UnixTimeBin],
              [?assertEqual({ok, DateTime}, moyo_validator:validate(X, datetime, [{to_datetime, iso8601}])) || X <- TValue],
              [?assertMatch({error, _},     moyo_validator:validate(X, datetime, [{to_datetime, iso8601}])) || X <- FValue]
      end},
     {"datetime binary_in",
      fun() ->
              FValue = [DateTime, Iso8601, UnixTime, UnixTimeBin],
              [?assertMatch({error, _}, moyo_validator:validate(X, datetime, [binary_in])) || X <- FValue]
      end},
     {"datetime binary_in {to_datetime, iso8601}",
      fun() ->
              TValue = [Iso8601],
              FValue = [DateTime, UnixTime, UnixTimeBin],
              [?assertEqual({ok, DateTime}, moyo_validator:validate(X, datetime, [binary_in, {to_datetime, iso8601}])) || X <- TValue],
              [?assertMatch({error, _},     moyo_validator:validate(X, datetime, [binary_in, {to_datetime, iso8601}])) || X <- FValue]
      end},
     {"datetime binary_in {to_datetime, unixtime}",
      fun() ->
              TValue = [UnixTimeBin],
              FValue = [DateTime, UnixTime, Iso8601],
              [?assertEqual({ok, DateTime}, moyo_validator:validate(X, datetime, [binary_in, {to_datetime, unixtime}])) || X <- TValue],
              [?assertMatch({error, _},     moyo_validator:validate(X, datetime, [binary_in, {to_datetime, unixtime}])) || X <- FValue]
      end},
     {"datetime binary_in to_datetime",
      fun() ->
              TValue = [Iso8601, UnixTimeBin],
              FValue = [DateTime, UnixTime],
              [?assertEqual({ok, DateTime}, moyo_validator:validate(X, datetime, [binary_in, to_datetime])) || X <- TValue],
              [?assertMatch({error, _},     moyo_validator:validate(X, datetime, [binary_in, to_datetime])) || X <- FValue]
      end},
     {"datetime iso8601 check",
      fun() ->
              Expected = {{2014, 7, 1}, {1, 2, 3}},
              Input    = [
                          <<"2014-07-01T10:02:03+09:00">>, <<"20140701T010203">>, <<"2014-07-01T01:02:03Z">>
                         ],
              [?assertEqual({ok, Expected}, moyo_validator:validate(X, datetime, [{to_datetime, iso8601}])) || X <- Input]
      end},
     {"datetime more",
      fun() ->
              Min   = {{2014, 7, 1}, {1, 2, 2}},
              Equal = {{2014, 7, 1}, {1, 2, 3}},
              Max   = {{2014, 7, 1}, {1, 2, 4}},
              Input = [
                       <<"2014-07-01T10:02:03+09:00">>, <<"20140701T010203">>, 1404144123
                      ],
              [?assertEqual({ok, Equal}, moyo_validator:validate(X, {datetime, [{more, Min}]},   [to_datetime])) || X <- Input],
              [?assertMatch({error, _},  moyo_validator:validate(X, {datetime, [{more, Equal}]}, [to_datetime])) || X <- Input],
              [?assertMatch({error, _},  moyo_validator:validate(X, {datetime, [{more, Max}]},   [to_datetime])) || X <- Input]
      end},
     {"datetime less",
      fun() ->
              Min   = {{2014, 7, 1}, {1, 2, 2}},
              Equal = {{2014, 7, 1}, {1, 2, 3}},
              Max   = {{2014, 7, 1}, {1, 2, 4}},
              Input = [
                       <<"2014-07-01T10:02:03+09:00">>, <<"20140701T010203">>, 1404144123
                      ],
              [?assertEqual({ok, Equal}, moyo_validator:validate(X, {datetime, [{less, Max}]},   [to_datetime])) || X <- Input],
              [?assertMatch({error, _},  moyo_validator:validate(X, {datetime, [{less, Equal}]}, [to_datetime])) || X <- Input],
              [?assertMatch({error, _},  moyo_validator:validate(X, {datetime, [{less, Min}]},   [to_datetime])) || X <- Input]
      end},
     {"datetime range",
      fun() ->
              Min   = {{2014, 7, 1}, {1, 2, 2}},
              Equal = {{2014, 7, 1}, {1, 2, 3}},
              More  = {{2014, 7, 1}, {1, 2, 4}},
              Max   = {{2014, 7, 1}, {1, 2, 5}},
              Input = Equal,
              ?assertEqual({ok, Input}, moyo_validator:validate(Input, {datetime, [{range, Min, Max}]})),
              ?assertEqual({ok, Input}, moyo_validator:validate(Input, {datetime, [{range, Equal, Max}]})),
              ?assertEqual({ok, Input}, moyo_validator:validate(Input, {datetime, [{range, Min, Equal}]})),
              ?assertEqual({ok, Input}, moyo_validator:validate(Input, {datetime, [{range, Equal, Equal}]})),
              ?assertMatch({error, _}, moyo_validator:validate(Input, {datetime, [{range, More, Max}]})),
              ?assertMatch({error, _}, moyo_validator:validate(Input, {datetime, [{range, Max, Min}]}))
      end},
     {"date equal",
      fun() ->
              Min   = {{2014, 7, 1}, {1, 2, 2}},
              Equal = {{2014, 7, 1}, {1, 2, 3}},
              Max   = {{2014, 7, 1}, {1, 2, 4}},
              Input = [
                       <<"2014-07-01T10:02:03+09:00">>, <<"20140701T010203">>, 1404144123
                      ],
              [?assertMatch({error, _},  moyo_validator:validate(X, {datetime, [{equal, Max}]},   [to_datetime])) || X <- Input],
              [?assertEqual({ok, Equal}, moyo_validator:validate(X, {datetime, [{equal, Equal}]}, [to_datetime])) || X <- Input],
              [?assertMatch({error, _},  moyo_validator:validate(X, {datetime, [{equal, Min}]},   [to_datetime])) || X <- Input]
      end}
    ].

validate_list_test_() ->
    [
     {"list",
      fun () ->
              List = ["apple", "banana", "cherry"],

              ?assertEqual({ok, List}, moyo_validator:validate(List, {list, string}))
      end},

     {"listの中の型が違う",
      fun () ->
              List = [honeydew_melon],
              Type = integer,

              ?assertMatch({error, _}, moyo_validator:validate(List, {list, Type}))
      end},

     {"listのバリデーションでlistじゃない型が来た場合",
      fun () ->
              Input = grapes,

              ?assertMatch({error, _}, moyo_validator:validate(Input, {list, atom}))
      end}
    ].

validate_tuple_test_() ->
    [
     {"tuple check",
      fun() ->
              TestData = [
                          %% {InputValue, Spec}
                          {{1, two, "three"},             {tuple, [{integer, [non_negative]}, {enum, [one, two]}, {string, [ascii]}]}},
                          {{1},                           {tuple, [integer]}},
                          {[{1, "assoc"}, {2, "assoc2"}], {list, {tuple, [integer, string]}}}
                         ],
              [?assertEqual({ok, Value}, moyo_validator:validate(Value, Spec))
               || {Value, Spec} <- TestData]
      end},
     {"tuple check: error",
      fun() ->
              TestData = [
                          %% {InputValue, Spec}
                          {{1, two, "three"},   {tuple, [integer, integer, string]}},
                          {{1, two, "three"},   {tuple, [{integer, [{range, 3, 10}]}, atom, string]}}
                         ],
              [?assertMatch({error, _}, moyo_validator:validate(Value, Spec))
               || {Value, Spec} <- TestData]
      end},
     {"error: not_tuple",
      fun() ->
              ?assertMatch({error, {not_tuple, _, _}}, moyo_validator:validate([1,2], {tuple, [integer, integer]}))
      end},
     {"error: incorrect_spec_size",
      fun() ->
              Input = [
                       %% {InputValue, Spec}
                       {{1,2},   {tuple, [integer, integer, integer]}},
                       {{1,2,3}, {tuple, [integer, integer]}}
                      ],
              [?assertEqual({error, incorrect_spec_size}, moyo_validator:validate(Value, Spec))
               || {Value, Spec} <- Input]
      end},
     {"error: not_supported_spec",
      fun() ->
              ?assertMatch({error, {not_supported_spec, _, _}},
                           moyo_validator:validate({1,2}, {tuple, {integer, integer}}))
      end}
    ].

validate_enum_test_() ->
    [
     {"enum",
      fun () ->
              Value    = durian,
              EnumList = [durian, eggplant, fig],

              ?assertEqual({ok, Value}, moyo_validator:validate(Value, {enum, EnumList}))
      end},

     {"enumに一致する値がない場合",
      fun () ->
              Input    = 'PC',
              EnumList = ['SmartPhone', 'STB', 'Game', 'TV'],

              ?assertMatch({error, _}, moyo_validator:validate(Input, {enum, EnumList}))
      end},

     {"enum, binary_in",
      fun () ->
              Input     = <<"erlang">>,
              Value     = erlang,
              EnumList  = [erlang, ruby, java],

              ?assertEqual({ok, Value}, moyo_validator:validate(Input, {enum, EnumList}, [{binary_in, existing_atom}]))
      end}
    ].

custom_spec_test_() ->
    [
     {"custom",
      fun () ->
              Value    = "Robot",
              CheckFun = fun (X) -> X =:= "Robot" end,

              ?assertEqual({ok, Value}, moyo_validator:validate(Value, {custom, CheckFun}))
      end},

     {"customで指定した関数でfalseが返る",
      fun () ->
              Value = 'string',
              IsStr = fun (X) -> is_list(X) end,

              ?assertMatch({error, _}, moyo_validator:validate(Value, {custom, IsStr}))
      end},

     {"customで指定した関数で例外が発生する",
      fun () ->
              Value = something,
              IsStr = fun (X) -> throw(X) end,

              ?assertMatch({error, _}, moyo_validator:validate(Value, {custom, IsStr}))
      end}
   ].

other_type_test_() ->
    [
     {"未定義の型を指定",
      fun () ->
              Value = beer,
              IllType = alcohol,

              ?assertMatch({error, _}, moyo_validator:validate(Value, IllType))
      end}
    ].

option_test_() ->
    [
     {"transformオプション",
      fun () ->
              String   = "987654321",
              Integer  = 987654321,
              StrToInt = fun (Str) -> list_to_integer(Str) end,

              ?assertEqual({ok, Integer},
                  moyo_validator:validate(String, integer, [{transform, StrToInt}]))
      end},

     {"transform関数内で例外が発生",
      fun () ->
              Binary   = <<"93265647">>,
              StrToInt = fun (Str) -> list_to_integer(Str) end,

              ?assertMatch({error, _},
                  moyo_validator:validate(Binary, integer, [{transform, StrToInt}]))
      end},

     {"binary_inの次に入力値が指定されていないオプションが来た時の処理",
      fun () ->
              Options = [binary_in, {transform, fun (X) -> X end}],

              ?assertMatch({error, _}, moyo_validator:validate(0, integer, Options))
      end},

     {"binary_in + {binary_in, Type}",
      fun () ->
              Options = [binary_in, {binary_in, existing_atom}],

              ?assertMatch({ok, ok}, moyo_validator:validate(<<"ok">>, atom, Options))
      end},

     {"未定義のオプションを実行",
      fun () ->
              Options = [undefined_option],

              ?assertMatch({error, _}, moyo_validator:validate(0, integer, Options))
      end},

     {"binary_inを使えない型でbinary_inを使用",
      fun () ->
              Value = <<"message">>,
              Spec = {enum, [message, server]},
              Options = [binary_in],

              ?assertMatch({error, _}, moyo_validator:validate(Value, Spec, Options))
      end},

     % 以下、ホワイトボックステスト
     {"未定義のオプションの入力値を取得する処理",
      fun () ->
              Options = [binary_in, undefined_option],

              ?assertMatch({error, _}, moyo_validator:validate(0, integer, Options))
      end},

     {"オプションの入力値として必要なデータ型が取得できるか",
      fun () ->
              % binary_inの入力値が取得するパスを無理やり通す
              Options = [binary_in, binary_in],

              ?assertMatch({error, _}, moyo_validator:validate(0, integer, Options))
      end}
    ].

validate_equal_test_() ->
    [
     {"入力値が指定の値に等しいかをチェックする",
      fun () ->
              Expected = 123,
              Value1 = 123,       % 等しい
              Value2 = 123.0,     % 等しい
              Value3 = <<"123">>, % 等しくない

              ?assertEqual({ok, Value1}, moyo_validator:validate(Value1, {equal, Expected})),
              ?assertMatch({error, _}, moyo_validator:validate(Value2, {equal, Expected})),
              ?assertMatch({error, _}, moyo_validator:validate(Value3, {equal, Expected}))
      end}
    ].

validate_or_test_() ->
    [
     {"入力値が最初の条件にマッチする",
      fun () ->
              Specs = [integer, atom],
              Value = 10,
              ?assertEqual({ok, Value}, moyo_validator:validate(Value, {'or', Specs}))
      end},
     {"入力値が最後の条件にマッチする",
      fun () ->
              Specs = [integer, atom, {equal, <<"abc">>}],
              Value = <<"abc">>,
              ?assertEqual({ok, Value}, moyo_validator:validate(Value, {'or', Specs}))
      end},
     {"入力値がいずれの条件にもマッチしない",
      fun () ->
              Specs = [integer, atom, {equal, <<"abc">>}],
              Value = self(),
              ?assertMatch({error, _}, moyo_validator:validate(Value, {'or', Specs}))
      end},
     {"条件リストが空の場合は、バリデーションに失敗",
      fun () ->
              Value = self(),
              ?assertMatch({error, _}, moyo_validator:validate(Value, {'or', []}))
      end}
    ].

validate_and_test_() ->
    [
     {"入力値が全ての条件にマッチする",
      fun () ->
              Specs = [integer, number],
              Value = 10,
              ?assertEqual({ok, Value}, moyo_validator:validate(Value, {'and', Specs}))
      end},
     {"入力値が一部でマッチしない",
      fun () ->
              Specs = [integer, atom, number],
              Value = 10,
              ?assertMatch({error, _}, moyo_validator:validate(Value, {'and', Specs}))
      end},
     {"条件リストが空の場合は、バリデーションに成功",
      fun () ->
              Value = self(),
              ?assertMatch({ok, Value}, moyo_validator:validate(Value, {'and', []}))
      end}
    ].

validate_not_test_() ->
    [
     {"入力値が条件に当てはまる",
      fun () ->
              Spec  = integer,
              Value = <<"bin">>,
              ?assertEqual({ok, Value}, moyo_validator:validate(Value, {'not', Spec}))
      end},
     {"入力値が条件に当てはまらない",
      fun () ->
              Spec  = integer,
              Value = 10,
              ?assertMatch({error, _}, moyo_validator:validate(Value, {'not', Spec}))
      end}
    ].

illigal_case_test_() ->
    [
     {"存在しないconstraintを指定",
      ?_assertMatch({error, _}, moyo_validator:validate(6, {integer, [kisu]}))}
    ].
