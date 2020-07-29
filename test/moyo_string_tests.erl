%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_stringモジュールのユニットテスト
-module(moyo_string_tests).

-include_lib("eunit/include/eunit.hrl").

to_string_test_() ->
    [
     {"バイナリが文字列に変換できる",
      fun () ->
              Input    = <<"hello world">>,
              Expected = "hello world",
              ?assertEqual(Expected, moyo_string:to_string(Input))
      end},
     {"アトムが文字列に変換できる",
      fun () ->
              Input    = hello_world,
              Expected = "hello_world",
              ?assertEqual(Expected, moyo_string:to_string(Input))
      end},
     {"整数が文字列に変換できる",
      fun () ->
              Input    = 1234,
              Expected = "1234",
              ?assertEqual(Expected, moyo_string:to_string(Input))
      end},
     {"浮動小数点数が文字列に変換できる",
      fun () ->
              Input    = 12.34,
              Expected = "1.23399999999999998579e+01",
              ?assertEqual(Expected, moyo_string:to_string(Input))
      end},
     {"PIDが文字列に変換できる",
      fun () ->
              Input    = c:pid(0,1,0),
              Expected = "<0.1.0>",
              ?assertEqual(Expected, moyo_string:to_string(Input))
      end},
     {"非負の整数値リストは文字列として扱われる",
      fun () ->
              ?assertEqual([1,2,3], moyo_string:to_string([1,2,3])),
              ?assertEqual([10000000], moyo_string:to_string([10000000]))
      end},
     {"負数が混じったリストは文字列として扱われない",
      fun () ->
              Input    = [1, -2, 3],
              Expected = "[1,-2,3]",
              ?assertEqual(Expected, moyo_string:to_string(Input))
      end},
     {"整数以外が混じったリストは文字列として扱われない",
      fun () ->
              Input    = [1, one, 3],
              Expected = "[1,one,3]",
              ?assertEqual(Expected, moyo_string:to_string(Input))
      end},
     {"複雑なデータ構造も文字列に変換可能",
      fun () ->
              Input1    = [{1,2,3}, c:pid(0,1,0), [[[[[<<"abc">>]], {hello}]]]],
              Expected1 = "[{1,2,3},<0.1.0>,[[[[[<<97,98,99>>]],{hello}]]]]",
              ?assertEqual(Expected1, moyo_string:to_string(Input1)),

              Input2    = {{1,2,3}, c:pid(0,1,0), [[[[[<<"abc">>]], {hello}]]]},
              Expected2 = "{{1,2,3},<0.1.0>,[[[[[<<97,98,99>>]],{hello}]]]}",
              ?assertEqual(Expected2, moyo_string:to_string(Input2))
      end},
     {"複雑なデータ構造をプリント表現に変換可能",
      fun() ->
              Input1    = [{1,2,3}, c:pid(0,1,0), [[[[[<<"abc">>]], {hello}]]]],
              Expected1 = "[{1,2,3},<0.1.0>,[[[[[<<\"abc\">>]],{hello}]]]]",
              ?assertEqual(Expected1, moyo_string:to_string(Input1, [print])),

              Input2    = {{1,2,3}, c:pid(0,1,0), [[[[[<<"abc">>]], {hello}]]]},
              Expected2 = "{{1,2,3},<0.1.0>,[[[[[<<\"abc\">>]],{hello}]]]}",
              ?assertEqual(Expected2, moyo_string:to_string(Input2, [print]))
      end},
     {"関数を文字列に変換",
      fun () ->
              Input = fun () -> ok end,
              ?assert(is_list(moyo_string:to_string(Input))) % 具体的な文字列表現は環境依存なのでテストしない
      end},
     {"ポートを文字列に変換",
      fun () ->
              Input = open_port("ls", []),
              ?assert(is_list(moyo_string:to_string(Input))) % 具体的な文字列表現は環境依存なのでテストしない
      end},
     {"リファレンスを文字列に変換",
      fun () ->
              Input = make_ref(),
              ?assert(is_list(moyo_string:to_string(Input))) % 具体的な文字列表現は環境依存なのでテストしない
      end},
     {"浮動小数点数が文字列に変換できる: 有効数字6桁の指数形式",
      fun () ->
              Input    = 12.34,
              Expected = "1.234000e+01",
              ?assertEqual(Expected, moyo_string:to_string(Input, [{float_format, [{scientific, 6}]}]))
      end},
     {"浮動小数点数が文字列に変換できる: 小数点以下6桁の非指数形式",
      fun () ->
              Input    = 12.34,
              Expected = "12.340000",
              ?assertEqual(Expected, moyo_string:to_string(Input, [{float_format, [{decimals, 6}]}]))
      end},
     {"浮動小数点数が文字列に変換できる: 小数点以下6桁未満(0は可能な限り省略)の非指数形式",
      fun () ->
              Input    = 12.34,
              Expected = "12.34",
              ?assertEqual(Expected, moyo_string:to_string(Input, [{float_format, [{decimals, 6},compact]}]))
      end},
     {"浮動小数点数用オプションを指定しても無視して文字列に変換できる",
      fun () ->
              Input    = {1, 2, 3},
              Expected = "{1,2,3}",
              ?assertEqual(Expected, moyo_string:to_string(Input, [{float_format, [{decimals, 6},compact]}]))
      end}
    ].

format_test_() ->
    [
     {"io_lib:format/2 の出力を文字列に変換",
      fun () ->
              Output = moyo_string:format("~p + ~p = ~p", [1 , 2, 1 + 2]),
              ?assertEqual("1 + 2 = 3", Output)
      end}
    ].

is_iolist_test_() ->
    [
     {"空リストは iolist() と判定される",
      fun () ->
              Input = [],
              ?assert(moyo_string:is_iolist(Input))
      end},
     {"ASCII文字列は iolist() と判定される",
      fun () ->
              Input = "test",
              ?assert(moyo_string:is_iolist(Input))
      end},
     {"バイナリは iolist() とは判定されない",
      fun () ->
              Input = <<"test">>,
              ?assert(not moyo_string:is_iolist(Input))
      end},
     {"バイナリを含むリストは iolist() と判定される",
      fun () ->
              Input = [<<"test">>, <<"test">>],
              ?assert(moyo_string:is_iolist(Input))
      end},
     {"末尾要素がバイナリの場合は、真リストではなくても良い",
      fun () ->
              Input = [<<"test">> | <<"test">>],
              ?assert(moyo_string:is_iolist(Input)),

              %% 末尾が文字だとダメ
              ?assert(not moyo_string:is_iolist([<<"test">> | $a]))
      end},
     {"文字列とバイナリの混合は許可される",
      fun () ->
              Input = [<<"test">>, "test"],
              ?assert(moyo_string:is_iolist(Input))
      end},
     {"iolist()は、iolist()を要素として持つことができる",
      fun () ->
              Input = [<<"test">>, ["test", [<<"test">>], "test" | <<"test">>], "test"],
              ?assert(moyo_string:is_iolist(Input))
      end},
     {"ユニコード文字列は iolist() とは判定されない",
      fun () ->
              Input = [28450,23383], % "漢字"
              ?assert(not moyo_string:is_iolist(Input))
      end},
     {"UTF-8文字列は iolist() と判定される",
      fun () ->
              Input = binary_to_list(unicode:characters_to_binary([28450,23383])), % "漢字"
              ?assert(moyo_string:is_iolist(Input))
      end}
    ].

is_iodatat_test_() ->
    %% NOTE: is_iodatat_test_/0 との差分のみ実施
    [
     {"バイナリは iodata() とは判定される",
      fun () ->
              Input = <<"test">>,
              ?assert(moyo_string:is_iodata(Input))
      end}
    ].

is_ascii_string_test_() ->
    [
     {"ASCII文字列かどうかを判定する",
      fun () ->
              ?assert(moyo_string:is_ascii_string("abc_123#!")),  % ASCII文字列
              ?assert(not moyo_string:is_ascii_string("日本語")), % UTF-8文字列 (非ASCII)
              ?assert(not moyo_string:is_ascii_string(123))       % 文字列ではない
      end}
    ].
