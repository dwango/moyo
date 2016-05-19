%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_binaryモジュールのユニットテスト
-module(moyo_binary_tests).

-include_lib("eunit/include/eunit.hrl").

to_hex_test_() ->
    [
     {"生のバイナリを16進数表記のバイナリに変換する",
      fun () ->
              Input1    = <<"ab_YZ">>,
              Expected1 = <<"61625f595a">>,
              ?assertMatch(Expected1, moyo_binary:to_hex(Input1)),

              Input2    = <<145,111,31,119,9,93,72,40,107,121>>,
              Expected2 = <<"916f1f77095d48286b79">>,
              ?assertMatch(Expected2, moyo_binary:to_hex(Input2))
      end}
    ].

from_hex_test_() ->
    [
     {"16進数表記のバイナリを生のバイナリに変換する",
      fun () ->
              Input1    = <<"61625f595a">>,
              Expected1 = <<"ab_YZ">>,
              ?assertMatch(Expected1, moyo_binary:from_hex(Input1)),

              Input2    = <<"916f1f77095d48286b79">>,
              Expected2 = <<145,111,31,119,9,93,72,40,107,121>>,
              ?assertMatch(Expected2, moyo_binary:from_hex(Input2)),

              %% 奇数桁でも大丈夫
              Input3    = <<"777">>,
              Expected3 = <<16#7, 16#77>>,
              ?assertMatch(Expected3, moyo_binary:from_hex(Input3)),

              Input4    = <<>>,
              Expected4 = <<>>,
              ?assertMatch(Expected4, moyo_binary:from_hex(Input4)),

              ?assertError({invalid_hex_binary, _}, moyo_binary:from_hex(<<"Other than hex char">>))
      end}
    ].

try_binary_to_existing_atom_test_() ->
    [
     {"バイナリを既に存在するアトムに変換する",
      fun () ->
              ExistingAtom = erlang,
              Input = <<"erlang">>,
              ?assertEqual(ExistingAtom, moyo_binary:try_binary_to_existing_atom(Input, utf8))
      end},
     {"存在しないアトムに変換しようとすると、入力バイナリがそのまま返される",
      fun () ->
              Input = <<"__erlay_lisp_ruby_scala_java__">>, % 多分、これに対応するアトムは存在しない
              ?assertEqual(Input, moyo_binary:try_binary_to_existing_atom(Input, utf8))
      end}
    ].

format_test_() ->
    [
     {"io_lib:format/2 のバイナリ版",
      fun () ->
              Output = moyo_binary:format("~p + ~p = ~p", [1, 2, 1 + 2]),
              ?assertEqual(<<"1 + 2 = 3">>, Output)
      end}
    ].

generate_random_list_test_() ->
    [
     {"ランダム かつ ユニーク なバイナリセットを生成する",
      fun () ->
              ByteSize = 10,
              Count    = 100,
              BinList  = moyo_binary:generate_random_list(ByteSize, Count),

              %% 指定サイズ通りになっているか
              ?assert(lists:all(fun (Bin) -> byte_size(Bin) =:= ByteSize end, BinList)),

              %% 要素数が指定通り かつ ユニーク か
              ?assertEqual(Count, length(BinList)),
              ?assertEqual(Count, length(lists:usort(BinList)))
      end},
     {"ランダム かつ ユニーク なバイナリセットを生成する: 衝突する確立が大きい場合",
      fun () ->
              ByteSize = 2,
              Count    = 500,
              BinList  = moyo_binary:generate_random_list(ByteSize, Count),

              %% 指定サイズ通りになっているか
              ?assert(lists:all(fun (Bin) -> byte_size(Bin) =:= ByteSize end, BinList)),

              %% 要素数が指定通り かつ ユニーク か
              ?assertEqual(Count, length(BinList)),
              ?assertEqual(Count, length(lists:usort(BinList)))
      end}
    ].

strip_test_() ->
    [
     {"1引数で両側のスペースを取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"  abc ">>),

              ?assertEqual(Stripped, <<"abc">>)
      end},
     {"2引数で左側のスペースを取り除く",
      fun () ->
              % 両側を取り除かないかcheckするために右側にもスペース
              Stripped = moyo_binary:strip(<<"  def ">>, left),

              ?assertEqual(Stripped, <<"def ">>)
      end},
     {"2引数で右側のスペースを取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"  ghi  ">>, right),

              ?assertEqual(Stripped, <<"  ghi">>)
      end},
     {"2引数で両側のスペースを取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<" jkl  ">>, both),

              ?assertEqual(Stripped, <<"jkl">>)
      end},
     {"3引数で左側の任意の1文字を取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"mmnom">>, left, <<"m">>),

              ?assertEqual(Stripped, <<"nom">>)
      end},
     {"3引数で右側の任意の1文字を取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"ppqrp">>, right, <<"p">>),

              ?assertEqual(Stripped, <<"ppqr">>)
      end},
     {"3引数で両側の任意の1文字を取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"ssstuss">>, both, <<"s">>),

              ?assertEqual(Stripped, <<"tu">>)
      end},
     {"4引数で順序に従い文字列を取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"ababbabcabcabbab">>, both, <<"ab">>, order),

              ?assertEqual(Stripped, <<"babcabcabb">>)
      end},
     {"4引数で順不同で文字列を取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"ababbabcabcabbab">>, both, <<"ab">>, random),

              ?assertEqual(Stripped, <<"cabc">>)
      end},
     {"エスケープシーケンスを取り除く",
      fun () ->
              Stripped = moyo_binary:strip(<<"abcdefg\r\nhijklmn\r\n">>, right, <<"\r\n">>, order),

              ?assertEqual(Stripped, <<"abcdefg\r\nhijklmn">>)
      end}
    ].

abbreviate_test_() ->
    [
     {"指定長を超えた部分は省略される",
      fun () ->
              Input = <<"hello world">>,
              Expexted = <<"hello...">>,
              ?assertEqual(Expexted, moyo_binary:abbreviate(Input, 8, <<"...">>))
      end},
     {"デフォルトの省略文字列は<<\"...\">>",
      fun () ->
              Input = <<"hello world">>,
              Expexted = <<"hello...">>,
              ?assertEqual(Expexted, moyo_binary:abbreviate(Input, 8))
      end},
     {"入力バイナリの長さが、指定長以下の場合は、省略されない",
      fun () ->
              Input = <<"hello world">>,
              Expexted = Input,
              ?assertEqual(Expexted, moyo_binary:abbreviate(Input, 11, <<"...">>))
      end},
     {"指定長よりも省略文字列の方が長く、かつ、省略が適用される場合は、省略文字列がそのまま返る (指定長に合わせて切り詰められはしない)",
      fun () ->
              Input = <<"man">>,
              Expexted = <<"......">>,
              ?assertEqual(Expexted, moyo_binary:abbreviate(Input, 1, <<"......">>))
      end}
    ].

to_float_test_() ->
    [
     {"整数表現のバイナリを浮動小数点数に変換する",
      fun () ->
              Input  = <<"1234">>,
              Result = moyo_binary:to_float(Input),
              ?assert(is_float(Result)),
              ?assertEqual(1234.0, Result)
      end},
     {"小数表現のバイナリを浮動小数点数に変換する",
      fun () ->
              Input  = <<"1234.5678">>,
              Result = moyo_binary:to_float(Input),
              ?assertEqual(1234.5678, Result)
      end},
     {"整数／小数以外の場合は例外が発生する",
      fun () ->
              Input = <<"abc">>,
              ?assertError(badarg, moyo_binary:to_float(Input))
      end}
    ].

to_number_test_() ->
    [
     {"整数表現のバイナリは整数に変換する",
      fun () ->
              Input  = <<"1234">>,
              Result = moyo_binary:to_number(Input),
              Expected = 1234,
              ?assertEqual(Expected, Result)
      end},
     {"小数表現のバイナリは浮動小数点数に変換する",
      fun () ->
              Input  = <<"1234.5678">>,
              Result = moyo_binary:to_number(Input),
              Expected = 1234.5678,
              ?assertEqual(Expected, Result)
      end},
     {"整数表現、小数表現のいずれでもなかった場合は badarg を投げる",
      fun () ->
              Input = <<"abc">>,
              ?assertError(badarg, moyo_binary:to_number(Input))
      end}
    ].

to_binary_test_() ->
    [
     {"バイナリを入力に渡した場合は、そのままの値が返る",
      fun () ->
               Input    = <<"hello world">>,
               Expected = <<"hello world">>,
               ?assertEqual(Expected, moyo_binary:to_binary(Input))
       end},
      {"アトムがバイナリに変換できる",
       fun () ->
               Input    = hello_world,
               Expected = <<"hello_world">>,
               ?assertEqual(Expected, moyo_binary:to_binary(Input))
       end},
      {"整数がバイナリに変換できる",
       fun () ->
               Input    = 1234,
               Expected = <<"1234">>,
               ?assertEqual(Expected, moyo_binary:to_binary(Input))
       end},
      {"浮動小数点数がバイナリに変換できる",
       fun () ->
               Input    = 12.34,
               Expected = <<"1.23399999999999998579e+01">>,
               ?assertEqual(Expected, moyo_binary:to_binary(Input))
       end},
      {"PIDがバイナリに変換できる",
       fun () ->
               Input    = c:pid(0,1,0),
               Expected = <<"<0.1.0>">>,
               ?assertEqual(Expected, moyo_binary:to_binary(Input))
      end},
     {"複雑なデータ構造もバイナリに変換可能",
      fun () ->
              Input1    = [{1,2,3}, c:pid(0,1,0), [[[[[<<"abc">>]], {hello}]]]],
              Expected1 = <<"[{1,2,3},<0.1.0>,[[[[[<<97,98,99>>]],{hello}]]]]">>,
              ?assertEqual(Expected1, moyo_binary:to_binary(Input1)),

              Input2    = {{1,2,3}, c:pid(0,1,0), [[[[[<<"abc">>]], {hello}]]]},
              Expected2 = <<"{{1,2,3},<0.1.0>,[[[[[<<97,98,99>>]],{hello}]]]}">>,
              ?assertEqual(Expected2, moyo_binary:to_binary(Input2))
      end},
     {"関数を文字列に変換",
      fun () ->
              Input = fun () -> ok end,
              ?assert(is_binary(moyo_binary:to_binary(Input))) % 具体的な文字列表現は環境依存なのでテストしない
      end},
     {"ポートを文字列に変換",
      fun () ->
              Input = open_port("ls", []),
              ?assert(is_binary(moyo_binary:to_binary(Input))) % 具体的な文字列表現は環境依存なのでテストしない
      end},
     {"リファレンスを文字列に変換",
      fun () ->
              Input = make_ref(),
              ?assert(is_binary(moyo_binary:to_binary(Input))) % 具体的な文字列表現は環境依存なのでテストしない
      end}
    ].

fill_test_() ->
    [
     {"10個の0が続くbinaryを作成",
      fun () ->
              Expected = <<0,0,0,0,0,0,0,0,0,0>>,
              ?assertEqual(Expected, moyo_binary:fill(0, 10))
      end},
     {"12個のaが続くbinaryを作成",
      fun () ->
              Expected = <<"aaaaaaaaaaaa">>,
              ?assertEqual(Expected, moyo_binary:fill($a, 12))
      end},
     {"0個を指定した場合",
      fun () ->
              Expected = <<>>,
              ?assertEqual(Expected, moyo_binary:fill(100, 0))
      end}
    ].

join_test_() ->
    [
     {"適当なバイナリリストをjoin",
      fun () ->
              Result = moyo_binary:join([<<"a">>, <<"b">>, <<"c">>], <<", ">>),
              Expected = <<"a, b, c">>,

              ?assertEqual(Expected, Result)
      end},

     {"1要素のバイナリリストをjoin",
      fun () ->
              Result = moyo_binary:join([<<"one">>], <<", ">>),
              Expected = <<"one">>,

              ?assertEqual(Expected, Result)
      end},

     {"0要素のバイナリリストをjoin",
      fun () ->
              Result = moyo_binary:join([], <<", ">>),
              Expected = <<>>,

              ?assertEqual(Expected, Result)
      end},

     {"空セパレータでjoin",
      fun () ->
              Result = moyo_binary:join([<<"ni">>, <<"co">>, <<"nic">>, <<"o">>], <<>>),
              Expected = <<"niconico">>,

              ?assertEqual(Expected, Result)
      end},

     {"0要素のバイナリリストを空セパレータでjoin",
      fun () ->
              Result = moyo_binary:join([], <<>>),
              Expected = <<>>,

              ?assertEqual(Expected, Result)
      end}
    ].

divide_test_() ->
    [
     {"iodata を 0/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(0, [$a, "b", <<"c">> | <<"d">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<>>, <<"abcd">> },

              ?assertEqual(Expected, Result)
      end},

     {"iodata を 1/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(1, [$a, "b", <<"c">> | <<"d">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"a">>, <<"bcd">> },

              ?assertEqual(Expected, Result)
      end},

     {"iodata を 2/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(2, [$a, "b", <<"c">> | <<"d">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"ab">>, <<"cd">> },

              ?assertEqual(Expected, Result)
      end},

     {"iodata を 3/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(3, [$a, "b", <<"c">> | <<"d">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"abc">>, <<"d">> },

              ?assertEqual(Expected, Result)
      end},

     {"iodata を 4/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(4, [$a, "b", <<"c">> | <<"d">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"abcd">>, <<>> },

              ?assertEqual(Expected, Result)
      end},

     {"iodata を 5/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(5, [$a, "b", <<"c">> | <<"d">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"abcd">>, <<>> },

              ?assertEqual(Expected, Result)
      end},

     {"binary を 0/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(0, <<"abcd">>),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<>>, <<"abcd">> },

              ?assertEqual(Expected, Result)
      end},

     {"binary を 1/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(1, <<"abcd">>),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"a">>, <<"bcd">> },

              ?assertEqual(Expected, Result)
      end},

     {"binary を 2/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(2, <<"abcd">>),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"ab">>, <<"cd">> },

              ?assertEqual(Expected, Result)
      end},

     {"binary を 3/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(3, <<"abcd">>),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"abc">>, <<"d">> },

              ?assertEqual(Expected, Result)
      end},

     {"binary を 4/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(4, <<"abcd">>),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"abcd">>, <<>> },

              ?assertEqual(Expected, Result)
      end},

     {"binary を 5/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(5, <<"abcd">>),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"abcd">>, <<>> },

              ?assertEqual(Expected, Result)
      end},

     {"[binary] を 5/4 で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(5, [<<"abcd">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"abcd">>, <<>> },

              ?assertEqual(Expected, Result)
      end},

     {"ネストした iodata の途中で divide",
      fun () ->
              {First, Second} = moyo_binary:divide(2, [<<"a">>, [<<"b">>, <<"c">>], <<"d">>]),
              %% 返ってくる値は実装依存で形式が変化しうるので単体テストではバイナリ化して正規化
              Result = { iolist_to_binary(First), iolist_to_binary(Second) },
              Expected = { <<"ab">>, <<"cd">> },

              ?assertEqual(Expected, Result)
      end}
    ].

fixed_point_binary_to_number_test_() ->
    [
     {"固定小数点16.16",
      fun() ->
              ?assertEqual(1.5, moyo_binary:fixed_point_binary_to_number(16, 16, <<0, 1, 128, 0>>))
      end},
     {"固定小数点32.32",
      fun() ->
              ?assertEqual(1.5, moyo_binary:fixed_point_binary_to_number(32, 32, <<0, 0, 0, 1, 128, 0, 0, 0>>))
      end}
    ].

number_to_fixed_point_binary_test_() ->
    [
     {"固定小数点16.16",
      fun() ->
              ?assertEqual(<<0, 1, 128, 0>>, moyo_binary:number_to_fixed_point_binary(16, 16, 1.5))
      end},
     {"固定小数点32.32.",
      fun() ->
              ?assertEqual(<<0, 0, 0, 1, 128, 0, 0, 0>>,
                           moyo_binary:number_to_fixed_point_binary(32, 32, 1.5))
      end}
    ].

from_integer_test_() ->
    [
        {"大文字",
            fun() ->
                ?assertEqual(<<"G">>, moyo_binary:from_integer(16, 36, uppercase)),
                ?assertEqual(<<"1111">>, moyo_binary:from_integer(15, 2, uppercase)),
                ?assertEqual(<<"-A">>, moyo_binary:from_integer(-10, 16, uppercase))
            end},
        {"小文字",
            fun() ->
                ?assertEqual(<<"g">>, moyo_binary:from_integer(16, 36, lowercase)),
                ?assertEqual(<<"1111">>, moyo_binary:from_integer(15, 2, lowercase)),
                ?assertEqual(<<"-a">>, moyo_binary:from_integer(-10, 16, lowercase))
            end}
    ].

to_lower_test_() ->
    [
     {"ASCII文字列のアルファベット",
      fun() ->
              ?assertEqual(<<"string">>, moyo_binary:to_lower(<<"StRiNg">>)),
              ?assertEqual(<<"string">>, moyo_binary:to_lower(<<"STRING">>)),
              ?assertEqual(<<"string">>, moyo_binary:to_lower(<<"string">>))
      end},
     {"ASCII文字列のアルファベット以外",
      fun() ->
              ?assertEqual(<<"  \n\n">>, moyo_binary:to_lower(<<"  \n\n">>)),
              ?assertEqual(<<"0123-+-+">>, moyo_binary:to_lower(<<"0123-+-+">>))
      end},
     {"UTF-8文字列",
      fun() ->
              ?assertEqual(<<"もよ"/utf8>>, moyo_binary:to_lower(<<"もよ"/utf8>>)),
              ?assertEqual(<<"ĄĆŁŹ"/utf8>>, moyo_binary:to_lower(<<"ĄĆŁŹ"/utf8>>))
      end}
    ].

to_upper_test_() ->
    [
     {"ASCII文字列のアルファベット",
      fun() ->
              ?assertEqual(<<"STRING">>, moyo_binary:to_upper(<<"StRiNg">>)),
              ?assertEqual(<<"STRING">>, moyo_binary:to_upper(<<"STRING">>)),
              ?assertEqual(<<"STRING">>, moyo_binary:to_upper(<<"string">>))
      end},
     {"ASCII文字列のアルファベット以外",
      fun() ->
              ?assertEqual(<<"  \n\n">>, moyo_binary:to_upper(<<"  \n\n">>)),
              ?assertEqual(<<"0123-+-+">>, moyo_binary:to_upper(<<"0123-+-+">>))
      end},
     {"UTF-8文字列",
      fun() ->
              ?assertEqual(<<"もよ"/utf8>>, moyo_binary:to_upper(<<"もよ"/utf8>>)),
              ?assertEqual(<<"ĄĆŁŹ"/utf8>>, moyo_binary:to_upper(<<"ĄĆŁŹ"/utf8>>))
      end}
    ].
