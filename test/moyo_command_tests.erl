%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_commandモジュールのユニットテスト
-module(moyo_command_tests).

-include_lib("eunit/include/eunit.hrl").

generate_command_test_() ->
    [
     {"オプションなしでコマンド文字列を作成(binary)",
      fun () ->
              Command = moyo_command:generate_command(<<"command">>,
                                                       [{<<"option_char">>, <<"option_arg">>}]),
              Expected = <<"command -option_char option_arg">>,

              ?assertEqual(Expected, Command)
      end},

     {"オプションなしでコマンド文字列を作成(string)",
      fun () ->
              Command = moyo_command:generate_command("command", [{"option_char", "option_arg"}]),
              Expected = <<"command -option_char option_arg">>,

              ?assertEqual(Expected, Command)
      end},

     {"オプションなしでコマンド文字列を作成(atom+binary)",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, <<"option_arg">>}]),
              Expected = <<"command -option_char option_arg">>,

              ?assertEqual(Expected, Command)
      end},

     {"引数なしでオプションを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, none}]),
              Expected = <<"command -option_char">>,

              ?assertEqual(Expected, Command)
      end},

     {"escape_allオプションを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, <<"option_arg">>}],
                                                       [escape_all]),
              Expected = <<"command -option_char 'option_arg'">>,

              ?assertEqual(Expected, Command)
      end},

     {"long_option",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, <<"option_arg">>,
                                                                  [long_option]}]),
              Expected = <<"command --option_char option_arg">>,

              ?assertEqual(Expected, Command)
      end},

     {"equal",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, <<"option_arg">>, [equal]}]),
              Expected = <<"command -option_char=option_arg">>,

              ?assertEqual(Expected, Command)
      end},

     {"escape",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, <<"option_arg">>,
                                                                  [escape]}]),
              Expected = <<"command -option_char 'option_arg'">>,

              ?assertEqual(Expected, Command)
      end},

     {"引数を指定",
      fun () ->
              Command = moyo_command:generate_command(command, [argument]),
              Expected = <<"command argument">>,

              ?assertEqual(Expected, Command)
      end},

     {"別の引数オプションがあるときにescape_allを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, option_arg,
                                                                  [long_option]}], [escape_all]),
              Expected = <<"command --option_char 'option_arg'">>,

              ?assertEqual(Expected, Command)
      end},

     {"複数引数があるときにescape_allを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char1, option_arg1,
                                                                  [long_option]},
                                                                 {option_char2, option_arg2}],
                                                                [escape_all]),
              Expected = <<"command --option_char1 'option_arg1' -option_char2 'option_arg2'">>,

              ?assertEqual(Expected, Command)
      end},

     {"オプションではない引数があるときにescape_all",
      fun () ->
              Command = moyo_command:generate_command(command, [argument], [escape_all]),
              Expected = <<"command 'argument'">>,

              ?assertEqual(Expected, Command)
      end},

     {"引数にescapeオプションが指定されているときにescape_allを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [{argument, [escape]}, {optchr, optarg}], [escape_all]),
              Expected = <<"command 'argument' -optchr 'optarg'">>,

              ?assertEqual(Expected, Command)
      end},

     {"引数にescapeオプションを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [{argument, [escape]}]),
              Expected = <<"command 'argument'">>,

              ?assertEqual(Expected, Command)
      end},

     {"argumentに2要素目が空リストの2要素のタプルをした時の挙動",
      fun () ->
              Command = moyo_command:generate_command(command, [{option_char, []}]),
              Expected = <<"command -option_char ">>,

              ?assertEqual(Expected, Command)
      end},

     {"引数にinteger()を指定する",
      fun () ->
              Command = moyo_command:generate_command(command, [0]),
              Expected = <<"command 0">>,

              ?assertEqual(Expected, Command)
      end},

     {"引数に負数を指定する",
      fun () ->
              Command = moyo_command:generate_command(command, [-1]),
              Expected = <<"command -1">>,

              ?assertEqual(Expected, Command)
      end},

     {"引数にinteger()とオプションを指定する",
      fun () ->
              Command = moyo_command:generate_command(command, [{1, [escape]}]),
              Expected = <<"command '1'">>,

              ?assertEqual(Expected, Command)
      end},

     {"オプションの引数にfloat()とオプションを指定する",
      fun () ->
              Command = moyo_command:generate_command(command, [{num, 1}]),
              Expected = <<"command -num 1">>,

              ?assertEqual(Expected, Command)
      end},

     {"niceオプションを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [argument, {optchr, optarg}], [{nice, -5}]),
              Expected = <<"nice -n -5 command argument -optchr optarg">>,

              ?assertEqual(Expected, Command)
      end},

     {"stdoutオプションを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [argument, {optchr, optarg}],
                                                      [{stdout, <<"/tmp/stdout_option.txt">>}]),
              Expected = <<"command argument -optchr optarg 1> /tmp/stdout_option.txt">>,
              ?assertEqual(Expected, Command)
      end},

     {"stdoutオプションを指定(stderr)",
      fun () ->
              Command = moyo_command:generate_command(command, [argument, {optchr, optarg}], [{stdout, stderr}]),
              Expected = <<"command argument -optchr optarg 1>&2">>,
              ?assertEqual(Expected, Command)
      end},

     {"stderrオプションを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [argument, {optchr, optarg}],
                                                      [{stderr, <<"/tmp/stderr_option.txt">>}]),
              Expected = <<"command argument -optchr optarg 2> /tmp/stderr_option.txt">>,
              ?assertEqual(Expected, Command)
      end},

     {"stderrオプションを指定(stdout)",
      fun () ->
              Command = moyo_command:generate_command(command, [argument, {optchr, optarg}], [{stderr, stdout}]),
              Expected = <<"command argument -optchr optarg 2>&1">>,
              ?assertEqual(Expected, Command)
      end},

     {"discard_stderrオプションを指定",
      fun () ->
              Command = moyo_command:generate_command(command, [argument, {optchr, optarg}], [discard_stderr]),
              Expected = <<"command argument -optchr optarg 2> /dev/null">>,
              ?assertEqual(Expected, Command)
      end}
    ].

execute_test_() ->
    [
     {"echoコマンドを実行",
      fun () ->
              Result = moyo_command:execute("echo", ["test"]),
              Expected = {{ok, <<"test\n">>}, <<"echo test">>},

              ?assertEqual(Expected, Result)
      end},

     {"timeout設定時に処理が成功",
      fun () ->
              Result = moyo_command:execute("echo", ["timeout"], [{timeout, 10}]),
              Expected = {{ok, <<"timeout\n">>}, <<"echo timeout">>},

              ?assertEqual(Expected, Result)
      end},

     {"sleepコマンドでtimeout",
      fun () ->
              Result = moyo_command:execute("sleep", ["1s"], [{timeout, 10}]),

              ?assertMatch({{error, timeout}, <<"sleep 1s">>}, Result)
      end},

     {"exit_statusで0以外で終了(異常終了)した時の挙動",
      fun () ->
              Result = moyo_command:execute("sh", ["../test/testdata/moyo_command/exit_1.sh"]),

              ?assertMatch({{error, {exit_status, 1}}, <<"sh ../test/testdata/moyo_command/exit_1.sh">>}, Result)
      end},

     {"timeout時の外部プログラムの終了方法を指定",
      fun () ->
              CloseFun = fun (Port) -> port_close(Port) end,
              Result = moyo_command:execute("sleep", ["1s"], [{timeout, 10}, {close_function, CloseFun}]),

              ?assertEqual({{error, timeout}, <<"sleep 1s">>}, Result)
      end},

     {"標準エラー出力を捨てる",
      fun () ->
              Result = moyo_command:execute("sh",
                           ["../test/testdata/moyo_command/stderr.sh"], [discard_stderr]),
              Expected = {{ok, <<>>}, <<"sh ../test/testdata/moyo_command/stderr.sh 2> /dev/null">>},

              ?assertEqual(Expected, Result)
      end},

     {"stderr_to_stdoutとdiscard_stderr",
      fun () ->
              Result = moyo_command:execute("sh",
                           ["../test/testdata/moyo_command/stderr.sh"], [discard_stderr, stderr_to_stdout]),
              Expected = {{ok, <<>>}, <<"sh ../test/testdata/moyo_command/stderr.sh 2> /dev/null">>},

              ?assertEqual(Expected, Result)
      end},

     %% テスト中に標準エラー出力が出てしまうのでコメントアウト
     %% {"例外: discard_stderrを付けても標準エラー出力が出る場合(要修正)",
     %%  fun () ->
     %%          %% TODO: リダイレクト機能実装後修正する.
     %%          Result = moyo_command:execute("echo", ["stderr", "1>&2"], [discard_stderr]),
     %%          Expected = {{ok, <<>>}, <<"echo stderr 1>&2 2> /dev/null">>},

     %%          ?assertEqual(Expected, Result)
     %%  end},

     {"例外: discard_stderrを付けても標準エラー出力がなくならず, 標準出力として受け取ることができる",
      fun () ->
              Result = moyo_command:execute("echo", ["stderr"], [{stdout, stderr}, discard_stderr, stderr_to_stdout]),
              Expected = {{ok, <<"stderr\n">>}, <<"echo stderr 1>&2 2> /dev/null">>},

              %% 標準出力が/dev/nullにリダイレクトされない.
              ?assertEqual(Expected, Result)
      end},

     {"コマンドの実行がタイムアウトした場合でも、ポートは自動的に閉じられる(リークしない)",
      fun () ->
              %% ポート取得用の関数
              Self = self(),
              CloseFun =
                  fun (Port) ->
                          ?assert(undefined =/= erlang:port_info(Port)), % この時点ではまだ開いているはず
                          Self ! {command_port, Port}
                  end,

              %% 確実にタイムアウトするような時間を指定して実行
              Result = moyo_command:execute("sleep", ["5s"],
                                            [{timeout, 1}, {close_function, CloseFun}]),
              ?assertEqual({{error, timeout}, <<"sleep 5s">>}, Result),
              receive
                  {command_port, Port} ->
                      %% moyo_command:execute/3 の実行の終了に合わせて Port も自動でクローズされているはず
                      ?assertEqual(undefined, erlang:port_info(Port))
              end

      end},

     {"open_portに対してlineオプションを付けても結果に問題がないことを確認する",
      fun () ->
              Result = moyo_command:execute("echo", ["test"], [{line, 1}]),

              Expected = {{ok, <<"test\n">>}, <<"echo test">>},
              ?assertEqual(Expected, Result)
      end},

     {"標準出力の行ごとにヘッダを付ける",
      fun () ->
              %% 各行の先頭に"output: "というヘッダを付けるフィルタ
              Fold = {fun ({eol, Line}, {Acc, eol}) ->
                              {<<Acc/binary, "output: ", Line/binary, "\n">>, eol};
                          ({noeol, Line}, {Acc, eol}) ->
                              {<<Acc/binary, "output: ", Line/binary>>, noeol};
                          ({eol, Line}, {Acc, noeol}) ->
                              {<<Acc/binary, Line/binary, "\n">>, eol};
                          ({noeol, Line}, {Acc, noeol}) ->
                              {<<Acc/binary, Line/binary>>, noeol};
                          (exit, {Acc, _}) ->
                              Acc
                      end, {<<>>, eol}},

              Result = moyo_command:execute("echo", ["\"line1\nline2\n\""], [{line, 1}, {stdout_hook_fun, Fold}]),

              Expected = {{ok, <<"output: line1\noutput: line2\noutput: \n">>}, <<"echo \"line1\nline2\n\"">>},
              io:format("~p~n", [Result]),
              ?assertEqual(Expected, Result)
      end},

     {"標準出力で必要のない行を除く",
      fun () ->
              %% alertの文字を含む行のみ残すfilter
              Fold = {fun ({eol, Line}, {Acc, Rest}) ->
                              LineAll = <<Rest/binary, Line/binary, "\n">>,
                              case binary:match(LineAll, <<"alert">>) of
                                  nomatch -> {Acc, <<>>};
                                  _       -> {<<Acc/binary, LineAll/binary>>, <<>>}
                              end;
                          ({noeol, Line}, {Acc, Rest}) ->
                              {Acc, <<Rest/binary, Line/binary>>};
                          (exit, {Acc, Rest}) ->
                              <<Acc/binary, Rest/binary>>
                      end, {<<>>, <<>>}},

              Result = moyo_command:execute("echo", ["\"alert1\ninfo\nalert2\nnotice\nalert3\n\""],
                                            [{line, 1}, {stdout_hook_fun, Fold}]),

              Expected = {{ok, <<"alert1\nalert2\nalert3\n">>}, <<"echo \"alert1\ninfo\nalert2\nnotice\nalert3\n\"">>},
              ?assertEqual(Expected, Result)

      end}
    ].

escape_shell_arg_test_() ->
    [
     {"シングルクォーテーション入りのバイナリをエスケープする",
      fun () ->
              Binary = <<"1234567890-=\\`[];',./!@#$%^&*()_+|~{}:\"<>?">>,
              Expected = <<"'1234567890-=\\`[];'\\'',./!@#$%^&*()_+|~{}:\"<>?'">>,

              Escaped = moyo_command:escape_shell_arg(Binary),
              ?assertEqual(Expected, Escaped)
      end}
    ].

reduce_parameters_test_() ->
    [
     {"undefinedを省けるかテスト",
      fun () ->
              Params = [atom, undefined, {tuple, tuple}, {undefined}, "string", "undefined", <<"binary">>,
                        <<"undefined">>, 0123],
              Result = moyo_command:reduce_parameters(Params),
              Expected =
                  [atom, {tuple, tuple}, {undefined}, "string", "undefined", <<"binary">>, <<"undefined">>, 0123],

              ?assertEqual(Expected, Result)
      end},

     {"指定した要素を削除できるかテスト",
      fun () ->
              Params = [atom, undefined, {tuple, tuple}, {undefined}, "string", "", "undefined", <<"binary">>,
                        <<"">>, <<"undefined">>, 0123],
              ExParams = [undefined, "", <<"">>],
              Result = moyo_command:reduce_parameters(Params, ExParams),
              Expected =
                  [atom, {tuple, tuple}, {undefined}, "string", "undefined", <<"binary">>, <<"undefined">>, 0123],

              ?assertEqual(Expected, Result)
      end}
    ].
