%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 外部コマンドの実行に関する処理を集めたユーティリティモジュール.
-module(moyo_command).

%% 想定しているコマンドのパターン
%% command
%% command option
%% command -o             (short option)
%% command -option        (long option)
%% command --option
%% command -option Value
%% command --option Value
%% command -option=Value
%% command --option=Value

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         execute/2,           execute/3,
         generate_command/2,  generate_command/3,
         escape_shell_arg/1,
         reduce_parameters/1, reduce_parameters/2
        ]).

-export_type([
              command/0, argument/0, option/0,
              stdout_hook_fun/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
%% execute/3の第3引数に渡せるオプションの中でopen_port/2に渡さないオプションリスト.
-define(NON_OPENPORT_OPTION, [escape_all, stdout, stderr, discard_stderr, timeout, close_function, stdout_hook_fun, nice, open_port_module]).

%% float() -> binary()のデフォルト表記法
-define(DEFAULT_FLOAT_NOTATION, {decimals, 4}).

-type iodata_or_atom()   :: iodata() | atom().
%% iodata(), または, atom().
-type command()          :: iodata_or_atom().
%% 外部プログラムの実行パス.
-type argument()         :: iodata() | integer() | {iodata(), [argument_option(), ...]} | option_element().
%% 外部プログラムに渡す引数.
-type option_element()   :: {option_character(), option_argument()}
                          | {option_character(), option_argument(), [argument_option()]}.
%% 外部プログラムに渡す引数の中でオプションとして指定するもの.
-type option_character() :: iodata_or_atom().
%% オプション文字.
-type option_argument()  :: iodata() | integer() | none.
%% オプションの引数部分.
-type argument_option()  :: long_option | equal | escape | float_option().
%% 引数に関するオプション.
-type float_option()     :: {decimals, Decimals :: 0..253} | {scientific, Decimals :: 0..249} | compact.
%% 小数パラメータに関するオプション.
-type option()           :: port_settings() | escape_all
                          | {stdout, destination_file_path() | stderr} | {stderr, destination_file_path() | stdout}
                          | discard_stderr
                          | {timeout, time()} | {nice, integer()} | {close_function, fun((port()) -> ok)}
                          | {stdout_hook_fun, {stdout_hook_fun(), term()}}
                          | {open_port_module, module()}.
%% execute/2, execute/3に指定できるオプション.
-type port_settings()    :: term(). % open_portに指定できるオプション.
-type time()             :: non_neg_integer().
%% timeoutに指定できる数字.
-type destination_file_path() :: binary().
%% 標準出力/エラー出力の出力先.
-type stdout_hook_fun()  :: fun((binary() | {eol | noeol, binary()}, term()) -> term() | binary()) |
                            fun((exit, term()) -> binary()).
%% 標準出力に対するフィルタ関数.

%% ===タイマ関連のタイプ===
-type timer_setting()    :: {timer_ref(), timer_key()} | none.
%% タイマに関する設定.
-type timer_ref()        :: reference().
%% タイマのリファレンス.
-type timer_key()        :: reference().
%% タイマを判別するID(キー).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc オプションリストからコマンド文字列(バイナリ)を生成する.
-spec generate_command(command(), [argument()]) -> binary().
generate_command(Command, ArgumentList) -> generate_command(Command, ArgumentList, []).

%% @doc オプションリストからコマンド文字列(バイナリ)を生成する.
%%
%% 【argument option】<br />
%% ● `long_option': long optionにする("--"でオプションを指定する.).<br />
%% ● `equal'      : オプション文字とオプション引数の間を"="で繋ぐ.<br />
%% ● `escape'     : オプション引数をシングルクォーテーションでエスケープする.<br />
%%
%% 【argument option (小数)】<br />
%% ● `{scientific, 0..253}' : 小数を指数表記で出力する.数字は有効桁数.<br />
%% ● `{decimals, 0..249}'   : 小数を実数表記で出力する.数字は有効桁数.<br />
%% ● `compact'              : 後端のゼロを省く.<br />
%%  (\*) デフォルトは`[{decimals, 4}]'<br />
%%  (\*) 表記方法が複数指定されている場合,最も後に指定された表記方法が採用される.<br />
%%
%% 【option】<br />
%% ● `escape_all'            : 全てのオプション引数をエスケープする.<br />
%% ● `{stdout, Destination}' : 標準出力を指定先のファイルに出力する.
%%                             Destinationには出力ファイル先を指定する.<br />
%% ● `{stderr, Destination}' : 標準エラー出力を指定先のファイルに出力する.
%%                             Destinationには出力ファイル先を指定する.<br />
%% ● `discard_stderr'        : 標準エラー出力を/dev/nullに捨てる.<br />
%% ● `{timeout, Time}'       : Time `ミリ秒' で処理が終わらなかった場合, タイムアウトする.<br />
%% ● `{close_function, Fun}' : timeoutオプションでタイムアウトした時の処理を明示的に指定する.<br />
%% ● `{stdout_hook_fun, {Fun, Init}}' : 標準出力をフィルタリングする.
%%                                      Initに初期値を, Funは2引数の関数で第1引数に`exit'が来た場合は`binary'を返す.
-spec generate_command(command(), [argument()], [option()]) -> binary().
generate_command(Command, ArgumentList, OptionList) ->
    %% escape_allオプション
    FlatArgList = case lists:member(escape_all, OptionList) of
                      true ->
                          lists:map(fun ({Arg, [Item|_] = OptList}) when is_atom(Item) ->
                                            flatten_option({Arg, [escape|OptList]});
                                        ({Char, Arg}) -> flatten_option({Char, Arg, [escape]});
                                        ({Char, Arg, OptList}) -> flatten_option({Char, Arg, [escape|OptList]});
                                        (Arg) -> flatten_option({Arg, [escape]})
                                    end, ArgumentList);
                      _    -> lists:map(fun flatten_option/1, ArgumentList)
                  end,
    CmdBin = iodata_and_atom_to_binary(Command),
    FullCmd = join_with_space_separator([CmdBin|FlatArgList]),

    %% stdout, stderr, discard_stderrとniceをハンドリングする.
    do_post_processing(FullCmd, OptionList).

%% @doc 外部コマンドを実行する.
-spec execute(command(), [argument()]) -> {{ok, Output} | {error, Reason}, FullCommandBinary} when
      Output            :: binary(),
      Reason            :: term(),
      FullCommandBinary :: binary().
execute(Command, ArgumentList) -> execute(Command, ArgumentList, []).

%% @doc 外部コマンドを実行する.
%%
%% オプション
%% generate_commandのオプション + open_portのオプション
-spec execute(command(), [argument()], [option()]) -> {{ok, Output} | {error, Reason}, FullCommandBinary} when
      Output            :: binary(),
      Reason            :: timeout | {exit_status, Status},
      Status            :: integer(),
      FullCommandBinary :: binary().
execute(Command, ArgumentList, OptionList) ->
    FullCmd = generate_command(Command, ArgumentList, OptionList),

    Parent = self(),
    OldTrapExit = process_flag(trap_exit, true),
    Child = spawn_link(fun () -> execute_impl(Parent, FullCmd, OptionList) end),

    %% 別プロセスから結果を取得.
    Result = receive_result(Child),

    process_flag(trap_exit, OldTrapExit),

    {Result, FullCmd}.

%% @doc シングルクォーテーションで両端を囲み、バイナリ中のシングルクォーテーションをエスケープする.
-spec escape_shell_arg(Binary::binary()) -> EscapedBinary::binary().
escape_shell_arg(Binary) -> escape_shell_arg_impl(Binary, []).

%% @doc パラメータリストを整理する.
%%
%% パラメータリストから`不必要な要素'を除く.
%% `不必要な要素'のdefaultは, `undefined'とする.
-spec reduce_parameters(Parameters::[any()]) -> ValidParameters::[any()].
reduce_parameters(Parameters) -> reduce_parameters(Parameters, [undefined]).

%% @doc パラメータリストを整理する.
%%
%% パラメータリストから不必要な要素を除く.
-spec reduce_parameters(Parameters::[any()], UnnecessaryParameters::[any()]) -> ValidParameters::[any()].
reduce_parameters(Parameters, UnnecessaryParameters) ->
    ExclusionParams = [undefined | UnnecessaryParameters],
    %% パラメータの並びに意味がある可能性があるので, 並びは変えない.
    lists:filter(fun (Param) -> lists:member(Param, ExclusionParams) =:= false end, Parameters).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc iodata(), または, atom()をbinary()に変換する.
-spec iodata_and_atom_to_binary(iodata_or_atom()) -> binary().
iodata_and_atom_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
iodata_and_atom_to_binary(Iodata)                  -> iolist_to_binary(Iodata).

%% @doc デフォルトの表記オプションに従ってfloat()をbinary()に変換する
-spec parse_float_to_binary(float()) -> binary().
parse_float_to_binary(Float) -> parse_float_to_binary(Float, []).

%% @doc float()をbinary()に変換する
-spec parse_float_to_binary(float(), [argument_option()]) -> binary().
parse_float_to_binary(Float, OptionList) ->
    {Notation, SigFigure, Compact} = fetch_float_notation(OptionList),
    FloatOption = case Compact of
        compact -> [{Notation, SigFigure}, Compact];
        none    -> [{Notation, SigFigure}]
    end,

    FloatBinary = float_to_binary(Float, FloatOption),
    case {Notation, Compact} of
        {scientific, compact} -> compactize_scientific_notation(FloatBinary);
        _                     -> FloatBinary
    end.

%% @doc オプションリストから表記方法, 有効数字, compactか否かを取り出す
-spec fetch_float_notation([argument_option()]) -> {N, S, C} when
    N :: scientific | decimals | none,
    S :: 0..253 | none,
    C :: compact | normal | none.
fetch_float_notation(OptionList) -> fetch_float_notation_impl([?DEFAULT_FLOAT_NOTATION| OptionList], {none, none, none}).

%% @doc オプションリストから表記方法, 有効数字, compactか否かを取り出す
-spec fetch_float_notation_impl([argument_option()], {N, S, C}) -> {N, S, C} when
    N :: scientific | decimals | none,
    S :: 0..253 | none,
    C :: compact | normal | none.
fetch_float_notation_impl([], NotationTuple) -> NotationTuple;
fetch_float_notation_impl([compact|OptionList], {Notation, SigFigure, _Compact}) ->
    fetch_float_notation_impl(OptionList, {Notation, SigFigure, compact});
fetch_float_notation_impl([{scientific, N}|OptionList], {_Notation, _SigFigure, Compact}) ->
    fetch_float_notation_impl(OptionList, {scientific, N, Compact});
fetch_float_notation_impl([{decimals, N}|OptionList], {_Notation, _SigFigure, Compact}) ->
    fetch_float_notation_impl(OptionList, {decimals, N, Compact});
fetch_float_notation_impl([_Option|OptionList], NotationTuple) ->
    fetch_float_notation_impl(OptionList, NotationTuple).

%% @doc scientific-notationのcompactモード
-spec compactize_scientific_notation(binary()) -> binary().
compactize_scientific_notation(FloatBinary) ->
    [NormVal, Exp] = binary:split(FloatBinary, <<"e">>),
    RoundedNormVal = cutoff_trailings(NormVal, $0),
    <<RoundedNormVal/binary, "e", Exp/binary>>.

%% @doc binary()の後端のバイトが指定されたバイト値でなくなるまで後端のバイトを削除する．
-spec cutoff_trailings(binary(), byte()) -> binary().
cutoff_trailings(Bin, Cutoff) ->
    ByteLength = byte_size(Bin),
    case binary:last(Bin) of
        Cutoff -> cutoff_trailings(binary:part(Bin, {0, ByteLength-1}), Cutoff);
        _      -> Bin
    end.

%% @doc 引数で用いられる値をbinary()に変換する.
-spec to_binary_for_argument(iodata_or_atom() | integer()) -> binary().
to_binary_for_argument(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
to_binary_for_argument(Float) when is_float(Float)       -> parse_float_to_binary(Float);
to_binary_for_argument(IodataOrAtom)                     -> iodata_and_atom_to_binary(IodataOrAtom).

%% @doc オプションのタプルをフラットなバイナリに変換.
-spec flatten_option(Option::argument()) -> binary().
%% {引数, オプション}のパターン
%% floatはOptionListの要素がatom以外のパターンもある
flatten_option({Argument, OptionList}) when is_float(Argument) ->
    Argument1 = parse_float_to_binary(Argument, OptionList),
    generate_option_argument(Argument1, OptionList);
flatten_option({Argument, [Item|_]=OptionList}) when is_atom(Item) or is_tuple(Item) ->
    Argument1 = to_binary_for_argument(Argument),
    generate_option_argument(Argument1, OptionList);

%% {オプション文字, オプション引数}のパターン
flatten_option({MaybeOptionCharacter, MaybeArgument}) ->
    flatten_option({MaybeOptionCharacter, MaybeArgument, []});

%% {オプション文字, オプション引数, オプション}のパターン
flatten_option({MaybeOptionCharacter, MaybeArgument, OptionList}) ->
    {OptionCharacter, Argument} = prepare_char_and_arg(MaybeOptionCharacter, MaybeArgument),

    Option = generate_option(OptionCharacter, OptionList),
    Argument1 = generate_option_argument(Argument, OptionList),

    generate_option_set(Option, Argument1, OptionList);

%% 引数のパターン
flatten_option(Argument) -> to_binary_for_argument(Argument).

%% @doc オプション文字とオプション引数を調節する.
-spec prepare_char_and_arg(MaybeOptionCharacter, MaybeArgument) -> {OptionCharacter, Argument} when
      MaybeOptionCharacter :: option_character(),
      MaybeArgument        :: option_argument(),
      OptionCharacter      :: binary(),
      Argument             :: binary() | none.
prepare_char_and_arg(OptionCharacter, Argument) ->
    OptCharBin = iodata_and_atom_to_binary(OptionCharacter),
    ReturnArgument = case Argument of
                         none -> none;
                         _    -> to_binary_for_argument(Argument)
                     end,
    {OptCharBin, ReturnArgument}.

%% @doc オプションを生成する("-option_char"の部分.).
-spec generate_option(OptionCharacter::binary(), [argument_option()]) -> binary() | none.
generate_option(OptionCharacter, OptionList) ->
    LongF = lists:member(long_option, OptionList),
    Symbol = case LongF of true -> <<"--">>; false -> <<"-">> end,

    <<Symbol/binary, OptionCharacter/binary>>.

%% @doc オプション引数を調整.
-spec generate_option_argument(Argument::binary() | none, [argument_option()]) -> binary() | none.
generate_option_argument(none,     _)     -> none;
generate_option_argument(Argument, OptionList) ->
    EscapeF = lists:member(escape, OptionList),
    case EscapeF of
        true -> escape_shell_arg(Argument);
        _    -> Argument
    end.

%% @doc 1つのオプション(文字+引数)の文字列(バイナリ)を生成.
-spec generate_option_set(Option, Argument, [argument_option()]) -> binary() when
      Option   :: binary(),
      Argument :: binary() | none.
generate_option_set(Option, none, _) -> Option;
generate_option_set(Option, Argument, OptionList) ->
    EqualF = lists:member(equal, OptionList),
    Separator = case EqualF of true -> <<"=">>; _ -> <<" ">> end,

    moyo_binary:join([Option, Argument], Separator).

%% @doc open_port/2実行時に必須のopen_portのオプションを追加する.
%%
%% ● exit_status: timeout, 実行終了を取得.<br />
%% ● binary:      出力をbinaryで取得.<br />
-spec prepare_option_for_open_port([option()]) -> [port_settings()].
prepare_option_for_open_port(OptionList) ->
    %% open_portで用いないオプションを削除.
    DeletedList = lists:foldl(fun (Key, List) -> proplists:delete(Key, List) end,
                              OptionList, ?NON_OPENPORT_OPTION),
    [exit_status, binary | DeletedList].

%% @doc 特殊なオプションを反映させる.
-spec do_post_processing(Command::binary(), [option()]) -> binary().
do_post_processing(Command, OptionList) ->
    %% redirect optionを処理する
    Command1 = lists:foldl(
                 fun ({stdout, stderr}, Acc) -> join_with_space_separator([Acc, <<"1>&2">>                 ]);
                     ({stdout, Dest},   Acc) -> join_with_space_separator([Acc, <<"1>">>,   Dest           ]);
                     ({stderr, stdout}, Acc) -> join_with_space_separator([Acc, <<"2>&1">>                 ]);
                     ({stderr, Dest},   Acc) -> join_with_space_separator([Acc, <<"2>">>,   Dest           ]);
                     (discard_stderr,   Acc) -> join_with_space_separator([Acc, <<"2>">>,   <<"/dev/null">>]);
                     (_,                Acc) -> Acc
                 end, Command, OptionList),

    %% niceオプション: niceを指定する.
    case proplists:lookup(nice, OptionList) of
        {nice, Nice} -> moyo_binary:join([<<"nice">>, <<"-n">>, integer_to_binary(Nice), Command1], <<" ">>);
        none         -> Command1
    end.

%% @doc 別プロセスで行う外部プログラム実行処理.
-spec execute_impl(Parent, Command, [port_settings()]) -> {Self, {ok, Result} | {error, Reason}} when
      Parent  :: pid(),
      Command :: binary(),
      Self    :: pid(),
      Result  :: binary(),
      Reason  :: timeout | {exit_status, Status},
      Status  :: integer().
execute_impl(Parent, Command, OptionList) ->
    OptionListForOpenPort = prepare_option_for_open_port(OptionList),

    %% 外部コマンドを実行.
    Module = proplists:get_value(open_port_module, OptionList, erlang),
    Port = Module:open_port({spawn, binary_to_list(Command)}, OptionListForOpenPort),

    Time = proplists:get_value(timeout, OptionList),
    TimerSetting = set_timer(Time),

    {StdoutHookFun, Init} = get_stdout_hook_fun(OptionList),

    %% 結果を取得.
    Result = receive_result_loop(Port, TimerSetting, StdoutHookFun, Init),

    %% 結果に応じて, ポートに対して処理を行う.
    _ = case Result of % dialyzerのためにアンダーバーでマッチングする.
            {error, timeout} -> close_execution_port(Port, OptionList); % = ok
            _ -> % {ok, Result} | {error, {exit_status, _}}
                %% タイマが既にキャンセルされている場合があり得るのでhandlingしない.
                cancel_timer(TimerSetting)
        end,
    Parent ! {self(), Result}.

%% @doc タイマをセットする.
%%
%% タイムアウト時間になるとKeyのメッセージが送られる.
-spec set_timer(Time::integer()|undefined) -> timer_setting().
set_timer(undefined) -> none;
set_timer(Time)      ->
    Key = make_ref(),
    Ref = erlang:send_after(Time, self(), Key),
    {Ref, Key}.

%% @doc タイマをキャンセルする.
-spec cancel_timer(timer_setting()) -> ok | {error, Reason::term()}.
cancel_timer(none)          -> ok;
cancel_timer({TimerRef, _}) ->
    case erlang:cancel_timer(TimerRef) of
        false -> {error, already_canceled};
        _     -> ok
    end.

%% @doc 標準出力の結果を取得する.
%%
%% open_portでの出力結果を取得する.
-spec receive_result_loop(port(), timer_setting(), stdout_hook_fun(), Init) -> {ok, Result} | {error, Reason} when
      Init   :: term(),
      Result :: binary(),
      Reason :: timeout | {exit_status, Status},
      Status :: integer().
receive_result_loop(Port, TimerSetting, HookFun, Init) ->
    TimerKey = get_timer_key(TimerSetting),
    receive_result_loop_impl(Port, TimerKey, HookFun, Init).

%% @doc receive_result_loop_implの内部関数.
-spec receive_result_loop_impl(port(), TimerKey, stdout_hook_fun(), Acc) -> {ok, Result} | {error, Reason} when
      TimerKey :: timer_key() | none,
      Acc      :: term(),
      Result   :: binary(),
      Reason   :: timeout | {exit_status, Status},
      Status   :: integer().
receive_result_loop_impl(Port, TimerKey, HookFun, Acc) ->
    receive
        {Port, {data, Data}}            -> receive_result_loop_impl(Port, TimerKey, HookFun, HookFun(Data, Acc));
        {Port, {exit_status, 0}}        -> {ok,    HookFun(exit, Acc)};
        {Port, {exit_status, Code}}     -> {error, {exit_status, Code}};
        TimerKey when TimerKey =/= none -> {error, timeout}
    end.

%% @doc タイマ設定からキーを取り出す.
-spec get_timer_key(timer_setting()) -> timer_key() | none.
get_timer_key(none)          -> none;
get_timer_key({_, TimerKey}) -> TimerKey.

%% @doc 外部プログラムを実行しているportを閉じる.
-spec close_execution_port(port(), [option()]) -> ok.
close_execution_port(Port, OptionList) ->
    _ = case proplists:get_value(close_function, OptionList) of
            undefined -> ok; %プロセス終了時にport_close()が呼ばれるので
            CloseFun -> CloseFun(Port)   % = ok
        end,
    ok.

%% @doc 別プロセスで実行された外部プログラムの結果を取得する.
-spec receive_result(pid()) -> {ok, Result} | {error, Reason} when
      Result :: binary(),
      Reason :: timeout | {exit_status, Status},
      Status :: integer().
receive_result(Pid) ->
    receive
        {Pid, Result} ->
            receive
                {'EXIT', Pid, normal} -> Result;
                {'EXIT', Pid, Why}    -> {error, Why}
            end;
        {'EXIT', Pid, Why} -> {error, Why}
    end.

%% @doc escape_shell_arg/1の内部処理.
-spec escape_shell_arg_impl(Binary1::binary(), Binary2::iodata()) -> EscapedBinary::binary().
escape_shell_arg_impl(<<"'",  Tail/binary>>, Iodata) -> escape_shell_arg_impl(Tail, [<<"'\\''">> | Iodata]);
escape_shell_arg_impl(<<Head, Tail/binary>>, Iodata) -> escape_shell_arg_impl(Tail, [Head | Iodata]);
escape_shell_arg_impl(<<>>,                  Iodata) -> list_to_binary([$', lists:reverse(Iodata), $']).

%% @doc binary listを` 'をseparatorとして繋げる.
-spec join_with_space_separator([binary()]) -> binary().
join_with_space_separator(BinaryList) ->
    moyo_binary:join(BinaryList, <<" ">>).

%% @doc 標準出力フィルターを取得する.
-spec get_stdout_hook_fun(OptionList) -> HookFun when
      OptionList :: [option()],
      HookFun    :: {stdout_hook_fun(), term()}.
get_stdout_hook_fun(OptionList) ->
    DefaultFun = case proplists:lookup(line, OptionList) of
                     none ->
                         {fun (exit, Acc) -> Acc;
                              (Data, Acc) -> <<Acc/binary, Data/binary>>
                          end, <<>>};
                     {line, _} ->
                         {fun (exit,          Acc) -> Acc;
                              ({eol,   Line}, Acc) -> <<Acc/binary, Line/binary, "\n">>;
                              ({noeol, Line}, Acc) -> <<Acc/binary, Line/binary>>
                          end, <<>>}
                 end,
    proplists:get_value(stdout_hook_fun, OptionList, DefaultFun).
