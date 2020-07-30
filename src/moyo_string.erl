%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 文字列(整数値のリスト)に関する処理を集めたユーティリティモジュール.
-module(moyo_string).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         to_string/1,
         to_string/2,

         format/2,

         is_ascii_string/1,
         is_iodata/1,
         is_iolist/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Types
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              encode_option/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type float_format_option() :: {scientific, Decimals :: 0..249}
                             | {decimals, Decimals :: 0..253}
                             | compact.

-type encode_option() :: print | {float_format, [float_format_option()]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @equiv to_string(V, [])
to_string(V) ->
    to_string(V, []).

%% @doc Erlangの項を文字列(数値のリスト)に、指定されたオプションに従って変換する
%%
%% 入力値が非負の数値リストの場合は、変換は行われずにそのまま返される。<br />
%% ユニコード値のリストから、UTF-8のリストへ変換したい場合等は unicode モジュールを使用する必要がある。<br />
%%
%% 入力値がタプルや深いリストならば `print' オプションを指定することで<br />
%% io_lib:format/2 のフォーマット`"~p"'に従った表現で変換することができる。<br />
%% デフォルト値は`"~w"'。<br />
%%
%% 入力値が浮動小数点数ならば float_to_list/2 で指定できるオプション<br />
%%   [{scientific, Decimals} | {decimals, Decimals} | compact]<br />
%% を利用して変換方式を指定することができる。<br />
%% {scientific, Decimals} と {decimals, Decimals} が同時に指定された場合は、最後に指定されたものが採用される。<br />
%% 例:
%% ```
%% > moyo_string:to_string(12.34, [{float_format, [{scientific, 6}]}]).
%% "1.234000e+01"
%% > moyo_string:to_string(12.34, [{float_format, [{decimals, 6}]}]).
%% "12.340000"
%% > moyo_string:to_string(12.34, [{float_format, [{decimals, 6}, compact]}]).
%% "12.34"
%% '''
-spec to_string(term(), [encode_option()]) -> string().
to_string(V, []) ->
    to_string_impl(V, "~w");
to_string(V, [print | _]) ->
    to_string_impl(V, "~p");
to_string(V, [{float_format, FloatFormatOptions} | _]) when is_float(V) ->
    float_to_list(V, FloatFormatOptions);
to_string(V, [_ | Rest]) ->
    to_string(V, Rest).

%% @doc 指定されたフォーマットの文字列を生成して返す.
%%
%% `lists:flatten(io_lib:format(Format, Data))'と同等.
-spec format(Format, Data) -> string() when
      Format :: io:format(),
      Data   :: [term()].
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%% @doc 引数の値がASCII文字列であるかどうかを判定する
%%
%% ASCII文字列とは 0 から 127 までのコード値の文字で構成されている文字列のこと
-spec is_ascii_string(Value::term()) -> boolean().
is_ascii_string(X) when is_list(X) -> lists:all(fun (C) -> is_integer(C) andalso C >= 0 andalso C =< 127 end, X);
is_ascii_string(_)                 -> false.

%% @doc 引数の値が`iolist'であるかどうかを判定する
%%
%% `iolist()'の型は `maybe_improper_list(byte() | binary() | iolist(), binary() | [])'. <br />
%% See: [http://www.erlang.org/doc/reference_manual/typespec.html]
-spec is_iolist(Value::term()) -> boolean().
is_iolist([])                      -> true;
is_iolist([X])                     -> is_iolist_element(X);
is_iolist([H | T]) when is_list(T) -> is_iolist_element(H) andalso is_iolist(T);
is_iolist([H | T])                 -> is_iolist_element(H) andalso is_binary(T);
is_iolist(_Other)                  -> false.

%% @doc 引数の値が`iodata'であるかどうかを判定する
%%
%% `iodata()'の型は`binary() | iolist()'.
-spec is_iodata(Value::term()) -> boolean().
is_iodata(X) -> is_binary(X) orelse is_iolist(X).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc 引数の値が`iolist'の要素になり得るものかどうかを判定する
-spec is_iolist_element(term()) -> boolean().
is_iolist_element(X) when is_binary(X)                                    -> true;
is_iolist_element(X) when is_integer(X) andalso (0 =< X andalso X =< 255) -> true;
is_iolist_element(X)                                                      -> is_iolist(X).

%% @private
%% @doc Erlangの項を文字列(数値のリスト)に変換する
-spec to_string_impl(term(), io:format()) -> string().
to_string_impl(V, _) when is_binary(V)    -> binary_to_list(V);
to_string_impl(V, _) when is_atom(V)      -> atom_to_list(V);
to_string_impl(V, _) when is_integer(V)   -> integer_to_list(V);
to_string_impl(V, _) when is_float(V)     -> float_to_list(V);
to_string_impl(V, _) when is_function(V)  -> erlang:fun_to_list(V);
to_string_impl(V, _) when is_pid(V)       -> erlang:pid_to_list(V);
to_string_impl(V, _) when is_port(V)      -> erlang:port_to_list(V);
to_string_impl(V, _) when is_reference(V) -> erlang:ref_to_list(V);
to_string_impl(V, IoLibFormat) when is_list(V) ->
    IsNonNegInteger = fun (C) -> is_integer(C) andalso C >= 0 end,
    case lists:all(IsNonNegInteger, V) of
        true  -> V;
        false -> format(IoLibFormat, [V])
    end;
to_string_impl(V, IoLibFormat) ->
    format(IoLibFormat, [V]).
