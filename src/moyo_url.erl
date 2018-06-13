%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc URL関連の処理を集めたユーティリティモジュール.
-module(moyo_url).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         urlencode_base64/1, urlencode_base64/2,
         urldecode_base64/1,

         urlencode_rfc3986/1,
         urldecode_rfc3986/1,

         parse_query/1,
         build_qs/1,

         parse_scheme/1,
         parse_url/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc バイナリをbase64url形式にエンコードする.
%%
%% base64url形式とは、通常のbase64エンコードの結果文字列の"+"および"/"を、それぞれ"-"および"_"に置き換えたもの。<br />
%% (この処理によって、base64エンコード結果をURL内に安全に含めることができるようになる)<br />
%%
%% base64urlの詳細は [http://tools.ietf.org/html/rfc4648#section-5] を参照のこと.
%%
%% オプションで`no_padding'が指定された場合は、base64エンコード結果の文字列から末尾のパディング文字("=")が除外される.
%%
%% ```
%% > urlencode_base64(<<"this is a pen">>, []).
%% <<"dGhpcyBpcyBhIHBlbg==">>
%%
%% > urlencode_base64(<<"this is a pen">>, [no_padding]).
%% <<"dGhpcyBpcyBhIHBlbg">>
%% '''
-spec urlencode_base64(PlainText, Options) -> EncodedText when
      PlainText   :: binary(),
      EncodedText :: binary(),
      Options     :: [Option],
      Option      :: no_padding.
urlencode_base64(PlainText, Options) when is_binary(PlainText) ->
    EncodedText = moyo_binary:tr(base64:encode(PlainText), [{$+, $-}, {$/, $_}]),
    case lists:member(no_padding, Options) of
        false -> EncodedText;
        true  -> re:replace(EncodedText, <<"=+$">>, <<"">>, [{return, binary}])
    end.

%% @equiv urlencode_base64(PlainText, [])
urlencode_base64(PlainText) ->
    urlencode_base64(PlainText, []).

%% @doc base64url形式でエンコードされたバイナリをデコードする.
%%
%% base64url形式については {@link urlencode_base64/2} のドキュメントを参照のこと。<br />
%% `EncodedText'の末尾にパディング文字("=")が不足している場合は、デコード時に自動で補われる。
-spec urldecode_base64(EncodedText) -> PlainText when
      EncodedText :: binary(),
      PlainText   :: binary().
urldecode_base64(EncodedText) when is_binary(EncodedText) ->
    EncodedText2 =
        case (byte_size(EncodedText) rem 4) of
            0 -> EncodedText;
            N -> <<EncodedText/binary, <<"===">>:(4-N)/binary>>  % 不足しているパディング文字を補完する
        end,
    base64:decode(moyo_binary:tr(EncodedText2, [{$-, $+}, {$_, $/}])).

%% @doc テキストを RFC3986 にもとづいてエンコードする.
%%
%% [a-zA-Z0-9_.~-]を除く全ての文字は`%XX'形式の文字列で置換される. <br />
%% RFC3986: [http://www.faqs.org/rfcs/rfc3986.html]
-spec urlencode_rfc3986(binary()) -> EncodedText::binary().
urlencode_rfc3986(PlainText) ->
    urlencode_impl(PlainText, fun is_rfc3986_safe_byte/1).

%% @doc RFC3986形式でエンコードされているバイナリをデコードする.
%%
%% なお、RFC3986に正式に準拠していないバイナリが渡された場合でも、デコードに支障がないようであれば、特にエラーとはならない.<br />
%% (ex. "|"のように本来パーセントエスケープされるべき文字が生のまま含まれていても、エラーとはならない)<br />
%%
%% RFC3986に関しては{@link urlencode_rfc3986}を参照のこと.
-spec urldecode_rfc3986(binary()) -> PlainText::binary().
urldecode_rfc3986(EncodedText) ->
    urldecode_impl(EncodedText, false).

%% @doc URLのクエリストリング部をパースして、対応する連想リストを取得する
%%
%% 基本的には、標準の{@link httpd:parse_query/1}と同じ動作を取るが、
%% 戻り値の連想リストのキー及び値の型が文字列からバイナリに変わっている。<br />
%% また、標準の関数は入力サイズが大きくなると極端に処理速度が遅くなるために、標準関数の使用は推奨されない。
%%
%% クエリストリング内の各パラメータのキーおよび値に含まれる'+'は半角スペースに展開され、
%% '%xx'は対応するコードの文字に展開される。
%%
%% また、キーに対応する値が存在しない場合は、空文字(バイナリ)が値として使用される。
%% ```
%% > parse_query(<<"a&b=10">>).
%% [{<<"a">>, <<"">>}, {<<"b">>, <<"10">>}]
%%
%% なお、入力として"%a=b"のように不正なパーセントエンコーディング文字が渡された場合の挙動は未定義。<br />
%% (何らかの解釈でデコードされた結果が返るかもしれないし、エラーとなるかもしれない)
%% '''
-spec parse_query(QueryString::binary()) -> [{Key::binary(), Value::binary()}].
parse_query(<<QueryString/binary>>) ->
    [case binary:split(Token, <<"=">>) of
         [Key, Value] -> {urldecode_impl(Key, true), urldecode_impl(Value, true)};
         [Key]        -> {urldecode_impl(Key, true), <<"">>}
     end || Token <- binary:split(QueryString, <<"&">>, [global])].

%% @doc 連想リストからHTTPのクエリ文字列を生成する.
%%
%% URLエンコード処理には{@link urlencode_rfc3986/1}が使用される. <br />
%% 連想リストの要素のキーおよび値が文字列ではない場合は{@link moyo_string:to_string/1}を使って文字列に変換される.
-spec build_qs(moyo_assoc:assoc_list()) -> binary().
build_qs(AssocList) ->
    list_to_binary(
      string:join(
        [begin
             K2 = moyo_url:urlencode_rfc3986(moyo_binary:to_binary(K)),
             V2 = moyo_url:urlencode_rfc3986(moyo_binary:to_binary(V)),
             [K2, $=, V2]
         end || {K, V} <- AssocList],
        "&")).

%% @doc URLのschemeを取得する
-spec parse_scheme(URL :: binary()) ->
    {ok, {Scheme :: binary(), Rest :: binary()}} | error.
parse_scheme(URL) ->
    case  binary:split(URL,<<"://">>) of
        [Scheme,Rest]-> {ok,{Scheme,Rest}};
        _->error
    end.

%% @doc URLをparseし、URLの要素をmapで返す
%%
%% URLに必須なschemeとhostが入力に含まれない場合、もしくはperseに失敗した場合errorとなる
%% http_uri:parseとの違い
%% + 入出力の文字列をバイナリ型で取り扱う
%%     string()で取り扱わない
%% + schemeをバイナリで返す
%%     http_uri版はschemeをatomで返すため、ユーザ入力を受け付ける場合、無限にatomが生成される恐れがあったが、
%%     この関数はバイナリで返すためleakの心配がない
%% + デフォルトのport指定を持たない
%%     http_uri版は"http://foo/"の様なhttp+port指定なし入力の場合、戻り値のportに80自動指定するが、
%%     この関数ではしない。parserとしての機能としては大きすぎ、parseしたいだけなのにschemeとportの組を設定する必要がある等邪魔になることがあったので外した

-spec parse_url(URL :: binary()) ->
    {ok, ParsedURL}| {error, Reason :: term()} when
    ParsedURL :: #{
        scheme => binary(),     % mandatory
        userinfo => binary(),   % optional
        host => binary(),       % mandatory
        port => pos_integer(),  % optional
        path => binary(),       % mandatory
        query => binary(),      % optional
        fragment => binary()    % optional
    }.
parse_url(URL) ->
    case parse_scheme(URL) of
        error -> {error, require_scheme};
        {ok, {Scheme, UserHostPortPathQueryFragment}} ->
            %% fragment query pathを取り出す 
            {UserHostPortPathQuery, Fragment} =
                splitL(UserHostPortPathQueryFragment, <<"#">>),
            {UserHostPortPath, Query} =
                splitL(UserHostPortPathQuery, <<"?">>),
            {UserHostPort, Path} =
                splitL(UserHostPortPath, <<"/">>),

            %%userinfoを取り出す
            {User, HostPort} =
                case binary:split(UserHostPort, <<"@">>) of
                    [U, HP] -> {U, HP};
                    [HP] -> {none, HP}
                end,

            %% hostとportを取り出す
            {Host, Port} =
                case HostPort of
                    <<"[", Rest/binary>> ->  %ipv6用
                        case binary:split(Rest, <<"]">>) of
                            [L, <<":", R/binary>>] -> {L, R};
                            [L, <<"">>] -> {L, none};
                            _ -> {none, none} % 空のHostを返して関数を失敗させる
                        end;
                    _ ->
                        splitL(HostPort, <<":">>)
                end,

            %% Hostは必須なのでvalidate
            case Host =/= none of
                false -> {error, require_host};
                true ->
                    case parse_port(Port) of    %% portが正の数字かどうか解析
                        {error, _} = Error -> Error;
                        {ok, Portint} ->
                            Result =
                                maps:filter(fun(_, V) -> V =/= none end, #{
                                    scheme=> Scheme,
                                    userinfo=> User,
                                    host=> Host,
                                    port=> Portint,
                                    path=> map_add_prefix(Path, <<"/">>, <<"/">>),
                                    query=> map_add_prefix(Query, <<"?">>, none),
                                    fragment=> map_add_prefix(Fragment, <<"#">>, none)
                                }),
                            {ok, Result}
                    end
            end
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec urlencode_impl(binary(), IsSafeByteFun) -> EncodedText::binary() when
      IsSafeByteFun :: fun((byte()) -> boolean()).
urlencode_impl(PlainText, IsSafeByteFun) ->
    list_to_binary(
      [case IsSafeByteFun(Byte) of
           true  -> Byte;
           false -> percent_escape(Byte)
       end || <<Byte>> <= PlainText]).

-spec percent_escape(byte()) -> Escaped::binary().
percent_escape(Byte) ->
    Table = <<"0123456789ABCDEF">>,
    High  = Byte bsr 4,
    Low   = Byte rem 16,
    <<$%, (binary:at(Table, High)), (binary:at(Table, Low))>>.

-spec is_rfc3986_safe_byte(byte()) -> boolean().
is_rfc3986_safe_byte(Byte) ->
    ($a =< Byte andalso Byte =< $z) orelse
    ($A =< Byte andalso Byte =< $Z) orelse
    ($0 =< Byte andalso Byte =< $9) orelse
    lists:member(Byte, "-_.~").

-spec urldecode_impl(binary(), Interpret_x_www_form_urlencoded) -> PlainText::binary() when
      Interpret_x_www_form_urlencoded :: boolean(). % '+'を' 'に展開するかどうか
urldecode_impl(EncodedText, Interpret_x_www_form_urlencoded) ->
    urldecode_impl(EncodedText, [], Interpret_x_www_form_urlencoded).

-spec urldecode_impl(binary(), iolist(), Interpret_x_www_form_urlencoded) -> PlainText::binary() when
      Interpret_x_www_form_urlencoded :: boolean().
urldecode_impl(<<>>, Acc, _Interpret_x_www_form_urlencoded) ->
    list_to_binary(lists:reverse(Acc));
urldecode_impl(<<$%, High, Low, Rest/binary>>, Acc, Interpret_x_www_form_urlencoded) ->
    ByteHigh = list_to_integer([High], 16),
    ByteLow  = list_to_integer([Low], 16),
    Byte     = (ByteHigh bsl 4) + ByteLow,
    urldecode_impl(Rest, [Byte | Acc], Interpret_x_www_form_urlencoded);
urldecode_impl(<<$+, Rest/binary>>, Acc, true) ->
    urldecode_impl(Rest, [$  | Acc], true);
urldecode_impl(<<Byte, Rest/binary>>, Acc, Interpret_x_www_form_urlencoded) ->
    urldecode_impl(Rest, [Byte | Acc], Interpret_x_www_form_urlencoded).

%% 常に2要素返すbinary:split関数。1要素しか返せない場合、右の要素をnoneにする
-spec splitL(Bin :: binary(), Pattern :: binary()) -> {binary(), none|binary()}.
splitL(Bin, Pattern) ->
    case binary:split(Bin, Pattern) of
        [L, R] -> {L, R};
        [L] -> {L, none}
    end.

%% バイナリが正の数字ならその数字を、noneならnoneを、それ以外ならerrorを返す
-spec parse_port(Bin :: binary()) -> {ok, pos_integer()}|{ok, none}|{error, Reason :: term()}.
parse_port(Bin) ->
    case Bin of
        none -> {ok, none};
        _ ->
            case catch binary_to_integer(Bin) of
                Portint when is_integer(Portint) andalso Portint > 0 -> {ok, Portint};
                _ -> {error, require_port_is_integer}
            end
    end.

%% none|binary を受け取り Default|入力にprefix を付けて返す
-spec map_add_prefix(Bin :: none|binary(), Prefix :: binary(), Default :: any()) -> any()|binary().
map_add_prefix(Bin, Prefix, Default) ->
    case Bin of
        none -> Default;
        _ -> <<Prefix/binary, Bin/binary>>
    end.
