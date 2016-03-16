%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc XMLに関する処理を集めたユーティリティモジュール.
%%
%% === 【XMLの表現形式】 ===
%% このモジュールでは、XMLの表現形式として、以下のような三要素のタプルが採用されている。
%% ```
%% {要素名, 属性連想リスト, 子要素およびテキストのリスト}
%% '''
%% また要素名や属性のキー及び値、テキストは、原則としてバイナリで保持されるようになっている。 <br />
%% (ただし、要素名や属性のキーは、アトムで保持することも可能。 <br />
%%  また、XML文字列を生成する場合の入力値としては、もう少し制限の緩い表現が使用可能となっている)
%%
%% 例えば `"<root attr=\"1\">text1<child />text2</root>"' といったXML文字列をパースすると次のような結果が返ってくる:
%% ```
%% > moyo_xml:parse_binary(<<"<root attr=\"1\">text1<child />text2</root>">>, []).
%% {{<<"root">>,
%%   [{<<"attr">>,<<"1">>}],
%%   [<<"text1">>,{<<"child">>,[],[]},<<"text2">>]},
%%  <<>>}
%% '''
-module(moyo_xml).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         parse_file/2,
         parse_binary/2,
         to_iolist/1,
         to_iolist/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Types
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              xml/0,
              xml_element/0,
              xml_element_name/0,
              xml_attribute/0,
              xml_attribute_key/0,
              xml_attribute_value/0,
              xml_content/0,
              xml_text/0,

              parse_option/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type xml()                 :: xml_element().
-type xml_element()         :: {xml_element_name(), [xml_attribute()], [xml_content()]}.
-type xml_element_name()    :: atom() | binary().
-type xml_attribute()       :: {xml_attribute_key(), xml_attribute_value()}.
-type xml_attribute_key()   :: atom() | binary().
-type xml_attribute_value() :: iodata() | term(). % parse系関数の結果としては常にバイナリが返る
-type xml_content()         :: xml_element() | xml_text().
-type xml_text()            :: iodata() | term(). % parse系関数の結果としては常にバイナリが返る

-type parse_option() :: {key_type, binary | atom | existing_atom}
                       |{allow_external_entity, true | false}.
%% <b>[key_type オプション]</b><br />
%% パース結果XMLの要素名および属性名をどのような型で表現するかを指定する.<br />
%% `binary'ならバイナリ型、`atom'ならアトム型.<br />
%% `existing_atom'の場合は、名前に対応するアトムが既に存在する場合はアトム型、存在しないならバイナリ型となる. <br />
%% デフォルト値は`binary'.
%%
%% <b>[allow_external_entity オプション]</b><br />
%% 外部エンティティ参照を許可するかどうかを指定する．<br />
%% `true' であれば外部エンティティ参照が許可される．<br />
%% `false' とすることで外部エンティティ参照を禁止できる．<br />
%% デフォルト値は`false'.

%%----------------------------------------------------------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------------------------------------------------------
-record(parse_option,
        {
          key_type = binary :: binary | atom | existing_atom
        }).

-record(parse_state,
       {
          element_stack   :: [xml_element()],
          options         :: #parse_option{}
       }).

%%----------------------------------------------------------------------------------------------------------------------
%% Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc XMLファイルをパースする.
%%
%% パース結果XMLの属性値およびテキストの型は常にバイナリとなる. <br />
%% パースに失敗した場合は例外が送出される.
-spec parse_file(FilePath, [parse_option()]) -> {xml(), RestXml} when
      FilePath :: file:name_all(),
      RestXml  :: binary().
parse_file(FilePath, Options) ->
    parse_impl(fun xmerl_sax_parser:file/2, FilePath, Options).

%% @doc XML文字列(バイナリ)をパースする.
%%
%% パース結果XMLの属性値およびテキストの型は常にバイナリとなる. <br />
%% パースに失敗した場合は例外が送出される.
-spec parse_binary(InputXml, [parse_option()]) -> {xml(), RestXml} when
      InputXml :: binary(),
      RestXml  :: binary().
parse_binary(<<InputXml/binary>>, Options) ->
    parse_impl(fun xmerl_sax_parser:stream/2, InputXml, Options).

%% @doc XMLをiolist形式の文字列に変換する
%%
%% 変換に失敗した場合は例外が送出される. <br />
%% 要素の属性値や内容は{@link moyo_string:to_string/1}によって、適宜文字列に変換される.
-spec to_iolist(xml()) -> XmlString::iolist().
to_iolist(Xml) ->
    to_iolist(Xml, []).

%% @doc XMLをiolist形式の文字列に、指定されたオプションに従って変換する
%%
%% 変換に失敗した場合は例外が送出される. <br />
%% 要素の属性値や内容は{@link moyo_string:to_string/2}によって、適宜文字列に変換される.
-spec to_iolist(xml(), [moyo_string:encode_option()]) -> XmlString::iolist().
to_iolist(Xml, Options) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"],
    IoList = xmerl:export_simple([normalize_xml_for_export_simple(Xml, Options)], xmerl_xml, [{prolog, Prolog}]),
    IoList.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%%----------------------------------------
%% Parse Functions
%%----------------------------------------
-spec parse_impl(SaxParseFun, InputXml, [parse_option()]) -> {xml(), RestXml} when
      InputXml        :: term(),
      SaxParseFun     :: fun ((InputXml, SaxParseOptions) -> SaxParseResult),
      SaxParseOptions :: term(),
      SaxParseResult  :: term(),
      RestXml         :: binary().
parse_impl(SaxParseFun, InputXml, Options) ->
    ParseOptions = #parse_option{
      key_type = moyo_assoc:fetch(key_type, Options, binary)
    },
    ExternalEntity = moyo_assoc:fetch(allow_external_entity, Options, false),
    ParseState = #parse_state{
      element_stack = [{root, [], []}], % 番兵値
      options       = ParseOptions
    },

    SaxOptions = [
                 {event_fun, fun(Event, Location, State) ->
                                  event_fun_parse_xml(Event, Location, State, ExternalEntity)
                              end},
                  {event_state, ParseState}
                 ],
    case SaxParseFun(InputXml, SaxOptions) of
        {ok, #parse_state{element_stack = ResultStack}, Rest} ->
            [{root, _, [Xml]}] = ResultStack,
            {Xml, Rest};
        FailureInfo ->
            error({parse_xml_failed, FailureInfo, InputXml})
    end.

%% @doc `xmerl_sax_parser'の`EventFun'の実装
event_fun_parse_xml({startElement, _Uri, LocalName, _QualifiedName, Attributes}, _Location, State, _ExternalEntity) ->
    #parse_state{element_stack = Stack, options = #parse_option{key_type = KeyType}} = State,
    Attributes2 = [begin
                       AttributeKey   = convert_string(K, KeyType),
                       AttributeValue = convert_string(V, binary),
                       {AttributeKey, AttributeValue}
                   end || {_, _, K, V} <- Attributes],
    NewElement = {convert_string(LocalName, KeyType), Attributes2, []},
    State#parse_state{element_stack = [NewElement | Stack]};

event_fun_parse_xml({endElement, _Uri, _LocalName, _QualifiedName}, _Location, State, _ExternalEntity) ->
    #parse_state{element_stack = [Element, ParentElement | Stack]} = State,
    FixedElement = element_contents_reverse(Element),
    State#parse_state{element_stack = [element_push_content(FixedElement, ParentElement) | Stack]};

event_fun_parse_xml({characters, Str}, _Location, State, _ExternalEntity) ->
    #parse_state{element_stack = [Element | Stack]} = State,
    State#parse_state{element_stack = [element_push_content(convert_string(Str, binary), Element) | Stack]};
event_fun_parse_xml({externalEntityDecl, Name, _PublicId, _SystemId}, _Location, _State, false) ->
    throw({error, "external_entity_not_allowed : " ++ Name});
event_fun_parse_xml(_Event, _Location, State, _ExternalEntity) ->
    State.

%% @doc XML要素の内容(`xml_content'のリスト)を反転する
-spec element_contents_reverse(xml_element()) -> xml_element().
element_contents_reverse(Element) ->
    setelement(3, Element, lists:reverse(element(3, Element))).

%% @doc XML要素にコンテントを追加する.
%%
%% なお追加分のコンテントは、既に存在するコンテンツの先頭に挿入される.
-spec element_push_content(xml_content(), xml_element()) -> xml_element().
element_push_content(Content, Element) ->
    setelement(3, Element, [Content | element(3, Element)]).

%% @doc 文字列を指定の型に変換する
-spec convert_string(FromString, ToType) -> ToString when
      FromString :: string(),
      ToType     :: binary | atom | existing_atom,
      ToString   :: binary() | atom().
convert_string(Str, ToType) ->
    case ToType of
        binary        -> <<_/binary>> = unicode:characters_to_binary(Str);
        atom          -> list_to_atom(Str);
        existing_atom ->
            try
                list_to_existing_atom(Str)
            catch
                _:_ -> <<_/binary>> = unicode:characters_to_binary(Str)
            end
    end.

%%----------------------------------------
%% Output Functions
%%----------------------------------------

%% @doc xmerl:export_simple/3用に入力XMLデータを正規化する
%%
%% TODO: 無闇にバイナリをアトムに変換するのは好ましくないので、xmerl:export_simple/3用の独自コールバックを用意するようにしたい
-spec normalize_xml_for_export_simple(xml_element()|xml_text(), [moyo_string:encode_option()]) -> xml_element().
normalize_xml_for_export_simple({Name, Attributes, Contents}, Options) ->
    %% 要素名の型をアトムに統一する
    Name2 = if
                is_binary(Name) -> binary_to_atom(Name, utf8);
                is_atom(Name)   -> Name;
                true            -> error({invalid_xml_element_name, Name})
            end,

    %% 属性のキーをアトムに統一する
    Attributes2 = lists:map(
                    fun ({K, V}) ->
                            K2 = if
                                     is_binary(K) -> binary_to_atom(K, utf8);
                                     is_atom(K)   -> K;
                                     true         -> error({invalid_xml_attribute_name, K})
                                 end,
                            V2 = case moyo_string:is_iodata(V) of
                                     true  -> V;
                                     false -> moyo_string:to_string(V, Options)
                                 end,
                            {K2, V2};
                        (WrongAttr) ->
                            error({invalid_xml_attribute, WrongAttr})
                    end,
                    Attributes),

    Contents2 = [normalize_xml_for_export_simple(Content, Options) || Content <- Contents],

    {Name2, Attributes2, Contents2};
normalize_xml_for_export_simple(Text, _Options) when is_atom(Text) ->
    [atom_to_binary(Text, utf8)];
normalize_xml_for_export_simple(Text, _Options) when is_binary(Text) ->
    [Text];
normalize_xml_for_export_simple(Text, Options) ->
    case moyo_string:is_iodata(Text) of
        true  -> Text;
        false -> moyo_string:to_string(Text, Options)
    end.
