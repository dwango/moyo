%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_xmlモジュールのユニットテスト
-module(moyo_xml_tests).

-include_lib("eunit/include/eunit.hrl").

-define(UTF8(Chars), unicode:characters_to_binary(Chars)).

parse_binary_test_() ->
    [
     {"空要素のみの場合のパース",
      fun () ->
              Input    = <<"<test />">>,
              Expected = {test, [], []},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type,atom}]))
      end},
     {"属性を含む場合のパース",
      fun () ->
              Input    = <<"<test attr1=\"10\" attr2=\"20\"/>">>,
              Expected = {test,
                          [{attr1, <<"10">>},
                           {attr2, <<"20">>}],
                          []},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type,atom}]))
      end},
     {"テキストを含む場合のパース",
      fun () ->
              Input    = ?UTF8("<test attr=\"属性値\">テキスト</test>"),
              Expected = {test,
                          [{attr, ?UTF8("属性値")}],
                          [?UTF8("テキスト")]},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type,atom}]))
      end},
     {"子要素を含む場合のパース",
      fun () ->
              Input    = ?UTF8("<test attr=\"属性値\">テキスト<child>子テキスト</child></test>"),
              Expected = {test,
                          [{attr, ?UTF8("属性値")}],
                          [
                           ?UTF8("テキスト"),
                           {child, [], [?UTF8("子テキスト")]}
                          ]},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type,atom}]))
      end},
     {"改行やコメントは無視される",
      fun () ->
              Input    = ?UTF8("<test>
                                  <!-- 改行あり -->
                                  <child>子テキスト</child>
                                </test>"),
              Expected = {test,
                          [],
                          [
                           {child, [], [?UTF8("子テキスト")]}
                          ]},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type,atom}]))
      end},
     {"入力データの途中でXMLのパースが終了した場合は、残り(未パース)のデータが返り値に含まれる",
      fun () ->
              Input    = ?UTF8("<test>テキスト</test><next>次のXML</next>"),
              Expected = {test,
                          [],
                          [
                           ?UTF8("テキスト")
                          ]},
              RestText = ?UTF8("<next>次のXML</next>"),

              ?assertEqual({Expected, RestText},
                           moyo_xml:parse_binary(Input, [{key_type,atom}]))
      end},
     {"デフォルトでは、要素名および属性名の型はバイナリとなる",
      fun () ->
              Input    = <<"<test attr=\"10\"/>">>,
              Expected = {<<"test">>,
                          [{<<"attr">>, <<"10">>}],
                          []},

              %% key_typeの指定なし
              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [])),

              %% 値に binary を指定しても同様
              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type, binary}]))
      end},
     {"key_typeオプションに atom を指定すると、要素名および属性名の型がアトムとなる",
      fun () ->
              Input    = <<"<test attr=\"10\"/>">>,
              Expected = {test,
                          [{attr, <<"10">>}],
                          []},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type, atom}]))
      end},
     {"key_typeオプションに existing_atom を指定すると、要素名および属性名が既にアトムとして存在する場合は、型がアトムとなる",
      fun () ->
              Input    = ?UTF8("<test アトムとして存在しない属性名=\"10\"/>"),
              Expected = {test,
                          [{?UTF8("アトムとして存在しない属性名"), <<"10">>}],
                          []},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_binary(Input, [{key_type, existing_atom}]))
      end},
     {"XMLとして不正な文字列を渡した場合はパースに失敗する",
      fun () ->
              Input = ?UTF8("<test>閉じタグの要素名が異なっている</close>"),

              ?assertError(_, moyo_xml:parse_binary(Input, []))
      end},
     {"外部エンティティが許可されていない場合は例外を吐く",
      fun () ->
              Input = <<"<!DOCTYPE test [<!ENTITY hoge SYSTEM \"http://localhost/hoge.xml\">]> <test>&hoge;</test>\n">>,

              ?assertError({parse_xml_failed, {error, _, "external_entity_not_allowed : hoge", _, _}, _},
                            moyo_xml:parse_binary(Input, [{allow_external_entity, false}]))
      end}
    ].

parse_file_test_() ->
    [
     {"XMLファイルをパースする",
      fun () ->
              Expected = {test,
                          [{attr, ?UTF8("属性値")}],
                          [
                           ?UTF8("テキスト"),
                           {child, [], [?UTF8("子テキスト")]}
                          ]},

              ?assertEqual({Expected, <<>>},
                           moyo_xml:parse_file(
                             filename:dirname(?FILE) ++ "/testdata/moyo_xml/test.xml",
                             [{key_type, atom}]))
      end}
    ].

to_iolist_test_() ->
    [
     {"xml()型のデータをiolistに変換する",
      fun () ->
              Xml = {test,
                     [{<<"attr1">>, ?UTF8("値")},
                      {attr2, 1},
                      {attr3, val}],
                     [
                      ?UTF8("テキスト"),
                      {<<"child">>, [], [there, <<" are ">>, 2, [" ", "pens"]]},
                      ?UTF8("数値"),
                      {<<"float">>, [], [12.34]}
                     ]},
              Expected1 = ?UTF8("<?xml version=\"1.0\" encoding=\"UTF-8\" ?><test attr1=\"値\" attr2=\"1\" attr3=\"val\">テキスト<child>there are 2 pens</child>数値<float>1.23399999999999998579e+01</float></test>"),
              Expected2 = ?UTF8("<?xml version=\"1.0\" encoding=\"UTF-8\" ?><test attr1=\"値\" attr2=\"1\" attr3=\"val\">テキスト<child>there are 2 pens</child>数値<float>12.34</float></test>"),
              ?assertEqual(Expected1, list_to_binary(moyo_xml:to_iolist(Xml))),
              ?assertEqual(Expected2, list_to_binary(moyo_xml:to_iolist(Xml, [{float_format, [{decimals, 6}, compact]}])))
      end},
     {"変換に失敗した場合は例外が送出される",
      fun () ->
              %% 不正な入力の一例
              Xml1 = {test, "wrong attributes", []}, % 属性の形式が不正
              ?assertError({invalid_xml_attribute, _}, moyo_xml:to_iolist(Xml1)),

              Xml2 = {1, [], []}, % 要素名の型が不正
              ?assertError({invalid_xml_element_name, _}, moyo_xml:to_iolist(Xml2)),

              Xml3 = {test, [{1, 2}], []}, % 属性名の型が不正
              ?assertError({invalid_xml_attribute_name, _}, moyo_xml:to_iolist(Xml3))
      end}
    ].
