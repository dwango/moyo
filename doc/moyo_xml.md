

# Module moyo_xml #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

XMLに関する処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="description"></a>

## Description ##


#### <a name="【XMLの表現形式】">【XMLの表現形式】</a> ####

このモジュールでは、XMLの表現形式として、以下のような三要素のタプルが採用されている。

```
  {要素名, 属性連想リスト, 子要素およびテキストのリスト}
```

また要素名や属性のキー及び値、テキストは、原則としてバイナリで保持されるようになっている。 <br />
(ただし、要素名や属性のキーは、アトムで保持することも可能。 <br />
また、XML文字列を生成する場合の入力値としては、もう少し制限の緩い表現が使用可能となっている)

例えば `"<root attr=\"1\">text1<child />text2</root>"` といったXML文字列をパースすると次のような結果が返ってくる:

```
  > moyo_xml:parse_binary(<<"<root attr=\"1\">text1<child />text2</root>">>, []).
  {{<<"root">>,
    [{<<"attr">>,<<"1">>}],
    [<<"text1">>,{<<"child">>,[],[]},<<"text2">>]},
   <<>>}
```

<a name="types"></a>

## Data Types ##




### <a name="type-parse_option">parse_option()</a> ###


<pre><code>
parse_option() = {key_type, binary | atom | existing_atom}
</code></pre>

__[key_type オプション]__<br />
パース結果XMLの要素名および属性名をどのような型で表現するかを指定する.<br />
`binary`ならバイナリ型、`atom`ならアトム型.<br />
`existing_atom`の場合は、名前に対応するアトムが既に存在する場合はアトム型、存在しないならバイナリ型となる. <br />
デフォルト値は`binary`.



### <a name="type-xml">xml()</a> ###


<pre><code>
xml() = <a href="#type-xml_element">xml_element()</a>
</code></pre>




### <a name="type-xml_attribute">xml_attribute()</a> ###


<pre><code>
xml_attribute() = {<a href="#type-xml_attribute_key">xml_attribute_key()</a>, <a href="#type-xml_attribute_value">xml_attribute_value()</a>}
</code></pre>




### <a name="type-xml_attribute_key">xml_attribute_key()</a> ###


<pre><code>
xml_attribute_key() = atom() | binary()
</code></pre>




### <a name="type-xml_attribute_value">xml_attribute_value()</a> ###


<pre><code>
xml_attribute_value() = iodata() | term()
</code></pre>

parse系関数の結果としては常にバイナリが返る



### <a name="type-xml_content">xml_content()</a> ###


<pre><code>
xml_content() = <a href="#type-xml_element">xml_element()</a> | <a href="#type-xml_text">xml_text()</a>
</code></pre>




### <a name="type-xml_element">xml_element()</a> ###


<pre><code>
xml_element() = {<a href="#type-xml_element_name">xml_element_name()</a>, [<a href="#type-xml_attribute">xml_attribute()</a>], [<a href="#type-xml_content">xml_content()</a>]}
</code></pre>




### <a name="type-xml_element_name">xml_element_name()</a> ###


<pre><code>
xml_element_name() = atom() | binary()
</code></pre>




### <a name="type-xml_text">xml_text()</a> ###


<pre><code>
xml_text() = iodata() | term()
</code></pre>

parse系関数の結果としては常にバイナリが返る

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_binary-2">parse_binary/2</a></td><td>XML文字列(バイナリ)をパースする.</td></tr><tr><td valign="top"><a href="#parse_file-2">parse_file/2</a></td><td>XMLファイルをパースする.</td></tr><tr><td valign="top"><a href="#to_iolist-1">to_iolist/1</a></td><td>XMLをiolist形式の文字列に変換する.</td></tr><tr><td valign="top"><a href="#to_iolist-2">to_iolist/2</a></td><td>XMLをiolist形式の文字列に、指定されたオプションに従って変換する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_binary-2"></a>

### parse_binary/2 ###

<pre><code>
parse_binary(InputXml, Options::[<a href="#type-parse_option">parse_option()</a>]) -&gt; {<a href="#type-xml">xml()</a>, RestXml}
</code></pre>

<ul class="definitions"><li><code>InputXml = binary()</code></li><li><code>RestXml = binary()</code></li></ul>

XML文字列(バイナリ)をパースする.

パース結果XMLの属性値およびテキストの型は常にバイナリとなる. <br />
パースに失敗した場合は例外が送出される.

<a name="parse_file-2"></a>

### parse_file/2 ###

<pre><code>
parse_file(FilePath, Options::[<a href="#type-parse_option">parse_option()</a>]) -&gt; {<a href="#type-xml">xml()</a>, RestXml}
</code></pre>

<ul class="definitions"><li><code>FilePath = <a href="file.md#type-name_all">file:name_all()</a></code></li><li><code>RestXml = binary()</code></li></ul>

XMLファイルをパースする.

パース結果XMLの属性値およびテキストの型は常にバイナリとなる. <br />
パースに失敗した場合は例外が送出される.

<a name="to_iolist-1"></a>

### to_iolist/1 ###

<pre><code>
to_iolist(Xml::<a href="#type-xml">xml()</a>) -&gt; XmlString::iolist()
</code></pre>
<br />

XMLをiolist形式の文字列に変換する

変換に失敗した場合は例外が送出される. <br />
要素の属性値や内容は[`moyo_string:to_string/1`](moyo_string.md#to_string-1)によって、適宜文字列に変換される.

<a name="to_iolist-2"></a>

### to_iolist/2 ###

<pre><code>
to_iolist(Xml::<a href="#type-xml">xml()</a>, Options::[<a href="moyo_string.md#type-encode_option">moyo_string:encode_option()</a>]) -&gt; XmlString::iolist()
</code></pre>
<br />

XMLをiolist形式の文字列に、指定されたオプションに従って変換する

変換に失敗した場合は例外が送出される. <br />
要素の属性値や内容は[`moyo_string:to_string/2`](moyo_string.md#to_string-2)によって、適宜文字列に変換される.

