

# Module moyo_url #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

URL関連の処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build_qs-1">build_qs/1</a></td><td>連想リストからHTTPのクエリ文字列を生成する.</td></tr><tr><td valign="top"><a href="#parse_query-1">parse_query/1</a></td><td>URLのクエリストリング部をパースして、対応する連想リストを取得する.</td></tr><tr><td valign="top"><a href="#urldecode_base64-1">urldecode_base64/1</a></td><td>base64url形式でエンコードされたバイナリをデコードする.</td></tr><tr><td valign="top"><a href="#urldecode_rfc3986-1">urldecode_rfc3986/1</a></td><td>RFC3986形式でエンコードされているバイナリをデコードする.</td></tr><tr><td valign="top"><a href="#urlencode_base64-1">urlencode_base64/1</a></td><td>Equivalent to <a href="#urlencode_base64-2"><tt>urlencode_base64(PlainText, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#urlencode_base64-2">urlencode_base64/2</a></td><td>バイナリをbase64url形式にエンコードする.</td></tr><tr><td valign="top"><a href="#urlencode_rfc3986-1">urlencode_rfc3986/1</a></td><td>テキストを RFC3986 にもとづいてエンコードする.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="build_qs-1"></a>

### build_qs/1 ###

<pre><code>
build_qs(AssocList::<a href="moyo_assoc.md#type-assoc_list">moyo_assoc:assoc_list()</a>) -&gt; binary()
</code></pre>
<br />

連想リストからHTTPのクエリ文字列を生成する.

URLエンコード処理には[`urlencode_rfc3986/1`](#urlencode_rfc3986-1)が使用される. <br />
連想リストの要素のキーおよび値が文字列ではない場合は[`moyo_string:to_string/1`](moyo_string.md#to_string-1)を使って文字列に変換される.

<a name="parse_query-1"></a>

### parse_query/1 ###

<pre><code>
parse_query(QueryString::binary()) -&gt; [{Key::binary(), Value::binary()}]
</code></pre>
<br />

URLのクエリストリング部をパースして、対応する連想リストを取得する

基本的には、標準の[`httpd:parse_query/1`](httpd.md#parse_query-1)と同じ動作を取るが、
戻り値の連想リストのキー及び値の型が文字列からバイナリに変わっている。<br />
また、標準の関数は入力サイズが大きくなると極端に処理速度が遅くなるために、標準関数の使用は推奨されない。

クエリストリング内の各パラメータのキーおよび値に含まれる'+'は半角スペースに展開され、
'%xx'は対応するコードの文字に展開される。

また、キーに対応する値が存在しない場合は、空文字(バイナリ)が値として使用される。

```
  > parse_query(<<"a&b=10">>).
  [{<<"a">>, <<"">>}, {<<"b">>, <<"10">>}]
  なお、入力として"%a=b"のように不正なパーセントエンコーディング文字が渡された場合の挙動は未定義。<br />
  (何らかの解釈でデコードされた結果が返るかもしれないし、エラーとなるかもしれない)
```

<a name="urldecode_base64-1"></a>

### urldecode_base64/1 ###

<pre><code>
urldecode_base64(EncodedText) -&gt; PlainText
</code></pre>

<ul class="definitions"><li><code>EncodedText = binary()</code></li><li><code>PlainText = binary()</code></li></ul>

base64url形式でエンコードされたバイナリをデコードする.

base64url形式については [`urlencode_base64/2`](#urlencode_base64-2) のドキュメントを参照のこと。<br />
`EncodedText`の末尾にパディング文字("=")が不足している場合は、デコード時に自動で補われる。

<a name="urldecode_rfc3986-1"></a>

### urldecode_rfc3986/1 ###

<pre><code>
urldecode_rfc3986(EncodedText::binary()) -&gt; PlainText::binary()
</code></pre>
<br />

RFC3986形式でエンコードされているバイナリをデコードする.

なお、RFC3986に正式に準拠していないバイナリが渡された場合でも、デコードに支障がないようであれば、特にエラーとはならない.<br />
(ex. "|"のように本来パーセントエスケープされるべき文字が生のまま含まれていても、エラーとはならない)<br />

RFC3986に関しては[`urlencode_rfc3986`](urlencode_rfc3986.md)を参照のこと.

<a name="urlencode_base64-1"></a>

### urlencode_base64/1 ###

`urlencode_base64(PlainText) -> any()`

Equivalent to [`urlencode_base64(PlainText, [])`](#urlencode_base64-2).

<a name="urlencode_base64-2"></a>

### urlencode_base64/2 ###

<pre><code>
urlencode_base64(PlainText, Options) -&gt; EncodedText
</code></pre>

<ul class="definitions"><li><code>PlainText = binary()</code></li><li><code>EncodedText = binary()</code></li><li><code>Options = [Option]</code></li><li><code>Option = no_padding</code></li></ul>

バイナリをbase64url形式にエンコードする.

base64url形式とは、通常のbase64エンコードの結果文字列の"+"および"/"を、それぞれ"-"および"_"に置き換えたもの。<br />
(この処理によって、base64エンコード結果をURL内に安全に含めることができるようになる)<br />

base64urlの詳細は [`http://tools.ietf.org/html/rfc4648#section-5`](http://tools.ietf.org.md/rfc4648#section-5) を参照のこと.

オプションで`no_padding`が指定された場合は、base64エンコード結果の文字列から末尾のパディング文字("=")が除外される.

```
  > urlencode_base64(<<"this is a pen">>, []).
  <<"dGhpcyBpcyBhIHBlbg==">>
  > urlencode_base64(<<"this is a pen">>, [no_padding]).
  <<"dGhpcyBpcyBhIHBlbg">>
```

<a name="urlencode_rfc3986-1"></a>

### urlencode_rfc3986/1 ###

<pre><code>
urlencode_rfc3986(PlainText::binary()) -&gt; EncodedText::binary()
</code></pre>
<br />

テキストを RFC3986 にもとづいてエンコードする.

[a-zA-Z0-9_.~-]を除く全ての文字は`%XX`形式の文字列で置換される. <br />
RFC3986: [`http://www.faqs.org/rfcs/rfc3986.html`](http://www.faqs.org/rfcs/rfc3986.md)

