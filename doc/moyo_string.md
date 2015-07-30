

# Module moyo_string #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

文字列(整数値のリスト)に関する処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td>指定されたフォーマットの文字列を生成して返す.</td></tr><tr><td valign="top"><a href="#is_ascii_string-1">is_ascii_string/1</a></td><td>引数の値がASCII文字列であるかどうかを判定する.</td></tr><tr><td valign="top"><a href="#is_iodata-1">is_iodata/1</a></td><td>引数の値が<code>iodata</code>であるかどうかを判定する.</td></tr><tr><td valign="top"><a href="#is_iolist-1">is_iolist/1</a></td><td>引数の値が<code>iolist</code>であるかどうかを判定する.</td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>Erlangの項を文字列(数値のリスト)に変換する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-2"></a>

### format/2 ###

<pre><code>
format(Format, Data) -&gt; string()
</code></pre>

<ul class="definitions"><li><code>Format = <a href="io.md#type-format">io:format()</a></code></li><li><code>Data = [term()]</code></li></ul>

指定されたフォーマットの文字列を生成して返す.

`lists:flatten(io_lib:format(Format, Data))`と同等.

<a name="is_ascii_string-1"></a>

### is_ascii_string/1 ###

<pre><code>
is_ascii_string(Value::term()) -&gt; boolean()
</code></pre>
<br />

引数の値がASCII文字列であるかどうかを判定する

ASCII文字列とは 0 から 127 までのコード値の文字で構成されている文字列のこと

<a name="is_iodata-1"></a>

### is_iodata/1 ###

<pre><code>
is_iodata(Value::term()) -&gt; boolean()
</code></pre>
<br />

引数の値が`iodata`であるかどうかを判定する

`iodata()`の型は`binary() | iolist()`.

<a name="is_iolist-1"></a>

### is_iolist/1 ###

<pre><code>
is_iolist(Value::term()) -&gt; boolean()
</code></pre>
<br />

引数の値が`iolist`であるかどうかを判定する

`iolist()`の型は `maybe_improper_list(byte() | binary() | iolist(), binary() | [])`. <br />
See: [`http://www.erlang.org/doc/reference_manual/typespec.html`](http://www.erlang.org/doc/reference_manual/typespec.html)

<a name="to_string-1"></a>

### to_string/1 ###

<pre><code>
to_string(V::term()) -&gt; string()
</code></pre>
<br />

Erlangの項を文字列(数値のリスト)に変換する

入力値が非負の数値リストの場合は、変換は行われずにそのまま返される。<br />
ユニコード値のリストから、UTF-8のリストへ変換したい場合等は unicode モジュールを使用する必要がある。

