

# Module moyo_binary #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

バイナリに関する処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#abbreviate-2">abbreviate/2</a></td><td>Equivalent to <a href="#abbreviate-3"><tt>abbreviate(Bin, MaxLength, &lt;&lt;"..."&gt;&gt;)</tt></a>.</td></tr><tr><td valign="top"><a href="#abbreviate-3">abbreviate/3</a></td><td>入力バイナリが最大長を超えている場合に、指定された省略文字列を使って切り詰めを行う.</td></tr><tr><td valign="top"><a href="#fill-2">fill/2</a></td><td>同じ数字(文字)が連続したバイナリを作る.</td></tr><tr><td valign="top"><a href="#fixed_point_binary_to_number-3">fixed_point_binary_to_number/3</a></td><td>固定小数点表記のバイナリから<code>number()</code>を生成する.</td></tr><tr><td valign="top"><a href="#format-2">format/2</a></td><td>指定されたフォーマットのバイナリを生成して返す.</td></tr><tr><td valign="top"><a href="#from_hex-1">from_hex/1</a></td><td>16進数表記のバイナリを生のバイナリに変換する.</td></tr><tr><td valign="top"><a href="#generate_random_list-2">generate_random_list/2</a></td><td>ランダム かつ ユニークな要素(バイナリ)を<code>Count</code>個含むリストを生成する.</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>バイナリリストの要素をセパレータで区切ったバイナリを返す.</td></tr><tr><td valign="top"><a href="#number_to_fixed_point_binary-3">number_to_fixed_point_binary/3</a></td><td><code>number()</code>から固定小数点表記のバイナリを生成する.</td></tr><tr><td valign="top"><a href="#strip-1">strip/1</a></td><td>バイナリの両端からスペース(\s)を取り除く(strip(Binary, both)).</td></tr><tr><td valign="top"><a href="#strip-2">strip/2</a></td><td>指定された方向のスペースを取り除く(strip(Binary, Direction, <<"\s">>)).</td></tr><tr><td valign="top"><a href="#strip-3">strip/3</a></td><td>指定された方向から任意の1文字を全て取り除く(strip(Binary, Direction, Target, single)).</td></tr><tr><td valign="top"><a href="#strip-4">strip/4</a></td><td>指定された方向から任意の文字を取り除く.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td>Erlangの項をバイナリに変換する.</td></tr><tr><td valign="top"><a href="#to_float-1">to_float/1</a></td><td>バイナリを小数に変換する.</td></tr><tr><td valign="top"><a href="#to_hex-1">to_hex/1</a></td><td>生のバイナリを16進数表記のバイナリに変換する.</td></tr><tr><td valign="top"><a href="#to_number-1">to_number/1</a></td><td>数値表現のバイナリを、整数もしくは浮動小数点数に変換する．.</td></tr><tr><td valign="top"><a href="#tr-2">tr/2</a></td><td>入力バイナリ内の文字を、マッピング指定に従って置換する.</td></tr><tr><td valign="top"><a href="#try_binary_to_existing_atom-2">try_binary_to_existing_atom/2</a></td><td>バイナリのアトムへの変換を試みる.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="abbreviate-2"></a>

### abbreviate/2 ###

<pre><code>
abbreviate(Bin::binary(), MaxLength::non_neg_integer()) -&gt; binary()
</code></pre>
<br />

Equivalent to [`abbreviate(Bin, MaxLength, <<"...">>)`](#abbreviate-3).

<a name="abbreviate-3"></a>

### abbreviate/3 ###

<pre><code>
abbreviate(Input::binary(), MaxLength::non_neg_integer(), Ellipsis::binary()) -&gt; binary()
</code></pre>
<br />

入力バイナリが最大長を超えている場合に、指定された省略文字列を使って切り詰めを行う

省略文字列が最大長よりも長い場合は、省略時には省略文字列の長さが優先される。(最大長の指定によって、省略文字列自体が切り詰められることはない)

入力バイナリが、UTF-8でエンコードされた日本語等のマルチバイト文字列の場合、省略の際に文字の境界は考慮されないので、
省略によって不正な文字列が生成される可能性があるので注意が必要。

```
  %% 省略される場合
  > abbreviate(<<"hello world">>, 6, <<"...">>).
  <<"hel...">>
 　
  %% 最大長よりも短い場合は、入力バイナリがそのまま返る
  > abbreviate(<<"hello world">>, 100, <<"...">>).
  <<"hello world">>
```

<a name="fill-2"></a>

### fill/2 ###

<pre><code>
fill(Int::integer(), Count::integer()) -&gt; binary()
</code></pre>
<br />

同じ数字(文字)が連続したバイナリを作る.

```
  > fill(0, 10).
  <<0,0,0,0,0,0,0,0,0,0>>
  > fill($a, 10).
  <<"aaaaaaaaaa">>
```

<a name="fixed_point_binary_to_number-3"></a>

### fixed_point_binary_to_number/3 ###

<pre><code>
fixed_point_binary_to_number(IntegerPartLength, DecimalPartLength, Bin::binary()) -&gt; number()
</code></pre>

<ul class="definitions"><li><code>IntegerPartLength = integer()</code></li><li><code>DecimalPartLength = integer()</code></li></ul>

固定小数点表記のバイナリから`number()`を生成する.

固定小数点のバイナリはビッグエンディアン.

```
  1> fixed_point_binary_to_number(16, 16, <<0, 1, 128, 0>>).
  1.5
```

<a name="format-2"></a>

### format/2 ###

<pre><code>
format(Format, Data) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Format = <a href="io.md#type-format">io:format()</a></code></li><li><code>Data = [term()]</code></li></ul>

指定されたフォーマットのバイナリを生成して返す.

`list_to_binary(io_lib:format(Format, Data))`と同等.

<a name="from_hex-1"></a>

### from_hex/1 ###

<pre><code>
from_hex(Encoded::binary()) -&gt; Raw::binary()
</code></pre>
<br />

16進数表記のバイナリを生のバイナリに変換する.
[0-9a-zA-Z]にマッチしない文字がある場合は {invalid_hex_binary, Input} error が発生する

ex:

```
  > moyo_binary:from_hex(<<"61625f595a">>).
  <<"ab_YZ">>
```

<a name="generate_random_list-2"></a>

### generate_random_list/2 ###

<pre><code>
generate_random_list(ByteSize::non_neg_integer(), Count::non_neg_integer()) -&gt; [binary()]
</code></pre>
<br />

ランダム かつ ユニークな要素(バイナリ)を`Count`個含むリストを生成する.

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(Tail::[binary()], Separator::binary()) -&gt; binary()
</code></pre>
<br />

バイナリリストの要素をセパレータで区切ったバイナリを返す.

<a name="number_to_fixed_point_binary-3"></a>

### number_to_fixed_point_binary/3 ###

<pre><code>
number_to_fixed_point_binary(IntegerPartLength, DecimalPartLength, Num::number()) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>IntegerPartLength = integer()</code></li><li><code>DecimalPartLength = integer()</code></li></ul>

`number()`から固定小数点表記のバイナリを生成する.

固定小数点のバイナリはビッグエンディアン.

```
  1> number_to_fixed_point_binary(16, 16, 1.5).
  <<0, 1, 128, 0>>
```

<a name="strip-1"></a>

### strip/1 ###

<pre><code>
strip(Binary::binary()) -&gt; binary()
</code></pre>
<br />

バイナリの両端からスペース(\s)を取り除く(strip(Binary, both)).

<a name="strip-2"></a>

### strip/2 ###

<pre><code>
strip(Binary, Direction) -&gt; Stripped
</code></pre>

<ul class="definitions"><li><code>Binary = binary()</code></li><li><code>Direction = left | right | both</code></li><li><code>Stripped = binary()</code></li></ul>

指定された方向のスペースを取り除く(strip(Binary, Direction, <<"\s">>)).

<a name="strip-3"></a>

### strip/3 ###

<pre><code>
strip(Binary, Direction, Target) -&gt; Stripped
</code></pre>

<ul class="definitions"><li><code>Binary = binary()</code></li><li><code>Direction = left | right | both</code></li><li><code>Target = binary()</code></li><li><code>Stripped = binary()</code></li></ul>

指定された方向から任意の1文字を全て取り除く(strip(Binary, Direction, Target, single)).

<a name="strip-4"></a>

### strip/4 ###

<pre><code>
strip(Binary, Direction, Target, Type) -&gt; Stripped
</code></pre>

<ul class="definitions"><li><code>Binary = binary()</code></li><li><code>Direction = left | right | both</code></li><li><code>Target = binary()</code></li><li><code>Type = single | order | random</code></li><li><code>Stripped = binary()</code></li></ul>

指定された方向から任意の文字を取り除く.

第4引数では次の値を指定.

* single

Targetで指定できる文字は1文字.
* order

Targetで複数文字を指定できる. 指定順序通りの場合のみ取り除く.<br />
ex:

```
  > strip(<<"ababbabcabcabbab">>, both, <<"ab">>, order).
  <<"babcabcabb">>
```

* random

Targetで複数文字を指定できる. 順不同で, 文字それぞれを取り除く.<br />
ex:

```
  > strip(<<"ababbabcabcabbab">>, both, <<"ab">>, random).
  <<"cabc">>
```


<a name="to_binary-1"></a>

### to_binary/1 ###

<pre><code>
to_binary(V::term()) -&gt; binary()
</code></pre>
<br />

Erlangの項をバイナリに変換する

ユニコード値のリスト(文字列)を、UTF-8バイナリ列に変換したい場合は`unicode`モジュールを使用すること.

<a name="to_float-1"></a>

### to_float/1 ###

<pre><code>
to_float(Bin::binary()) -&gt; float()
</code></pre>
<br />

バイナリを小数に変換する.

[`erlang:binary_to_float/1`](erlang.md#binary_to_float-1)とは異なり`<<"5">>`のような整数を表すバイナリも小数に変換可能.

<a name="to_hex-1"></a>

### to_hex/1 ###

<pre><code>
to_hex(Raw::binary()) -&gt; Encoded::binary()
</code></pre>
<br />

生のバイナリを16進数表記のバイナリに変換する.

16進数のアルファベット部分は、常に小文字が使用される. <br />
ex:

```
  > moyo_binary:to_hex(<<"ab_YZ">>).
  <<"61625f595a">>
```

<a name="to_number-1"></a>

### to_number/1 ###

<pre><code>
to_number(Bin::binary()) -&gt; number()
</code></pre>
<br />

数値表現のバイナリを、整数もしくは浮動小数点数に変換する．

引数で与えられたバイナリが整数表現だった場合は整数に、小数表現だった場合は浮動小数点数に変換する.
整数表現、小数表現のいずれでもなかった場合は badarg を投げる．

<a name="tr-2"></a>

### tr/2 ###

<pre><code>
tr(Subject::binary(), ConvertMapping) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>ConvertMapping = [{From::char(), To::char()}]</code></li></ul>

入力バイナリ内の文字を、マッピング指定に従って置換する.

```
  > tr(<<"abcdef">>, [{$a, $1}, {$c, $3}]).
  <<"1b3def">>
```

<a name="try_binary_to_existing_atom-2"></a>

### try_binary_to_existing_atom/2 ###

<pre><code>
try_binary_to_existing_atom(Binary::binary(), Encoding) -&gt; binary() | atom()
</code></pre>

<ul class="definitions"><li><code>Encoding = latin1 | unicode | utf8</code></li></ul>

バイナリのアトムへの変換を試みる.

バイナリに対応するアトムが既に存在する場合は、そのアトムを返し、存在しない場合は元のバイナリを返す.

