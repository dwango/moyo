

# Module moyo_frac #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

分数を扱うモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="description"></a>

## Description ##
分母は符号を持たないように、分子に符号を保持するようにする.

<a name="types"></a>

## Data Types ##




### <a name="type-fraction">fraction()</a> ###


__abstract datatype__: `fraction()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td>足し算 数(分数|浮動小数点数|整数)同士.</td></tr><tr><td valign="top"><a href="#comp-2">comp/2</a></td><td>分数の比較.</td></tr><tr><td valign="top"><a href="#denom-1">denom/1</a></td><td>分母を返す.</td></tr><tr><td valign="top"><a href="#divide-2">divide/2</a></td><td>割り算 数(分数|浮動小数点数|整数)同士.</td></tr><tr><td valign="top"><a href="#from_binary-1">from_binary/1</a></td><td>バイナリの整数、負数、分数、小数を分数に変換する.</td></tr><tr><td valign="top"><a href="#from_float-1">from_float/1</a></td><td>浮動小数点数を分数に変換する.</td></tr><tr><td valign="top"><a href="#from_float_binary-1">from_float_binary/1</a></td><td>バイナリ形式の小数を分数に変換する.</td></tr><tr><td valign="top"><a href="#from_integer-1">from_integer/1</a></td><td>整数を分数に変換する.</td></tr><tr><td valign="top"><a href="#from_number-1">from_number/1</a></td><td>数値(整数|浮動小数点数)を分数に変換する.</td></tr><tr><td valign="top"><a href="#is_fraction-1">is_fraction/1</a></td><td>分数かどうかの判定.</td></tr><tr><td valign="top"><a href="#max-2">max/2</a></td><td>大きい方の値を返す.</td></tr><tr><td valign="top"><a href="#min-2">min/2</a></td><td>小さい方の値を返す.</td></tr><tr><td valign="top"><a href="#mul-2">mul/2</a></td><td>掛け算 数(分数|浮動小数点数|整数)同士.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>分数を生成する.</td></tr><tr><td valign="top"><a href="#num-1">num/1</a></td><td>分子を返す.</td></tr><tr><td valign="top"><a href="#sub-2">sub/2</a></td><td>引き算 数(分数|浮動小数点数|整数)同士.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td>分数をバイナリに変換する.</td></tr><tr><td valign="top"><a href="#to_float-1">to_float/1</a></td><td>分数を浮動小数点数に変換する.</td></tr><tr><td valign="top"><a href="#to_integer-1">to_integer/1</a></td><td>整数部分を返す.</td></tr><tr><td valign="top"><a href="#to_tuple-1">to_tuple/1</a></td><td>分数をタプル({分子, 分母})に変換する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###

<pre><code>
add(A::<a href="#type-fraction">fraction()</a> | number(), B::<a href="#type-fraction">fraction()</a> | number()) -&gt; R::<a href="#type-fraction">fraction()</a>
</code></pre>
<br />

足し算 数(分数|浮動小数点数|整数)同士.

9パターンに対応.

<a name="comp-2"></a>

### comp/2 ###

<pre><code>
comp(A::<a href="#type-fraction">fraction()</a>, B::<a href="#type-fraction">fraction()</a>) -&gt; R::integer()
</code></pre>
<br />

分数の比較.

Aの方が大きい場合は正整数(1),
Bの方が大きい場合は負整数(-1),
同じ場合は0を返す.<br />
1, -1ではなく, 正整数, 負整数でチェックすること.

<a name="denom-1"></a>

### denom/1 ###

<pre><code>
denom(Frac::<a href="#type-fraction">fraction()</a>) -&gt; Denom::integer()
</code></pre>
<br />

分母を返す.

<a name="divide-2"></a>

### divide/2 ###

<pre><code>
divide(A::<a href="#type-fraction">fraction()</a> | number(), B::<a href="#type-fraction">fraction()</a> | number()) -&gt; R::<a href="#type-fraction">fraction()</a>
</code></pre>
<br />

割り算 数(分数|浮動小数点数|整数)同士.

9パターンに対応.

<a name="from_binary-1"></a>

### from_binary/1 ###

<pre><code>
from_binary(Binary::binary()) -&gt; <a href="#type-fraction">fraction()</a>
</code></pre>
<br />

バイナリの整数、負数、分数、小数を分数に変換する.

ex:

```
  1> moyo_frac:from_binary(<<"5">>).
  {fraction, 5, 1}
  2> moyo_frac:from_binary(<<"-5">>).
  {fraction, -5, 1}
  3> moyo_frac:from_binary(<<"1/5">>).
  {fraction, 1, 5}.
  4> moyo_frac:from_binary(<<"0.25">>).
  {fraction, 1, 4}
```

<a name="from_float-1"></a>

### from_float/1 ###

<pre><code>
from_float(Float::float()) -&gt; <a href="#type-fraction">fraction()</a>
</code></pre>
<br />

浮動小数点数を分数に変換する.

floatの精度の問題で同じと思っている値を比べても同じと判断されない場合がある.

ex:

```
  1> moyo_frac:comp(moyo_frac:from_float(3.14), moyo_frac:new(314, 100)).
  1
```

非正規化数を分数にすることはできるが, to_floatすると割り算でerrorが発生する.
無限大, NaNに関してはErlangのfloatでは使えないので考慮していない.

<a name="from_float_binary-1"></a>

### from_float_binary/1 ###

<pre><code>
from_float_binary(DecimalBin::binary()) -&gt; <a href="#type-fraction">fraction()</a>
</code></pre>
<br />

バイナリ形式の小数を分数に変換する.

小数(有理数)を分数にする関数.
文字列で見えている範囲で分数にする.<br />
ex: `3.1415 -> 31415/10000 -> 6283/2000`<br />
注: 約分は行われる.

ex:

```
  1> moyo_frac:from_float_binary(<<"3.1415">>).
  {fraction, 6283, 2000}
```

<a name="from_integer-1"></a>

### from_integer/1 ###

<pre><code>
from_integer(Int::integer()) -&gt; Frac::<a href="#type-fraction">fraction()</a>
</code></pre>
<br />

整数を分数に変換する.

<a name="from_number-1"></a>

### from_number/1 ###

<pre><code>
from_number(Number::number()) -&gt; <a href="#type-fraction">fraction()</a>
</code></pre>
<br />

数値(整数|浮動小数点数)を分数に変換する.

<a name="is_fraction-1"></a>

### is_fraction/1 ###

<pre><code>
is_fraction(Fraction::term()) -&gt; boolean()
</code></pre>
<br />

分数かどうかの判定.

<a name="max-2"></a>

### max/2 ###

<pre><code>
max(A::<a href="#type-fraction">fraction()</a>, B::<a href="#type-fraction">fraction()</a>) -&gt; M::<a href="#type-fraction">fraction()</a>
</code></pre>
<br />

大きい方の値を返す.

<a name="min-2"></a>

### min/2 ###

<pre><code>
min(A::<a href="#type-fraction">fraction()</a>, B::<a href="#type-fraction">fraction()</a>) -&gt; M::<a href="#type-fraction">fraction()</a>
</code></pre>
<br />

小さい方の値を返す.

<a name="mul-2"></a>

### mul/2 ###

<pre><code>
mul(A::<a href="#type-fraction">fraction()</a> | number(), B::<a href="#type-fraction">fraction()</a> | number()) -&gt; R::<a href="#type-fraction">fraction()</a>
</code></pre>
<br />

掛け算 数(分数|浮動小数点数|整数)同士.

9パターンに対応.

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Num::integer(), Denom::integer()) -&gt; <a href="#type-fraction">fraction()</a>
</code></pre>
<br />

分数を生成する.

分母に0を与えられた場合, `denominator_0_error`をthrowする.

<a name="num-1"></a>

### num/1 ###

<pre><code>
num(Frac::<a href="#type-fraction">fraction()</a>) -&gt; Num::integer()
</code></pre>
<br />

分子を返す.

<a name="sub-2"></a>

### sub/2 ###

<pre><code>
sub(A::<a href="#type-fraction">fraction()</a> | number(), B::<a href="#type-fraction">fraction()</a> | number()) -&gt; R::<a href="#type-fraction">fraction()</a>
</code></pre>
<br />

引き算 数(分数|浮動小数点数|整数)同士.

9パターンに対応.

<a name="to_binary-1"></a>

### to_binary/1 ###

<pre><code>
to_binary(Fraction::<a href="#type-fraction">fraction()</a>) -&gt; binary()
</code></pre>
<br />

分数をバイナリに変換する.

<a name="to_float-1"></a>

### to_float/1 ###

<pre><code>
to_float(Frac::<a href="#type-fraction">fraction()</a>) -&gt; Float::float()
</code></pre>
<br />

分数を浮動小数点数に変換する.

<a name="to_integer-1"></a>

### to_integer/1 ###

<pre><code>
to_integer(Frac::<a href="#type-fraction">fraction()</a>) -&gt; Int::integer()
</code></pre>
<br />

整数部分を返す.

<a name="to_tuple-1"></a>

### to_tuple/1 ###

<pre><code>
to_tuple(Frac::<a href="#type-fraction">fraction()</a>) -&gt; {Num::integer(), Denom::integer()}
</code></pre>
<br />

分数をタプル({分子, 分母})に変換する.

