

# Module moyo_math #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

数学的な関数を集めたモジュール.

Copyright (c) 2013-2015 DWANGO Co., Ltd. All Rights Reserved.

<a name="types"></a>

## Data Types ##




### <a name="type-random_sequence_symbols">random_sequence_symbols()</a> ###


<pre><code>
random_sequence_symbols() = alphabetical | numeric | alphanumeric
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ceil-1">ceil/1</a></td><td>数(number)を切り上げて整数を返す.</td></tr><tr><td valign="top"><a href="#divmod-2">divmod/2</a></td><td>除算した商と剰余を求める関数.</td></tr><tr><td valign="top"><a href="#floor-1">floor/1</a></td><td>数(number)を切り下げて整数を返す.</td></tr><tr><td valign="top"><a href="#gcd-2">gcd/2</a></td><td>最大公約数を求める.</td></tr><tr><td valign="top"><a href="#pow_int-2">pow_int/2</a></td><td>累乗関数.</td></tr><tr><td valign="top"><a href="#random_sequence-1">random_sequence/1</a></td><td>Equivalent to <a href="#random_sequence-2"><tt>random_sequence(Length, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#random_sequence-2">random_sequence/2</a></td><td>ランダム文字列を返す.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ceil-1"></a>

### ceil/1 ###

<pre><code>
ceil(Number::number()) -&gt; integer()
</code></pre>
<br />

数(number)を切り上げて整数を返す.

指定した以上の整数で最小のものを返す.

```
  > ceil(1.0).
  1.
  > ceil(0.5).
  1.
  > ceil(0.0).
  0.
  > ceil(-0.5).
  0.
  > ceil(-1.0).
  -1.
```

<a name="divmod-2"></a>

### divmod/2 ###

<pre><code>
divmod(A::integer(), B::integer()) -&gt; {Quotient::integer(), Remainder::integer()}
</code></pre>
<br />

除算した商と剰余を求める関数.

除数が0である場合, `badarith` errorが発生する.

<a name="floor-1"></a>

### floor/1 ###

<pre><code>
floor(Number::number()) -&gt; integer()
</code></pre>
<br />

数(number)を切り下げて整数を返す.

指定した以下の整数で最大のものを返す.

```
  > floor(1.0).
  1.
  > floor(0.5).
  0.
  > floor(0.0).
  0.
  > floor(-0.5).
  -1.
  > floor(-1.0).
  -1.
```

<a name="gcd-2"></a>

### gcd/2 ###

<pre><code>
gcd(A::integer(), B::integer()) -&gt; GCD::integer()
</code></pre>
<br />

最大公約数を求める.

両方の引数が0の場合, `both_0_error`をthrowする.

<a name="pow_int-2"></a>

### pow_int/2 ###

<pre><code>
pow_int(Base::integer(), Exponent::non_neg_integer()) -&gt; Value::integer()
</code></pre>
<br />

累乗関数.

計算結果がinteger()になる計算のみ行える.
具体的には、引数は整数のみで、第2引数は0以上のみを扱う.

<a name="random_sequence-1"></a>

### random_sequence/1 ###

<pre><code>
random_sequence(Length::non_neg_integer()) -&gt; binary()
</code></pre>
<br />

Equivalent to [`random_sequence(Length, [])`](#random_sequence-2).

<a name="random_sequence-2"></a>

### random_sequence/2 ###

<pre><code>
random_sequence(Length::non_neg_integer(), Options) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Options = [{symbol, Symbols}]</code></li><li><code>Symbols = <a href="#type-random_sequence_symbols">random_sequence_symbols()</a></code></li></ul>

ランダム文字列を返す

DataTypeで出力形式を指定し、Symbolで出力内容を指定する．

