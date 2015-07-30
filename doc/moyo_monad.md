

# Module moyo_monad #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

モナドに関する処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="types"></a>

## Data Types ##




### <a name="type-maybe">maybe()</a> ###


<pre><code>
maybe() = {just, term()} | nothing
</code></pre>

 なにか or Nothing を格納するデータ構造



### <a name="type-maybe_fun">maybe_fun()</a> ###


<pre><code>
maybe_fun() = {just, function()} | nothing
</code></pre>

 何らかの関数 or Nothing を格納するデータ構造

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_maybe-3">apply_maybe/3</a></td><td>MaybeFunが{just, Function}なら apply(Function, ArgList) を実行します.</td></tr><tr><td valign="top"><a href="#maybe-1">maybe/1</a></td><td>maybe()を作ります.</td></tr><tr><td valign="top"><a href="#maybe_fun-1">maybe_fun/1</a></td><td>maybe()を作ります.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_maybe-3"></a>

### apply_maybe/3 ###

<pre><code>
apply_maybe(MaybeFun::<a href="#type-maybe_fun">maybe_fun()</a>, ArgList::[any()], DefaultValue::any()) -&gt; any()
</code></pre>
<br />

MaybeFunが{just, Function}なら apply(Function, ArgList) を実行します. MaybeFunがnothingならDefaultValue を返します

<a name="maybe-1"></a>

### maybe/1 ###

<pre><code>
maybe(Value::term()) -&gt; <a href="#type-maybe">maybe()</a>
</code></pre>
<br />

maybe()を作ります.

{just nothing} は作れないので直接作ってください.

<a name="maybe_fun-1"></a>

### maybe_fun/1 ###

<pre><code>
maybe_fun(Fun::nothing | function()) -&gt; <a href="#type-maybe">maybe()</a>
</code></pre>
<br />

maybe()を作ります.

