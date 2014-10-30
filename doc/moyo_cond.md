

# Module moyo_cond #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


条件分岐処理関連のユーティリティ関数を提供するモジュール.
Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_if-3">apply_if/3</a></td><td><code>Condition</code>が<code>true</code>の場合は<code>ThenFun</code>が、<code>false</code>の場合は<code>ElseFun</code>が実行される.</td></tr><tr><td valign="top"><a href="#apply_unless-2">apply_unless/2</a></td><td><code>Condition</code>が<code>false</code>の場合は<code>ThenFun</code>が実行される.</td></tr><tr><td valign="top"><a href="#apply_when-2">apply_when/2</a></td><td><code>Condition</code>が<code>true</code>の場合は<code>ThenFun</code>が実行される.</td></tr><tr><td valign="top"><a href="#conditional-3">conditional/3</a></td><td>三項演算子と同様の機能を提供する。Conditionがtrueなら２つ目の値が、falseなら３つ目の値が返る.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_if-3"></a>

### apply_if/3 ###


<pre><code>
apply_if(Condition::boolean(), ThenFun, ElseFun) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>ThenFun = fun(() -&gt; Result)</code></li><li><code>ElseFun = fun(() -&gt; Result)</code></li><li><code>Result = term()</code></li></ul>

`Condition`が`true`の場合は`ThenFun`が、`false`の場合は`ElseFun`が実行される
<a name="apply_unless-2"></a>

### apply_unless/2 ###


<pre><code>
apply_unless(Condition::boolean(), ThenFun) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>ThenFun = fun(() -&gt; any())</code></li></ul>


`Condition`が`false`の場合は`ThenFun`が実行される


返り値は常に`ok`
<a name="apply_when-2"></a>

### apply_when/2 ###


<pre><code>
apply_when(Condition::boolean(), ThenFun) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>ThenFun = fun(() -&gt; any())</code></li></ul>


`Condition`が`true`の場合は`ThenFun`が実行される


返り値は常に`ok`
<a name="conditional-3"></a>

### conditional/3 ###


<pre><code>
conditional(Condition::boolean(), TValue::any(), FValue::any()) -&gt; any()
</code></pre>
<br />

三項演算子と同様の機能を提供する。Conditionがtrueなら２つ目の値が、falseなら３つ目の値が返る
