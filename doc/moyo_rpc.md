

# Module moyo_rpc #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

ノード間のやりとりをするユーティリティ関数.

Copyright (c) 2017 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ensure_loaded-2">ensure_loaded/2</a></td><td>Equivalent to <a href="#ensure_loaded-3"><tt>ensure_loaded(Node, Module, infinity)</tt></a>.</td></tr><tr><td valign="top"><a href="#ensure_loaded-3">ensure_loaded/3</a></td><td>指定したノードにおいて, 指定したモジュールが確実にロードされるようにする.</td></tr><tr><td valign="top"><a href="#function_exported-2">function_exported/2</a></td><td>Equivalent to <a href="#function_exported-3"><tt>function_exported(Node, MFA, infinity)</tt></a>.</td></tr><tr><td valign="top"><a href="#function_exported-3">function_exported/3</a></td><td>指定したノードに指定した関数が存在するかどうかを確認する.</td></tr><tr><td valign="top"><a href="#is_function_callable-2">is_function_callable/2</a></td><td>Equivalent to <a href="#is_function_callable-3"><tt>is_function_callable(Node, MFA, infinity)</tt></a>.</td></tr><tr><td valign="top"><a href="#is_function_callable-3">is_function_callable/3</a></td><td>指定したノードにおいて, 指定した関数が呼び出し可能かどうかを確認する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ensure_loaded-2"></a>

### ensure_loaded/2 ###

<pre><code>
ensure_loaded(Node::node(), Module::module()) -&gt; {ok, {module, Module}} | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Module = module()</code></li></ul>

Equivalent to [`ensure_loaded(Node, Module, infinity)`](#ensure_loaded-3).

<a name="ensure_loaded-3"></a>

### ensure_loaded/3 ###

<pre><code>
ensure_loaded(Node::node(), Module, Timeout::integer() | infinity) -&gt; {ok, {module, Module}} | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Module = module()</code></li></ul>

returns:

ノードとの通信に成功した場合, 元々モジュールがロードされていた場合や,
モジュールのロードに成功した場合には`{ok, {module Module}}`を,
失敗すれば`{error, Reason}`を返す.
`Reason`は`code:ensure_loaded/1`と同様のものとなる.

ノードとの通信に失敗した場合, `{error, {badrpc, BadRpcReason}}`を返す.
`BadRpcReason`は`rpc:call/5`と同様のものとなる.

指定したノードにおいて, 指定したモジュールが確実にロードされるようにする.

`code:ensure_loaded/1`のRPC版.

<a name="function_exported-2"></a>

### function_exported/2 ###

<pre><code>
function_exported(Node::node(), MFA::mfa()) -&gt; {ok, boolean()} | {error, term()}
</code></pre>
<br />

Equivalent to [`function_exported(Node, MFA, infinity)`](#function_exported-3).

<a name="function_exported-3"></a>

### function_exported/3 ###

<pre><code>
function_exported(Node::node(), MFA::mfa(), Timeout::integer() | infinity) -&gt; {ok, boolean()} | {error, term()}
</code></pre>
<br />

returns:

ノードとの通信に成功した場合, 指定された関数が存在すれば`{ok, true}`, 存在しなければ`{ok, false}`を返す.

ノードとの通信に失敗した場合, `{error, {badrpc, BadRpcReason}}`を返す.
`BadRpcReason`は`rpc:call/5`と同様のものとなる.

指定したノードに指定した関数が存在するかどうかを確認する.

`erlang:function_exported/3`のRPC版.

<a name="is_function_callable-2"></a>

### is_function_callable/2 ###

<pre><code>
is_function_callable(Node::node(), MFA::mfa()) -&gt; {ok, boolean()} | {error, nodedown | timeout | term()}
</code></pre>
<br />

Equivalent to [`is_function_callable(Node, MFA, infinity)`](#is_function_callable-3).

<a name="is_function_callable-3"></a>

### is_function_callable/3 ###

<pre><code>
is_function_callable(Node::node(), MFA::mfa(), Timeout::integer() | infinity) -&gt; {ok, boolean()} | {error, term()}
</code></pre>
<br />

returns:

ノードとの通信に成功した場合, 指定された関数が呼び出し可能ならば`{ok, true}`, 不可能ならば`{ok, false}`を返す.

ノードとの通信に失敗した場合, `{error, {badrpc, BadRpcReason}}`を返す.
`BadRpcReason`は`rpc:call/5`と同様のものとなる.
代表的なものは以下の通り:



<dt>nodedown</dt>



<dd>ノードが落ちている</dd>




<dt>timeout</dt>



<dd>タイムアウトした</dd>



指定したノードにおいて, 指定した関数が呼び出し可能かどうかを確認する.

[`function_exported/3`](#function_exported-3)の場合, まだロードされていないモジュールの関数に対しては`{ok, false}`を返すため,
その関数を呼び出せるかどうかの判定としては不十分である.
そこで`is_function_callable/3`では, 指定したモジュールのロードを試みた後に,
指定した関数が存在するかどうかを確認する. これにより, 呼び出し可能かどうかを確実に判定することができる.

