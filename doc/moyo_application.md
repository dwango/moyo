

# Module moyo_application #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

アプリケーション関連の処理を集めたモジュール.

Copyright (c) 2013-2015 DWANGO Co., Ltd. All Rights Reserved.

<a name="types"></a>

## Data Types ##




### <a name="type-name">name()</a> ###


<pre><code>
name() = atom()
</code></pre>

アプリケーション名

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ensure_all_loaded-1">ensure_all_loaded/1</a></td><td>指定されたアプリケーションおよびそれが依存するプリケーション群がロードされているようにする.</td></tr><tr><td valign="top"><a href="#ensure_all_unloaded-1">ensure_all_unloaded/1</a></td><td><code>Pred(Application)</code>が<code>true</code>を返したアプリケーションを全て停止してアンロードする.</td></tr><tr><td valign="top"><a href="#ensure_loaded-1">ensure_loaded/1</a></td><td>指定されたアプリケーションが確実にロードされているようにする.</td></tr><tr><td valign="top"><a href="#get_key-3">get_key/3</a></td><td><a href="applications.md#get_key-2"><code>applications:get_key/2</code></a>にデフォルト値を指定可能にしたもの.</td></tr><tr><td valign="top"><a href="#get_priv_dir-1">get_priv_dir/1</a></td><td><a href="code.md#priv_dir-1"><code>code:priv_dir/1</code></a>の代替となる関数。.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ensure_all_loaded-1"></a>

### ensure_all_loaded/1 ###

<pre><code>
ensure_all_loaded(Application::<a href="#type-name">name()</a>) -&gt; {ok, Loaded} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Loaded = <a href="ordsets.md#type-ordset">ordsets:ordset</a>(<a href="#type-name">name()</a>)</code></li><li><code>Reason = term()</code></li></ul>

指定されたアプリケーションおよびそれが依存するプリケーション群がロードされているようにする

`Loaded`は、関数呼び出し中に新規にロードされたアプリケーション群

<a name="ensure_all_unloaded-1"></a>

### ensure_all_unloaded/1 ###

<pre><code>
ensure_all_unloaded(Pred::fun((Application::atom()) -&gt; boolean())) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

`Pred(Application)`が`true`を返したアプリケーションを全て停止してアンロードする.

[`application:loaded_applications/0`](application.md#loaded_applications-0)の結果に基づき、<br />
ロードされているアプリケーションについて`Pred(Application)`を呼び出し、<br />
`Pred(Application)`が`true`を返したアプリケーションを全て停止してアンロードする.

一つでもアンロードに失敗した場合は、即座に`{error, Reason}`を返す.

※`kernel`と`stdlib`はアンロードしない.

<a name="ensure_loaded-1"></a>

### ensure_loaded/1 ###

<pre><code>
ensure_loaded(Application::<a href="#type-name">name()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = term()</code></li></ul>

指定されたアプリケーションが確実にロードされているようにする

[`application:load/1`](application.md#load-1)とは異なり、既にロード済みのアプリケーションが指定された場合は`ok`が返される

<a name="get_key-3"></a>

### get_key/3 ###

<pre><code>
get_key(Application::<a href="#type-name">name()</a>, Key::atom(), DefaultValue::term()) -&gt; Value::term()
</code></pre>
<br />

[`applications:get_key/2`](applications.md#get_key-2)にデフォルト値を指定可能にしたもの

<a name="get_priv_dir-1"></a>

### get_priv_dir/1 ###

<pre><code>
get_priv_dir(Application::<a href="#type-name">name()</a>) -&gt; {ok, <a href="file.md#type-filename">file:filename()</a>} | {error, bad_name}
</code></pre>
<br />

[`code:priv_dir/1`](code.md#priv_dir-1)の代替となる関数。

標準あるいはERL_LIBS環境変数で指定されたディレクトリ以下に指定したアプリケーションが存在せず
code:priv_dirに失敗した場合もprivディレクトリを推測して値を返す

