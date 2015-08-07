

# Module moyo_command #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

外部コマンドの実行に関する処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="types"></a>

## Data Types ##




### <a name="type-argument">argument()</a> ###


<pre><code>
argument() = iodata() | integer() | {iodata(), [<a href="#type-argument_option">argument_option()</a>, ...]} | <a href="#type-option_element">option_element()</a>
</code></pre>

 外部プログラムに渡す引数.



### <a name="type-argument_option">argument_option()</a> ###


<pre><code>
argument_option() = long_option | equal | escape | <a href="#type-float_option">float_option()</a>
</code></pre>

 引数に関するオプション.



### <a name="type-command">command()</a> ###


<pre><code>
command() = <a href="#type-iodata_or_atom">iodata_or_atom()</a>
</code></pre>

 外部プログラムの実行パス.



### <a name="type-destination_file_path">destination_file_path()</a> ###


<pre><code>
destination_file_path() = binary()
</code></pre>

 標準出力/エラー出力の出力先.



### <a name="type-float_option">float_option()</a> ###



<pre><code>
float_option() = {decimals, Decimals::0..253} | {scientific, Decimals::0..249} | compact
</code></pre>



  小数パラメータに関するオプション.



### <a name="type-iodata_or_atom">iodata_or_atom()</a> ###


<pre><code>
iodata_or_atom() = iodata() | atom()
</code></pre>

 iodata(), または, atom().



### <a name="type-option">option()</a> ###


<pre><code>
option() = <a href="#type-port_settings">port_settings()</a> | escape_all | {stdout, <a href="#type-destination_file_path">destination_file_path()</a> | stderr} | {stderr, <a href="#type-destination_file_path">destination_file_path()</a> | stdout} | discard_stderr | {timeout, <a href="#type-time">time()</a>} | {nice, integer()} | {close_function, fun((port()) -&gt; ok)} | {stdout_hook_fun, {<a href="#type-stdout_hook_fun">stdout_hook_fun()</a>, term()}}
</code></pre>

 execute/2, execute/3に指定できるオプション.



### <a name="type-option_argument">option_argument()</a> ###


<pre><code>
option_argument() = iodata() | integer() | none
</code></pre>

 オプションの引数部分.



### <a name="type-option_character">option_character()</a> ###


<pre><code>
option_character() = <a href="#type-iodata_or_atom">iodata_or_atom()</a>
</code></pre>

 オプション文字.



### <a name="type-option_element">option_element()</a> ###


<pre><code>
option_element() = {<a href="#type-option_character">option_character()</a>, <a href="#type-option_argument">option_argument()</a>} | {<a href="#type-option_character">option_character()</a>, <a href="#type-option_argument">option_argument()</a>, [<a href="#type-argument_option">argument_option()</a>]}
</code></pre>

 外部プログラムに渡す引数の中でオプションとして指定するもの.



### <a name="type-port_settings">port_settings()</a> ###


<pre><code>
port_settings() = term()
</code></pre>

open_portに指定できるオプション.



### <a name="type-stdout_hook_fun">stdout_hook_fun()</a> ###


<pre><code>
stdout_hook_fun() = fun((binary() | {eol | noeol, binary()}, term()) -&gt; term() | binary()) | fun((exit, term()) -&gt; binary())
</code></pre>

 標準出力に対するフィルタ関数.



### <a name="type-time">time()</a> ###


<pre><code>
time() = non_neg_integer()
</code></pre>

 timeoutに指定できる数字.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#escape_shell_arg-1">escape_shell_arg/1</a></td><td>シングルクォーテーションで両端を囲み、バイナリ中のシングルクォーテーションをエスケープする.</td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td>外部コマンドを実行する.</td></tr><tr><td valign="top"><a href="#execute-3">execute/3</a></td><td>外部コマンドを実行する.</td></tr><tr><td valign="top"><a href="#generate_command-2">generate_command/2</a></td><td>オプションリストからコマンド文字列(バイナリ)を生成する.</td></tr><tr><td valign="top"><a href="#generate_command-3">generate_command/3</a></td><td>オプションリストからコマンド文字列(バイナリ)を生成する.</td></tr><tr><td valign="top"><a href="#reduce_parameters-1">reduce_parameters/1</a></td><td>パラメータリストを整理する.</td></tr><tr><td valign="top"><a href="#reduce_parameters-2">reduce_parameters/2</a></td><td>パラメータリストを整理する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="escape_shell_arg-1"></a>

### escape_shell_arg/1 ###

<pre><code>
escape_shell_arg(Binary::binary()) -&gt; EscapedBinary::binary()
</code></pre>
<br />

シングルクォーテーションで両端を囲み、バイナリ中のシングルクォーテーションをエスケープする.

<a name="execute-2"></a>

### execute/2 ###

<pre><code>
execute(Command::<a href="#type-command">command()</a>, ArgumentList::[<a href="#type-argument">argument()</a>]) -&gt; {{ok, Output} | {error, Reason}, FullCommandBinary}
</code></pre>

<ul class="definitions"><li><code>Output = binary()</code></li><li><code>Reason = term()</code></li><li><code>FullCommandBinary = binary()</code></li></ul>

外部コマンドを実行する.

<a name="execute-3"></a>

### execute/3 ###

<pre><code>
execute(Command::<a href="#type-command">command()</a>, ArgumentList::[<a href="#type-argument">argument()</a>], OptionList::[<a href="#type-option">option()</a>]) -&gt; {{ok, Output} | {error, Reason}, FullCommandBinary}
</code></pre>

<ul class="definitions"><li><code>Output = binary()</code></li><li><code>Reason = timeout | {exit_status, Status}</code></li><li><code>Status = integer()</code></li><li><code>FullCommandBinary = binary()</code></li></ul>

外部コマンドを実行する.

オプション
generate_commandのオプション + open_portのオプション

<a name="generate_command-2"></a>

### generate_command/2 ###

<pre><code>
generate_command(Command::<a href="#type-command">command()</a>, ArgumentList::[<a href="#type-argument">argument()</a>]) -&gt; binary()
</code></pre>
<br />

オプションリストからコマンド文字列(バイナリ)を生成する.

<a name="generate_command-3"></a>

### generate_command/3 ###

<pre><code>
generate_command(Command::<a href="#type-command">command()</a>, ArgumentList::[<a href="#type-argument">argument()</a>], OptionList::[<a href="#type-option">option()</a>]) -&gt; binary()
</code></pre>
<br />

オプションリストからコマンド文字列(バイナリ)を生成する.

【argument option】<br />
● `long_option`: long optionにする("--"でオプションを指定する.).<br />
● `equal`      : オプション文字とオプション引数の間を"="で繋ぐ.<br />
● `escape`     : オプション引数をシングルクォーテーションでエスケープする.<br />



【argument option (小数)】<br />
● `{scientific, 0..253}` : 小数を指数表記で出力する.数字は有効桁数.<br />
● `{decimals, 0..249}`   : 小数を実数表記で出力する.数字は有効桁数.<br />
● `compact`              : 後端のゼロを省く.<br />
(\*) デフォルトは`[{decimals, 4}]`<br />
(\*) 表記方法が複数指定されている場合,最も後に指定された表記方法が採用される.<br />

【option】<br />
● `escape_all`            : 全てのオプション引数をエスケープする.<br />
● `{stdout, Destination}` : 標準出力を指定先のファイルに出力する.
Destinationには出力ファイル先を指定する.<br />
● `{stderr, Destination}` : 標準エラー出力を指定先のファイルに出力する.
Destinationには出力ファイル先を指定する.<br />
● `discard_stderr`        : 標準エラー出力を/dev/nullに捨てる.<br />
● `{timeout, Time}`       : Time `ミリ秒` で処理が終わらなかった場合, タイムアウトする.<br />
● `{close_function, Fun}` : timeoutオプションでタイムアウトした時の処理を明示的に指定する.<br />
● `{stdout_hook_fun, {Fun, Init}}` : 標準出力をフィルタリングする.
Initに初期値を, Funは2引数の関数で第1引数に`exit`が来た場合は`binary`を返す.

<a name="reduce_parameters-1"></a>

### reduce_parameters/1 ###

<pre><code>
reduce_parameters(Parameters::[any()]) -&gt; ValidParameters::[any()]
</code></pre>
<br />

パラメータリストを整理する.

パラメータリストから`不必要な要素`を除く.
`不必要な要素`のdefaultは, `undefined`とする.

<a name="reduce_parameters-2"></a>

### reduce_parameters/2 ###

<pre><code>
reduce_parameters(Parameters::[any()], UnnecessaryParameters::[any()]) -&gt; ValidParameters::[any()]
</code></pre>
<br />

パラメータリストを整理する.

パラメータリストから不必要な要素を除く.

