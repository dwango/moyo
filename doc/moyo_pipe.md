

# Module moyo_pipe #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


ポート(外部コマンド)に対するパイプ入出力機能を提供するためのモジュール.
Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="description"></a>

## Description ##


現在は出力機能にのみ対応済み
<a name="types"></a>

## Data Types ##




### <a name="type-output_data_generate_fun">output_data_generate_fun()</a> ###



<pre><code>
output_data_generate_fun() = fun((State::term()) -&gt; {ok, OutputData::iodata(), NextState::term()} | stop)
</code></pre>





### <a name="type-output_option">output_option()</a> ###



<pre><code>
output_option() = {interval, <a href="moyo_clock.md#type-non_neg_milliseconds">moyo_clock:non_neg_milliseconds()</a>}
</code></pre>



  interval: 各出力データ送信後にスリープする時間(ミリ秒).  デフォルト値は 10.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#output_start-3">output_start/3</a></td><td>指定ポートに対して固定データを出力し続けるプロセスを生成する.</td></tr><tr><td valign="top"><a href="#output_start-4">output_start/4</a></td><td>指定ポートに対してデータ出力を行い続けるプロセスを生成する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="output_start-3"></a>

### output_start/3 ###


<pre><code>
output_start(Port::port(), Data, Options) -&gt; OutputProcessId
</code></pre>

<ul class="definitions"><li><code>Data = iodata()</code></li><li><code>Options = [<a href="#type-output_option">output_option()</a>]</code></li><li><code>OutputProcessId = pid()</code></li></ul>


指定ポートに対して固定データを出力し続けるプロセスを生成する.


`output_start(Port, fun (State) -> {ok, Data, State} end, InitialState, Options)`と等価なので、詳細はそちらのドキュメントを参照. <br />
<a name="output_start-4"></a>

### output_start/4 ###


<pre><code>
output_start(Port::port(), DataGenerateFun, InitialState, Options) -&gt; OutputProcessId
</code></pre>

<ul class="definitions"><li><code>DataGenerateFun = <a href="#type-output_data_generate_fun">output_data_generate_fun()</a></code></li><li><code>InitialState = term()</code></li><li><code>Options = [<a href="#type-output_option">output_option()</a>]</code></li><li><code>OutputProcessId = pid()</code></li></ul>


指定ポートに対してデータ出力を行い続けるプロセスを生成する.


生成されたプロセスは、ポートの実行終了に伴い、自動で終了する. <br />
また`DataGenerateFun`が`stop`を返した場合もプロセスは終了する (この際にポートの停止は行われない). <br />
