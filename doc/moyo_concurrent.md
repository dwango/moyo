

# Module moyo_concurrent #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

並行処理の為のモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.3409;0c

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exec-1">exec/1</a></td><td>並行に複数のコマンドを実行する.</td></tr><tr><td valign="top"><a href="#exec-2">exec/2</a></td><td>並行に複数のコマンドを実行する
返り値は実行が終了した順番で返される.</td></tr><tr><td valign="top"><a href="#exec_sort-1">exec_sort/1</a></td><td>並行に複数のコマンドを実行し、その結果を入力の順に返す.</td></tr><tr><td valign="top"><a href="#exec_sort-2">exec_sort/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exec-1"></a>

### exec/1 ###

<pre><code>
exec(Inputs::[Input]) -&gt; [{Input, RetValue::term()}]
</code></pre>

<ul class="definitions"><li><code>Input = {module(), Function::atom(), Args::[term()]}</code></li></ul>

並行に複数のコマンドを実行する

see: `exec(Input, infinity)`

<a name="exec-2"></a>

### exec/2 ###

<pre><code>
exec(Inputs::[Input], Timeout) -&gt; [{Input, RetValue::term()}]
</code></pre>

<ul class="definitions"><li><code>Input = {module(), Function::atom(), Args::[term()]}</code></li><li><code>Timeout = timeout()</code></li></ul>

並行に複数のコマンドを実行する
返り値は実行が終了した順番で返される. <br />
また, 1つでも結果がerrorだった場合, その1つのerror結果を呼び出し元に投げ, 他のプロセスは強制終了される.

<a name="exec_sort-1"></a>

### exec_sort/1 ###

<pre><code>
exec_sort(Inputs::[Input]) -&gt; [RetValue::term()]
</code></pre>

<ul class="definitions"><li><code>Input = {module(), Function::atom(), Args::[term()]}</code></li></ul>

並行に複数のコマンドを実行し、その結果を入力の順に返す.

1つでも結果がerrorだった場合, その1つのerror結果を呼び出し元に投げ, 他のプロセスは強制終了される.

<a name="exec_sort-2"></a>

### exec_sort/2 ###

<pre><code>
exec_sort(Inputs::[Input], Timeout) -&gt; [RetValue::term()]
</code></pre>

<ul class="definitions"><li><code>Input = {module(), Function::atom(), Args::[term()]}</code></li><li><code>Timeout = timeout()</code></li></ul>

