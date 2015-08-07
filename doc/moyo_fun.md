

# Module moyo_fun #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

関数に関する処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="types"></a>

## Data Types ##




### <a name="type-computation">computation()</a> ###


<pre><code>
computation() = fun(() -&gt; <a href="#type-computation_status">computation_status()</a>) | fun((InputValue::term()) -&gt; <a href="#type-computation_status">computation_status()</a>)
</code></pre>

 合成関数を構成する関数一つ一つの定義．個々の関数はarityを0又は1とし,computation_status()を返すものとする.



### <a name="type-computation_status">computation_status()</a> ###


<pre><code>
computation_status() = {ok, Response::term()} | ok | {error, Reason::term()} | error
</code></pre>

 合成関数を構成する関数一つ一つの返り値の定義.



### <a name="type-stack_item">stack_item()</a> ###


<pre><code>
stack_item() = {Module::module(), Function::atom(), Arity::arity() | (Args::[term()]), Location::[{file, Filename::string()} | {line, Line::pos_integer()}]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_on_exit-4">apply_on_exit/4</a></td><td>Pidsで指定したプロセスのうちの一つでも死んだら指定の関数を実行する.</td></tr><tr><td valign="top"><a href="#apply_on_exit_impl-4">apply_on_exit_impl/4</a></td><td></td></tr><tr><td valign="top"><a href="#apply_on_exit_receiver-4">apply_on_exit_receiver/4</a></td><td></td></tr><tr><td valign="top"><a href="#composite_apply-1">composite_apply/1</a></td><td>関数のリストを先頭から順に実行する.ただし,先頭の関数は引数をとらない.</td></tr><tr><td valign="top"><a href="#composite_apply-2">composite_apply/2</a></td><td>関数のリストを先頭から順に実行する.先頭の関数が引数をとる場合は,
その引数を本関数の第二引数にリストで指定する.</td></tr><tr><td valign="top"><a href="#fold_range-4">fold_range/4</a></td><td>関数に loop X in [From, To] と直前の結果を渡して最後の結果を返す.</td></tr><tr><td valign="top"><a href="#map_range-3">map_range/3</a></td><td>関数に loop X in [From, To] を渡して各々の結果をリストで返す.</td></tr><tr><td valign="top"><a href="#maybe_fold_range-4">maybe_fold_range/4</a></td><td><code>{error, Reason}</code>を返した場合に途中で処理を中断し, 結果を返す <a href="#fold_range-4"><code>fold_range/4</code></a></td></tr><tr><td valign="top"><a href="#repeat-3">repeat/3</a></td><td>指定した回数だけ関数を実行する.</td></tr><tr><td valign="top"><a href="#try_apply-3">try_apply/3</a></td><td>指定された関数を実行する.</td></tr><tr><td valign="top"><a href="#try_apply-4">try_apply/4</a></td><td>指定された関数を実行する.</td></tr><tr><td valign="top"><a href="#try_call-1">try_call/1</a></td><td>引数の関数を実行する.</td></tr><tr><td valign="top"><a href="#try_call-2">try_call/2</a></td><td>引数の関数を実行する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_on_exit-4"></a>

### apply_on_exit/4 ###

<pre><code>
apply_on_exit(Pids::[pid()], Module::module(), Function::atom(), Args::[term()]) -&gt; Executor::pid()
</code></pre>
<br />

Pidsで指定したプロセスのうちの一つでも死んだら指定の関数を実行する.

<a name="apply_on_exit_impl-4"></a>

### apply_on_exit_impl/4 ###

<pre><code>
apply_on_exit_impl(Pids::[pid()], Module::module(), Function::atom(), Args::[term()]) -&gt; Executor::pid()
</code></pre>
<br />

<a name="apply_on_exit_receiver-4"></a>

### apply_on_exit_receiver/4 ###

<pre><code>
apply_on_exit_receiver(RefList::[reference()], Module::module(), Function::atom(), Args::[term()]) -&gt; Executor::pid()
</code></pre>
<br />

<a name="composite_apply-1"></a>

### composite_apply/1 ###

<pre><code>
composite_apply(FunctionList::[<a href="#type-computation">computation()</a>]) -&gt; <a href="#type-computation_status">computation_status()</a>
</code></pre>
<br />

関数のリストを先頭から順に実行する.ただし,先頭の関数は引数をとらない.

<a name="composite_apply-2"></a>

### composite_apply/2 ###

<pre><code>
composite_apply(FunctionList::[<a href="#type-computation">computation()</a>], Arg::term()) -&gt; <a href="#type-computation_status">computation_status()</a>
</code></pre>
<br />

関数のリストを先頭から順に実行する.先頭の関数が引数をとる場合は,
その引数を本関数の第二引数にリストで指定する.
リスト中の各関数の返り値を,次に実行される関数の引数として渡して
errorを吐くまで実行していく.

<a name="fold_range-4"></a>

### fold_range/4 ###

<pre><code>
fold_range(Function, AccIn::term(), From::integer(), To::integer()) -&gt; AccOut::term()
</code></pre>

<ul class="definitions"><li><code>Function = fun((Index::integer(), AccIn::term()) -&gt; AccOut::term())</code></li></ul>

関数に loop X in [From, To] と直前の結果を渡して最後の結果を返す.

<a name="map_range-3"></a>

### map_range/3 ###

<pre><code>
map_range(Function, From::integer(), To::integer()) -&gt; [AccOut::term()]
</code></pre>

<ul class="definitions"><li><code>Function = fun((X::integer()) -&gt; AccOut::term())</code></li></ul>

関数に loop X in [From, To] を渡して各々の結果をリストで返す.

<a name="maybe_fold_range-4"></a>

### maybe_fold_range/4 ###

<pre><code>
maybe_fold_range(Fun, AccIn::term(), From::integer(), To::integer()) -&gt; {ok, Result::term()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Index::integer(), AccIn::term()) -&gt; {ok, AccOut::term()} | {error, Reason})</code></li><li><code>Reason = term()</code></li></ul>

`{error, Reason}`を返した場合に途中で処理を中断し, 結果を返す [`fold_range/4`](#fold_range-4)

<a name="repeat-3"></a>

### repeat/3 ###

<pre><code>
repeat(Function, InitState::term(), MaxIndex::non_neg_integer()) -&gt; FinalState::term()
</code></pre>

<ul class="definitions"><li><code>Function = fun((Index::non_neg_integer(), State::term()) -&gt; NextState::term())</code></li></ul>

指定した回数だけ関数を実行する. 関数には loop index in [0, N) が渡される

<a name="try_apply-3"></a>

### try_apply/3 ###

<pre><code>
try_apply(Module::module(), Function::atom(), Args::[term()]) -&gt; FunctionResult | ErrorResult
</code></pre>

<ul class="definitions"><li><code>FunctionResult = term()</code></li><li><code>ErrorResult = {error, {'EXIT', {throw | error | exit, Reason::term(), [<a href="#type-stack_item">stack_item()</a>]}}}</code></li></ul>

指定された関数を実行する. 実行中に例外が発生した場合は`{error, {`EXIT', {Class, Reason, Stacktrace}}}'を返す

<a name="try_apply-4"></a>

### try_apply/4 ###

<pre><code>
try_apply(Module::module(), Function::atom(), Args::[term()], ErrorResult) -&gt; FunctionResult | ErrorResult
</code></pre>

<ul class="definitions"><li><code>FunctionResult = term()</code></li><li><code>ErrorResult = term()</code></li></ul>

指定された関数を実行する. 実行中に例外が発生した場合は`ErrorResult`を返す

<a name="try_call-1"></a>

### try_call/1 ###

<pre><code>
try_call(Fun::function()) -&gt; FunctionResult | ErrorResult
</code></pre>

<ul class="definitions"><li><code>FunctionResult = term()</code></li><li><code>ErrorResult = {error, {'EXIT', {throw | error | exit, Reason::term(), [<a href="#type-stack_item">stack_item()</a>]}}}</code></li></ul>

引数の関数を実行する. 実行中に例外が発生した場合は`{error, {`EXIT', {Class, Reason, Stacktrace}}}'を返す

<a name="try_call-2"></a>

### try_call/2 ###

<pre><code>
try_call(Fun::function(), ErrorResult) -&gt; FunctionResult | ErrorResult
</code></pre>

<ul class="definitions"><li><code>FunctionResult = term()</code></li><li><code>ErrorResult = term()</code></li></ul>

引数の関数を実行する. 実行中に例外が発生した場合は`ErrorResult`を返す

