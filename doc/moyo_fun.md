

# Module moyo_fun #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


関数に関する処理を集めたユーティリティモジュール.
Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.


<a name="types"></a>

## Data Types ##




### <a name="type-stack_item">stack_item()</a> ###



<pre><code>
stack_item() = {Module::module(), Function::atom(), Arity::arity() | (Args::[term()]), Location::[{file, Filename::string()} | {line, Line::pos_integer()}]}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#repeat-3">repeat/3</a></td><td>指定した回数だけ関数を実行する.</td></tr><tr><td valign="top"><a href="#try_apply-3">try_apply/3</a></td><td>指定された関数を実行する.</td></tr><tr><td valign="top"><a href="#try_apply-4">try_apply/4</a></td><td>指定された関数を実行する.</td></tr><tr><td valign="top"><a href="#try_call-1">try_call/1</a></td><td>引数の関数を実行する.</td></tr><tr><td valign="top"><a href="#try_call-2">try_call/2</a></td><td>引数の関数を実行する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

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
