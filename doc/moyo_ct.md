

# Module moyo_ct #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Common Test Utility.
Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-1">all/1</a></td><td>SUITE moduleのCommon TestのCall back Functions以外を取得する.</td></tr><tr><td valign="top"><a href="#eunit-1">eunit/1</a></td><td>EunitをCommon Testに組み込む場合に使用できる.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-1"></a>

### all/1 ###


<pre><code>
all(SuiteModule::module()) -&gt; [Function::atom()]
</code></pre>
<br />


SUITE moduleのCommon TestのCall back Functions以外を取得する.


Arity =:= 1 以外は取得しない為, Eunit対象も外れる.
<a name="eunit-1"></a>

### eunit/1 ###


<pre><code>
eunit(Application::atom()) -&gt; ok
</code></pre>
<br />

EunitをCommon Testに組み込む場合に使用できる.
