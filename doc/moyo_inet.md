

# Module moyo_inet #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


`inet`の拡張ライブラリ.
Copyright (c) 2013-2015 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#find_free_port-0">find_free_port/0</a></td><td>現在空いているポートを1つ返す.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="find_free_port-0"></a>

### find_free_port/0 ###


<pre><code>
find_free_port() -&gt; {ok, <a href="inet.md#type-port_number">inet:port_number()</a>} | {error, Reason::term()}
</code></pre>
<br />


現在空いているポートを1つ返す


この関数を呼び出した際の空きポートを返す為, そのポートが他のアプリケーションによって使用されてしまい, 使えない可能性がある.
