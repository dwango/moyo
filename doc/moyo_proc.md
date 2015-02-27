

# Module moyo_proc #
* [Description](#description)
* [Data Types](#types)


プロセス関連の処理や型を集めたユーティリティモジュール.
Copyright (c) 2013-2015 DWANGO Co., Ltd. All Rights Reserved.


<a name="types"></a>

## Data Types ##




### <a name="type-otp_name">otp_name()</a> ###



<pre><code>
otp_name() = {local, Name::atom()} | {global, Name::term()} | {via, module(), Name::term()}
</code></pre>



[`gen_server:start_link/4`](gen_server.md#start_link-4)等に指定可能な起動プロセスの名前



### <a name="type-otp_ref">otp_ref()</a> ###



<pre><code>
otp_ref() = (Name::atom()) | {Name::atom(), node()} | {global, Name::term()} | {via, module(), Name::term()} | pid()
</code></pre>



[`gen_server:cast/2`](gen_server.md#cast-2)等に指定可能な宛先プロセスの参照
