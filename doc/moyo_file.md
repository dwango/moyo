

# Module moyo_file #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

ファイル関連の処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>{error, _} が返ってきた時にエラーを発生させる版の file:close/1 です.</td></tr><tr><td valign="top"><a href="#delete_directory-2">delete_directory/2</a></td><td>指定ディレクトリを削除する.</td></tr><tr><td valign="top"><a href="#delete_if_exists-1">delete_if_exists/1</a></td><td>ファイルを削除する.</td></tr><tr><td valign="top"><a href="#get_disk_usage-1">get_disk_usage/1</a></td><td>指定パスのディスク使用量を取得する.</td></tr><tr><td valign="top"><a href="#get_disk_usage_async-3">get_disk_usage_async/3</a></td><td>指定パスのディス使用量を非同期に取得する.</td></tr><tr><td valign="top"><a href="#make_temp_filepath-0">make_temp_filepath/0</a></td><td>Equivalent to <a href="#make_temp_filepath-1"><tt>make_temp_filepath(&lt;&lt;""&gt;&gt;)</tt></a>.</td></tr><tr><td valign="top"><a href="#make_temp_filepath-1">make_temp_filepath/1</a></td><td>ユニークな一時ファイルパスを生成して返す.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>{error, _} が返ってきた時にエラーを発生させる版の file:open/2 です.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>{error, _} が返ってきた時にエラーを発生させる版の file:write/2 です.</td></tr><tr><td valign="top"><a href="#write_or_close-2">write_or_close/2</a></td><td>{error, _} が返ってきた時にファイルを閉じてエラーを発生させる版の file:write/2 です.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(IoDevice::<a href="file.md#type-io_device">file:io_device()</a>) -&gt; ok
</code></pre>
<br />

{error, _} が返ってきた時にエラーを発生させる版の file:close/1 です.

<a name="delete_directory-2"></a>

### delete_directory/2 ###

<pre><code>
delete_directory(DirectoryPath::<a href="file.md#type-name_all">file:name_all()</a>, Recursive::boolean()) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

指定ディレクトリを削除する.

`Recursive`に`true`が指定された場合は、ディレクトリの子要素を含めて再帰的に削除処理を実行する. <br />
なお、指定ディレクトリが存在しない場合は`ok`が返される.

<a name="delete_if_exists-1"></a>

### delete_if_exists/1 ###

<pre><code>
delete_if_exists(FilePath::<a href="file.md#type-name_all">file:name_all()</a>) -&gt; ok
</code></pre>
<br />

ファイルを削除する. 失敗したらエラーが発生するが, ファイルがない時はエラーは起きず ok を返す.

(補足)file:delete はファイルがないと {error, enoent} を返すが, この関数はそのような場合何もしないで ok を返す.

<a name="get_disk_usage-1"></a>

### get_disk_usage/1 ###

<pre><code>
get_disk_usage(Path::<a href="file.md#type-name_all">file:name_all()</a>) -&gt; {ok, UsageBytes::non_neg_integer()} | {error, Reason::term()}
</code></pre>
<br />

指定パスのディスク使用量を取得する.

対象パスが存在しない場合は`{ok, 0}`が返る.

<a name="get_disk_usage_async-3"></a>

### get_disk_usage_async/3 ###

<pre><code>
get_disk_usage_async(Path::<a href="file.md#type-name_all">file:name_all()</a>, Pid::pid(), Tag::term()) -&gt; ok
</code></pre>
<br />

指定パスのディス使用量を非同期に取得する.

容量取得処理が終了したタイミングで`Pid`プロセスに`{Tag, get_disk_usage(Path)}`形式のメッセージが送信される.

<a name="make_temp_filepath-0"></a>

### make_temp_filepath/0 ###

<pre><code>
make_temp_filepath() -&gt; Path::binary()
</code></pre>
<br />

Equivalent to [`make_temp_filepath(<<"">>)`](#make_temp_filepath-1).

<a name="make_temp_filepath-1"></a>

### make_temp_filepath/1 ###

<pre><code>
make_temp_filepath(Prefix::binary()) -&gt; Path::binary()
</code></pre>
<br />

ユニークな一時ファイルパスを生成して返す.

生成されるパスの形式は`/tmp/Prefix_ユニークな文字列`となる.

なお、この関数自体はファイルの作成を行わないため、以下のように一時ファイルパスの生成から、
実際のファイル作成までの間に、他のプロセスとの競合が発生する可能性があるので注意が必要.

```
  %% 1] 一時ファイルパスを生成 (この時点ではユニーク)
  > Path = make_temp_filepath().
  　
  %% 2] ここで他のプロセスが偶然同じファイル名を使用して file:write_file/2 を呼び出した
  　
  %% 3] Pathにデータを書き込み
        => 他のプロセスが書き込んだ内容を上書きしてしまう！
  > file:write_file(Path, <<"data">>).
```

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(File::<a href="file.md#type-name_all">file:name_all()</a>, Modes::[Mode]) -&gt; <a href="file.md#type-io_device">file:io_device()</a>
</code></pre>

<ul class="definitions"><li><code>Mode = term()</code></li></ul>

{error, _} が返ってきた時にエラーを発生させる版の file:open/2 です.

<a name="write-2"></a>

### write/2 ###

<pre><code>
write(IoDevice::<a href="file.md#type-io_device">file:io_device()</a>, Bytes::iodata()) -&gt; ok
</code></pre>
<br />

{error, _} が返ってきた時にエラーを発生させる版の file:write/2 です.

<a name="write_or_close-2"></a>

### write_or_close/2 ###

<pre><code>
write_or_close(IoDevice::<a href="file.md#type-io_device">file:io_device()</a>, Bytes::iodata()) -&gt; ok
</code></pre>
<br />

{error, _} が返ってきた時にファイルを閉じてエラーを発生させる版の file:write/2 です.

