

# Module moyo_clock #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


日付や時間に関する処理を集めたユーティリティモジュール.
Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="description"></a>

## Description ##
 
<a name="types"></a>

## Data Types ##




### <a name="type-datetime">datetime()</a> ###



<pre><code>
datetime() = <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>





### <a name="type-iso8601datetype">iso8601datetype()</a> ###



<pre><code>
iso8601datetype() = yyyy | yyyy_mm | yyyy_mm_dd | yyyy_ddd | yyyy_Www_d | yyyymmdd | yyyyWwwd | yyyyddd
</code></pre>





### <a name="type-iso8601timetype">iso8601timetype()</a> ###



<pre><code>
iso8601timetype() = hh | hhmm | hhmmss | hh_mm | hh_mm_ss | hh_s | hhmm_s | hh_mm_s | hhmmss_s | hh_mm_ss_s
</code></pre>





### <a name="type-iso8601type">iso8601type()</a> ###



<pre><code>
iso8601type() = <a href="#type-iso8601datetype">iso8601datetype()</a> | {<a href="#type-iso8601datetype">iso8601datetype()</a>, <a href="#type-iso8601timetype">iso8601timetype()</a>} | {<a href="#type-iso8601datetype">iso8601datetype()</a>, <a href="#type-iso8601timetype">iso8601timetype()</a>, <a href="#type-iso8601zonetype">iso8601zonetype()</a>}
</code></pre>





### <a name="type-iso8601zonetype">iso8601zonetype()</a> ###



<pre><code>
iso8601zonetype() = z | hh_mm | hhmm | hh
</code></pre>





### <a name="type-milliseconds">milliseconds()</a> ###



<pre><code>
milliseconds() = integer()
</code></pre>





### <a name="type-non_neg_milliseconds">non_neg_milliseconds()</a> ###



<pre><code>
non_neg_milliseconds() = non_neg_integer()
</code></pre>





### <a name="type-non_neg_seconds">non_neg_seconds()</a> ###



<pre><code>
non_neg_seconds() = non_neg_integer()
</code></pre>





### <a name="type-pos_milliseconds">pos_milliseconds()</a> ###



<pre><code>
pos_milliseconds() = pos_integer()
</code></pre>





### <a name="type-pos_seconds">pos_seconds()</a> ###



<pre><code>
pos_seconds() = pos_integer()
</code></pre>





### <a name="type-seconds">seconds()</a> ###



<pre><code>
seconds() = integer()
</code></pre>





### <a name="type-timezone">timezone()</a> ###



<pre><code>
timezone() = {Sign::1 | -1, <a href="calendar.md#type-time">calendar:time()</a>}
</code></pre>





### <a name="type-unix_timestamp">unix_timestamp()</a> ###



<pre><code>
unix_timestamp() = <a href="#type-non_neg_seconds">non_neg_seconds()</a>
</code></pre>



 1970-01-01T00:00:00Z からの経過秒数
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#datetime_add-2">datetime_add/2</a></td><td>引数の日時に指定秒数を加算する.</td></tr><tr><td valign="top"><a href="#datetime_diff-1">datetime_diff/1</a></td><td>引数の日時と現在時刻の差を求める.</td></tr><tr><td valign="top"><a href="#datetime_diff-2">datetime_diff/2</a></td><td>二つの日時の差を求める.</td></tr><tr><td valign="top"><a href="#datetime_format-2">datetime_format/2</a></td><td>Equivalent to <a href="#now_format-2"><tt>now_format(Format, datetime_to_now(DateTime))</tt></a>.</td></tr><tr><td valign="top"><a href="#datetime_to_iso8601ext-1">datetime_to_iso8601ext/1</a></td><td><code>datetime()</code>型のローカル時刻をISO8601の拡張表記の日付文字列(バイナリ)に変換する.</td></tr><tr><td valign="top"><a href="#datetime_to_now-1">datetime_to_now/1</a></td><td><code>calendar:datetime()</code>形式の日時を<code>erlang:timestamp()</code>形式に変換する.</td></tr><tr><td valign="top"><a href="#datetime_to_seconds-1">datetime_to_seconds/1</a></td><td><code>datetime()</code>形式の日時をUNIXタイプスタンプ形式の数値に変換する.</td></tr><tr><td valign="top"><a href="#is_date-1">is_date/1</a></td><td>iso8601もしくはunixtime形式、date()、datetime()、time()であるかを判定する。.</td></tr><tr><td valign="top"><a href="#is_date-2">is_date/2</a></td><td>Optionsに指定したiso8601形式のどれかであるかを判定する.</td></tr><tr><td valign="top"><a href="#is_valid_datetime-1">is_valid_datetime/1</a></td><td>正しい日時になっているかどうかを判定する.</td></tr><tr><td valign="top"><a href="#iso8601_to_datetime-1">iso8601_to_datetime/1</a></td><td>iso8601を<code>datetime()</code>形式に変換する。.</td></tr><tr><td valign="top"><a href="#iso8601ext_to_datetime-1">iso8601ext_to_datetime/1</a></td><td>ISO8601の拡張表記の日付文字列をローカル時刻の<code>datetime()</code>型に変換する.</td></tr><tr><td valign="top"><a href="#now-0">now/0</a></td><td>現在時刻を<code>erlang:timestamp()</code>形式で返す.</td></tr><tr><td valign="top"><a href="#now_format-1">now_format/1</a></td><td>Equivalent to <a href="#now_format-2"><tt>now_format(Format, now())</tt></a>.</td></tr><tr><td valign="top"><a href="#now_format-2">now_format/2</a></td><td>日付/時刻を書式化する.</td></tr><tr><td valign="top"><a href="#now_seconds-0">now_seconds/0</a></td><td>UNIXタイムスタンプ形式の現在時間(ローカル時刻)を取得する.</td></tr><tr><td valign="top"><a href="#now_unix_time_in_float-0">now_unix_time_in_float/0</a></td><td>UNIX Time をfloatで返す.</td></tr><tr><td valign="top"><a href="#parse_iso8601-1">parse_iso8601/1</a></td><td>iso8601形式をパースし, 結果をタプルで返す。.</td></tr><tr><td valign="top"><a href="#parse_iso8601_date-1">parse_iso8601_date/1</a></td><td>iso8601の日付部分のパースを提供する。.</td></tr><tr><td valign="top"><a href="#parse_iso8601_time-1">parse_iso8601_time/1</a></td><td>iso8601形式の時刻部分のパースを提供する。.</td></tr><tr><td valign="top"><a href="#parse_iso8601_timezone-1">parse_iso8601_timezone/1</a></td><td>タイムゾーンをtime形式に変換する.</td></tr><tr><td valign="top"><a href="#seconds_to_datetime-1">seconds_to_datetime/1</a></td><td>UNIXタイムスタンプ形式の日時を<code>datetime()</code>形式に変換する.</td></tr><tr><td valign="top"><a href="#seconds_to_now-1">seconds_to_now/1</a></td><td>UNIXタイムスタンプ形式の日時を<code>erlang:timestamp()</code>形式に変換する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="datetime_add-2"></a>

### datetime_add/2 ###


<pre><code>
datetime_add(DateTime::<a href="#type-datetime">datetime()</a>, Seconds::<a href="#type-seconds">seconds()</a>) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

引数の日時に指定秒数を加算する.
<a name="datetime_diff-1"></a>

### datetime_diff/1 ###


<pre><code>
datetime_diff(DateTime::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-seconds">seconds()</a>
</code></pre>
<br />


引数の日時と現在時刻の差を求める.


`Datetime` - 現在時刻 = 秒数
<a name="datetime_diff-2"></a>

### datetime_diff/2 ###


<pre><code>
datetime_diff(DateTime1::<a href="#type-datetime">datetime()</a>, DateTime2::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-seconds">seconds()</a>
</code></pre>
<br />


二つの日時の差を求める.


`DateTime1` - `DateTime2` = 秒数
<a name="datetime_format-2"></a>

### datetime_format/2 ###


<pre><code>
datetime_format(Format::binary(), DateTime::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; binary()
</code></pre>
<br />

Equivalent to [`now_format(Format, datetime_to_now(DateTime))`](#now_format-2).
<a name="datetime_to_iso8601ext-1"></a>

### datetime_to_iso8601ext/1 ###


<pre><code>
datetime_to_iso8601ext(DataTime::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; binary()
</code></pre>
<br />


`datetime()`型のローカル時刻をISO8601の拡張表記の日付文字列(バイナリ)に変換する



```
  > datetime_to_iso8601ext({{2014,4,20}, {9,9,9}}).
  <<"2014-04-20T09:09:09+09:00">>
```

<a name="datetime_to_now-1"></a>

### datetime_to_now/1 ###


<pre><code>
datetime_to_now(DateTime::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />

`calendar:datetime()`形式の日時を`erlang:timestamp()`形式に変換する.
<a name="datetime_to_seconds-1"></a>

### datetime_to_seconds/1 ###


<pre><code>
datetime_to_seconds(DateTime::<a href="#type-datetime">datetime()</a>) -&gt; <a href="#type-unix_timestamp">unix_timestamp()</a>
</code></pre>
<br />


`datetime()`形式の日時をUNIXタイプスタンプ形式の数値に変換する.


変換可能な範囲外の日時が渡された場合は、例外が送出される
<a name="is_date-1"></a>

### is_date/1 ###


<pre><code>
is_date(Value) -&gt; true | false
</code></pre>

<ul class="definitions"><li><code>Value = binary() | <a href="#type-datetime">datetime()</a> | <a href="calendar.md#type-date">calendar:date()</a> | <a href="calendar.md#type-time">calendar:time()</a> | non_neg_integer()</code></li></ul>


iso8601もしくはunixtime形式、date()、datetime()、time()であるかを判定する。



対応するiso8601の形式は[`parse_iso8601/1`](#parse_iso8601-1)に準拠する。



```
  > is_date(<<"2014-04-01T14-15-16Z">>).
  true.
```

<a name="is_date-2"></a>

### is_date/2 ###


<pre><code>
is_date(Value, Type) -&gt; true | false
</code></pre>

<ul class="definitions"><li><code>Value = binary() | <a href="#type-datetime">datetime()</a> | <a href="calendar.md#type-date">calendar:date()</a> | <a href="calendar.md#type-time">calendar:time()</a> | non_neg_integer()</code></li><li><code>Type = [<a href="#type-iso8601type">iso8601type()</a> | date | datetime | time | unixtime | any]</code></li></ul>


Optionsに指定したiso8601形式のどれかであるかを判定する.


iso8601形式のどれであるかを判定した後, Optionsの判定を行う為, 複数のOptionを指定することによってコストはそれほど増えない.
また, 日付部分は`yyyymmdd`時間部分は何でもよいといった指定はできない.

<a name="is_valid_datetime-1"></a>

### is_valid_datetime/1 ###


<pre><code>
is_valid_datetime(Bin::<a href="#type-datetime">datetime()</a>) -&gt; true | false
</code></pre>
<br />

正しい日時になっているかどうかを判定する

<a name="iso8601_to_datetime-1"></a>

### iso8601_to_datetime/1 ###


<pre><code>
iso8601_to_datetime(Bin::binary()) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />


iso8601を`datetime()`形式に変換する。



iso8601のtimezoneがある場合はUTCに変換する。


また、iso8601形式のbinary以外はerror(badarg, [Bin])が投げられる
<a name="iso8601ext_to_datetime-1"></a>

### iso8601ext_to_datetime/1 ###


<pre><code>
iso8601ext_to_datetime(Value::binary()) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />


ISO8601の拡張表記の日付文字列をローカル時刻の`datetime()`型に変換する



なお、現状許容している形式は'YYYY-MM-DDThh:mm:ss(Z|(+|-)hh:mm)'のみで、月や時間等の省略は不可。



不正な文字列が渡された場合は、エラーが送出される.



```
  > iso8601ext_to_datetime(<<"2014-04-20T09:09:09+09:00">>).
  {{2014,4,20}, {9,9,9}}
```

<a name="now-0"></a>

### now/0 ###


<pre><code>
now() -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />


現在時刻を`erlang:timestamp()`形式で返す


動作的には[`erlang:now/0`](erlang.md#now-0)および[`os:timestamp/0`](os.md#timestamp-0)と同様. <br />
ユニットテストでモックを行いやすくすることができるのがこの関数の存在意義.
<a name="now_format-1"></a>

### now_format/1 ###


<pre><code>
now_format(Format::binary()) -&gt; binary()
</code></pre>
<br />

Equivalent to [`now_format(Format, now())`](#now_format-2).
<a name="now_format-2"></a>

### now_format/2 ###


<pre><code>
now_format(Format::binary(), Now::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; binary()
</code></pre>
<br />


日付/時刻を書式化する.


以下の文字が `format` パラメータ文字列として認識されます.<br />
参考: php date関数<br />
[
http://php.net/manual/ja/function.date.php#refsect1-function.date-parameters
](http://php.net/manual/ja/function.date.php#refsect1-function.date-parameters)


<table class="line">
<tr><td class="line">Y</td><td class="line">年.4桁の数字.</td></tr>
<tr><td class="line">m</td><td class="line">月.数字.先頭にゼロをつける.</td></tr>
<tr><td class="line">d</td><td class="line">日.二桁の数字(先頭にゼロが付く場合も)</td></tr>
<tr><td class="line">H</td><td class="line">時.24時間単位.</td></tr>
<tr><td class="line">i</td><td class="line">分.先頭にゼロをつける.</td></tr>
<tr><td class="line">s</td><td class="line">秒.先頭にゼロをつける.</td></tr>
<tr><td class="line">p</td><td class="line">ミリ秒.3桁の数字.先頭にゼロをつける.</td></tr>
</table>



ex:

```
  1> moyo_clock:now_format(<<"Y/m/d-H:i:s(p)">>, now()).
  <<"2013/10/09-11:59:32(131)">>
```



formatパラメータ文字列にあたる文字をそのまま表示したい場合は, `\\` でエスケープできる.
( `\\` の後ろの1文字はどの文字でもそのまま表示される. `\` も例外ではない.)


ex:

```
  2> moyo_clock:now_format(<<"\\Year: Y\\\\">>, now()).
  <<"Year: 2013\\">>
```

<a name="now_seconds-0"></a>

### now_seconds/0 ###


<pre><code>
now_seconds() -&gt; <a href="#type-unix_timestamp">unix_timestamp()</a>
</code></pre>
<br />

UNIXタイムスタンプ形式の現在時間(ローカル時刻)を取得する.
<a name="now_unix_time_in_float-0"></a>

### now_unix_time_in_float/0 ###


<pre><code>
now_unix_time_in_float() -&gt; float()
</code></pre>
<br />

UNIX Time をfloatで返す
<a name="parse_iso8601-1"></a>

### parse_iso8601/1 ###


<pre><code>
parse_iso8601(Bin::binary()) -&gt; {ok, {<a href="#type-iso8601type">iso8601type()</a>, <a href="#type-datetime">datetime()</a>, <a href="#type-timezone">timezone()</a>}} | error
</code></pre>
<br />


iso8601形式をパースし, 結果をタプルで返す。



正しい時間であることも保証する。



パース可能な書式は[`iso8601type/0`](#iso8601type-0)の通り.



20-- (2000年代)などの省略形式 や/を使用した期間表記には対応していない.



返り値`Decimal::number`の値域は[0,1) doubleとして表現される
入力に小数点表記がない場合この値は0となる



```
  > parse_iso8601(<<"2014-04-02T14:15:16Z">>).
  {ok, {{{yyyymmdd, hhmmdd, Z}, {{2014, 4, 2}, {14, 15, 16}}, {1, {0, 0, 0}}}}
  > parse_iso8601(<<"hoge">>).
  error
```

<a name="parse_iso8601_date-1"></a>

### parse_iso8601_date/1 ###


<pre><code>
parse_iso8601_date(Bin::binary()) -&gt; {ok, {<a href="#type-iso8601datetype">iso8601datetype()</a>, <a href="calendar.md#type-date">calendar:date()</a>}, binary()} | error
</code></pre>
<br />


iso8601の日付部分のパースを提供する。



入力された時間の正しさも保証する。



```
  > parse_iso8601_date(<<"2014-W01">>).
  {ok, {yyyy, {2014, 1, 1}}, <<"-W01">>}
  > parse_iso8601_date(<<"2014021234">>).
  {ok, {yyyymmdd, {2014, 2, 12}}, <<"34">>}
  > parse_iso8601_date(<<"20141321">>).
  error
```


上記のように長く一致する形式として認識する。
ただし、3つ目の例のように、最長一致(2014-13-21)が正常な日付であった場合、
部分一致(2014-132)が正常な日付であってもerrorとして判定する.
その為、意図した通りの動作を期待するのであれば、日付の後ろに記述されている文字が0-9であるべきではない.
また、入力が日付部分のみであることを期待している場合、下記を用いるべきである.



```
  > {ok, _, <<>>} = parse_iso8691_date(<<"20140102">>).
  {ok, {yyyymmdd, {2014, 1, 2}}, <<>>}
```

<a name="parse_iso8601_time-1"></a>

### parse_iso8601_time/1 ###


<pre><code>
parse_iso8601_time(Bin::binary()) -&gt; {ok, {<a href="#type-iso8601timetype">iso8601timetype()</a>, <a href="calendar.md#type-time">calendar:time()</a>}, binary()} | error
</code></pre>
<br />


iso8601形式の時刻部分のパースを提供する。



時間の正しさも保証される。



日付の変わり目の表現については、24:00:00は許容していない.
これは、`calendar:time()`型で許容されていない為である.


また、小数点表記(hh:mm:ss.sなど)の小数は何桁でも許容されるが、
`calendar:time()`型で表現できない端数については切り捨てられる.
<a name="parse_iso8601_timezone-1"></a>

### parse_iso8601_timezone/1 ###


<pre><code>
parse_iso8601_timezone(Bin::binary()) -&gt; {ok, {<a href="#type-iso8601zonetype">iso8601zonetype()</a>, <a href="#type-timezone">timezone()</a>}, binary()} | error
</code></pre>
<br />


タイムゾーンをtime形式に変換する



対応形式は `Z`, `+hh:mm`, `-hh:mm`, `+hh`, `-hh`, `+hhmm`, `-hhmm`



```
  > parse_iso8601_timezone(<<"hoge">>).
  error
  > parse_iso8601_timezone(<<"+09:00">>).
  {ok, {hh_mm {1, {9, 0, 0}}}, <<>>}
```

<a name="seconds_to_datetime-1"></a>

### seconds_to_datetime/1 ###


<pre><code>
seconds_to_datetime(Seconds::<a href="#type-unix_timestamp">unix_timestamp()</a>) -&gt; <a href="#type-datetime">datetime()</a>
</code></pre>
<br />

UNIXタイムスタンプ形式の日時を`datetime()`形式に変換する.
<a name="seconds_to_now-1"></a>

### seconds_to_now/1 ###


<pre><code>
seconds_to_now(Seconds::<a href="#type-unix_timestamp">unix_timestamp()</a>) -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />

UNIXタイムスタンプ形式の日時を`erlang:timestamp()`形式に変換する.
