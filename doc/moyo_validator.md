

# Module moyo_validator #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


バリデーションに関する処理を集めたユーティリティモジュール.
Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="description"></a>

## Description ##



パラメータに対して, 指定したデータ型かどうかを検証する.
また, 値に対して制限(範囲や長さ等)を与えることや, オプションを指定することもできる.




#### <a name="【オプションについて】">【オプションについて】</a> ####

オプションを指定すると検証のために与えたパラメータと結果に返るパラメータのデータ型が異なることがある.<br />
しかし, 入出力でデータ型が異なっていた場合も返り値のパラメータはデータ型であることを保証する.<br />
オプションを複数指定した場合, 左からパラメータに適用されるため, 与えるリストの要素の順によって結果が変わる.<br />
<a name="types"></a>

## Data Types ##




### <a name="type-basic_type">basic_type()</a> ###



<pre><code>
basic_type() = integer | float | number | string | binary | boolean | atom | datetime | any
</code></pre>





### <a name="type-binary_constraint">binary_constraint()</a> ###



<pre><code>
binary_constraint() = {max_length, integer()} | {regexp, binary()} | ascii | not_empty
</code></pre>





### <a name="type-custom_spec_fun">custom_spec_fun()</a> ###



<pre><code>
custom_spec_fun() = fun((InputValue::term()) -&gt; boolean())
</code></pre>





### <a name="type-datetime_constraint">datetime_constraint()</a> ###



<pre><code>
datetime_constraint() = {range, <a href="calendar.md#type-datetime">calendar:datetime()</a>, <a href="calendar.md#type-datetime">calendar:datetime()</a>} | {equal, <a href="calendar.md#type-datetime">calendar:datetime()</a>} | {more, <a href="calendar.md#type-datetime">calendar:datetime()</a>} | {less, <a href="calendar.md#type-datetime">calendar:datetime()</a>}
</code></pre>





### <a name="type-float_constraint">float_constraint()</a> ###



<pre><code>
float_constraint() = <a href="#type-sign_constraint">sign_constraint()</a> | {range, <a href="#type-min_number">min_number()</a>, <a href="#type-max_number">max_number()</a>} | {more, number()} | {less, number()}
</code></pre>





### <a name="type-integer_constraint">integer_constraint()</a> ###



<pre><code>
integer_constraint() = <a href="#type-sign_constraint">sign_constraint()</a> | even | odd | {range, <a href="#type-min">min()</a>, <a href="#type-max">max()</a>} | {more, integer()} | {less, integer()}
</code></pre>





### <a name="type-max">max()</a> ###



<pre><code>
max() = integer()
</code></pre>





### <a name="type-max_number">max_number()</a> ###



<pre><code>
max_number() = number()
</code></pre>





### <a name="type-min">min()</a> ###



<pre><code>
min() = integer()
</code></pre>





### <a name="type-min_number">min_number()</a> ###



<pre><code>
min_number() = number()
</code></pre>





### <a name="type-number_constraint">number_constraint()</a> ###



<pre><code>
number_constraint() = <a href="#type-sign_constraint">sign_constraint()</a> | {range, <a href="#type-min_number">min_number()</a>, <a href="#type-max_number">max_number()</a>} | {more, number()} | {less, number()}
</code></pre>





### <a name="type-option">option()</a> ###



<pre><code>
option() = binary_in | {binary_in, <a href="#type-option_binary_in_output_type">option_binary_in_output_type()</a>} | int_to_bool | {transform, <a href="#type-transform_fun">transform_fun()</a>} | allow_integer | to_datetime | {to_datetime, <a href="#type-option_to_datetime_input_type">option_to_datetime_input_type()</a>}
</code></pre>





### <a name="type-option_binary_in_output_type">option_binary_in_output_type()</a> ###



<pre><code>
option_binary_in_output_type() = <a href="#type-basic_type">basic_type()</a> | existing_atom
</code></pre>





### <a name="type-option_to_datetime_input_type">option_to_datetime_input_type()</a> ###



<pre><code>
option_to_datetime_input_type() = iso8601 | unixtime
</code></pre>





### <a name="type-sign_constraint">sign_constraint()</a> ###



<pre><code>
sign_constraint() = positive | negative | non_negative
</code></pre>





### <a name="type-spec">spec()</a> ###



<pre><code>
spec() = <a href="#type-basic_type">basic_type()</a> | <a href="#type-type_constraints">type_constraints()</a> | {list, <a href="#type-spec">spec()</a>} | {tuple, [<a href="#type-spec">spec()</a>]} | {enum, [term()]} | {equal, term()} | {'or', [<a href="#type-spec">spec()</a>]} | {custom, <a href="#type-custom_spec_fun">custom_spec_fun()</a>}
</code></pre>



  {list, spec()}: 入力値が`spec()`に適合する値を要素とするリストかをチェックする<br />
{tuple, [spec()]}: 入力値の各要素が`spec()`と適合するかチェックする. `spec()`は`tuple`の要素数用意する必要がある.<br />
{enum, [term()]}: 入力値が`[term()]`のいずれかの要素と等しいかどうかをチェックする<br />
{equal, term()}: 入力値が`term()`と等しいかどうかをチェックする<br />
{'or', [spec()]}: 入力値が`[spec()]`のいずれかの条件に適合するかをチェックする<br />
{custom, custom_spec_fun()}: 入力値が`custom_spec_fun()`で指定の条件に適合するかどうかをチェックする<br />



### <a name="type-string_constraint">string_constraint()</a> ###



<pre><code>
string_constraint() = {max_length, integer()} | {regexp, string()} | ascii | not_empty
</code></pre>





### <a name="type-transform_fun">transform_fun()</a> ###



<pre><code>
transform_fun() = fun((InputValue::term()) -&gt; TransformedValue::term())
</code></pre>





### <a name="type-type_constraints">type_constraints()</a> ###



<pre><code>
type_constraints() = {integer, [<a href="#type-integer_constraint">integer_constraint()</a>]} | {float, [<a href="#type-float_constraint">float_constraint()</a>]} | {number, [<a href="#type-number_constraint">number_constraint()</a>]} | {string, [<a href="#type-string_constraint">string_constraint()</a>]} | {binary, [<a href="#type-binary_constraint">binary_constraint()</a>]} | {datetime, [<a href="#type-datetime_constraint">datetime_constraint()</a>]} | {boolean, []} | {atom, []} | {any, []}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#validate-2">validate/2</a></td><td>データ型, 制限を指定して, パラメータがその条件に一致するかを検証する.</td></tr><tr><td valign="top"><a href="#validate-3">validate/3</a></td><td>データ型, 制限を指定して, パラメータがその条件に一致するかを検証する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="validate-2"></a>

### validate/2 ###


<pre><code>
validate(InputValue, Spec) -&gt; {ok, OutputValue} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>InputValue = term()</code></li><li><code>Spec = <a href="#type-spec">spec()</a></code></li><li><code>OutputValue = term()</code></li><li><code>Reason = term()</code></li></ul>


データ型, 制限を指定して, パラメータがその条件に一致するかを検証する.



データ型だけではなく, 値等に制限を掛けて検査することができる.




#### <a name="制限一覧">制限一覧</a> ####


```
  ● integer
      ○ positive
          パラメータが負数, または, 0の時にerror.
      ○ negative
          パラメータが正数, または, 0の時にerror.
      ○ non_negative
          パラメータが負の数の場合error.
      ○ even
          パラメータが奇数の場合error.
      ○ odd
          パラメータが偶数の場合error.
      ○ {range, min(), max()}
          パラメータがmin()からmax()の間に含まれない場合error.
          パラメータがmin(), max()と同じ値の場合はokを返す.
      ○ {more, integer()}
          パラメータがinteger()以下の場合error.
      ○ {less, integer()}
          パラメータがinteger()以上の場合error.
  ● float
      ○ positive
          パラメータが負数, または, 0の時にerror.
      ○ negative
          パラメータが正数, または, 0の時にerror.
      ○ non_negative
          パラメータが負の数の場合error.
      ○ {range, min_number(), max_number()}
          パラメータがmin_number()からmax_number()の間に含まれない場合error.
          パラメータがmin_number(), max_number()と同じ値の場合はokを返す.
      ○ {more, integer()}
          パラメータがinteger()以下の場合error.
      ○ {less, integer()}
          パラメータがinteger()以上の場合error.
  ●number
      ○ positive
          パラメータが負数, または, 0の時にerror.
      ○ negative
          パラメータが正数, または, 0の時にerror.
      ○ non_negative
          パラメータが負の数の場合error.
      ○ {range, min_number(), max_number()}
          パラメータがmin_number()からmax_number()の間に含まれない場合error.
          パラメータがmin_number(), max_number()と同じ値の場合はokを返す.
      ○ {more, integer()}
          パラメータがinteger()以下の場合error.
      ○ {less, integer()}
          パラメータがinteger()以上の場合error.
  ● string, binary
      ○ {max_length, integer()}
          パラメータの長さがinteger()より大きい場合error
      ○ {regexp, string() | binary()}
          パラメータが正規表現string() | binary()にマッチしない場合はerror
      ○ascii
          パラメータがASCII文字列かどうか
      ○not_empty
          パラメータが空文字じゃないかどうか
  ●datetime
      ◯{more,calendar:datetime(}
          パラメータが値より大きいかをチェックする
      ◯{less,calendar:datetime()}
          パラメータが値より小さいかをチェックする
      ◯{range calendar:datetime(), calendar:datetime()}
          パラメータがMin以上Max以下であるかをチェックする
      ◯{equal,calendar:datetime()}
```

<a name="validate-3"></a>

### validate/3 ###


<pre><code>
validate(InputValue, Spec, Option::[Option]) -&gt; {ok, OutputValue} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>InputValue = term()</code></li><li><code>Spec = <a href="#type-spec">spec()</a></code></li><li><code>Option = <a href="#type-option">option()</a></code></li><li><code>OutputValue = term()</code></li><li><code>Reason = term()</code></li></ul>


データ型, 制限を指定して, パラメータがその条件に一致するかを検証する.



オプションを指定することができる(複数指定可能).




#### <a name="オプション一覧">オプション一覧</a> ####


```
  ● binary_in
      バイナリタイプのパラメータのみ受け付ける.
      出力時はバリデートするタイプに変換される.
      ※ 出力タイプが`atom'の場合は、入力に対応するアトムが既にあるかどうかに関わらず、<br />
        強制的にアトムが生成されるのでメモリリークを防ぐため、代わりに`{binary_in, existing_atom}'を使うことを推奨
      また, バリデーション対象の型は通さない.
      例として, integerのバリデーションだとしても, integer型のパラメータは通さない.
  ● {binary_in, option_binary_in_output_type()}
      バイナリタイプのパラメータのみ受け付ける.
      出力時は`option_binary_in_output_type()'で指定されたタイプに変換される.
  ● int_to_bool
      1, 0をbooleanとして扱う. 1がtrue, 0がfalse.
      このオプションを指定した時, true, falseは通さない.
  ● {transform, fun()}
      fun()に任意の関数を指定できる. このオプションを指定した場合,
      パラメータをこの関数に通してからバリデーションを行う.
  ● allow_integer
      typeとしてfloatを指定している時にintegerが来た場合でも許可する.
  ● to_datetime
      datetime型に変換する.変換元はunixtime, iso8601が可能.
  ● {to_datetime, option_to_datetime_input_type()}
      入力形式を指定した型のみに制限する.
```



型をdatetimeに指定した時のbinary_inの詳細な動作は以下の表にまとめる。（リストに入れる順番はbinary_inが先である必要がある)


|                         | `<<"12345">>` | `<<"2014-07-01T01:02:03">>` |
|:------------------------|--------------:|----------------------------:|
|(併記なし)               | error         | error                       |
|`to_datetime`            | ok            | ok                          |
|`{to_datetime, unixtime}`| ok            | error                       |
|`{to_datetime, iso8601}` | error         | ok                          |
