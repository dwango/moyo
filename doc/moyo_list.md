

# Module moyo_list #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

リストに関する処理を集めたユーティリティモジュール.

Copyright (c) 2013-2014 DWANGO Co., Ltd. All Rights Reserved.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#adjacent_uniq-1">adjacent_uniq/1</a></td><td><code>List</code>内で連接する重複要素を削除する.</td></tr><tr><td valign="top"><a href="#delete_all-2">delete_all/2</a></td><td><code>List</code>内に存在する全ての<code>Element</code>を削除する.</td></tr><tr><td valign="top"><a href="#find_if-2">find_if/2</a></td><td><code>PredFun</code>の結果が<code>true</code>となる<code>List</code>内の最初の要素を検索する.</td></tr><tr><td valign="top"><a href="#foldl_while-3">foldl_while/3</a></td><td>lists:foldl/3 の中断機能追加版: 関数適用後の結果が<code>{false, _}</code>となった場合は、そこで走査が中断される.</td></tr><tr><td valign="top"><a href="#foldr_while-3">foldr_while/3</a></td><td>lists:foldr/3 の中断機能追加版: 関数適用後の結果が<code>{false, _}</code>となった場合は、そこで走査が中断される.</td></tr><tr><td valign="top"><a href="#group_by-2">group_by/2</a></td><td>tuple の N 番目の値でグループ化する.</td></tr><tr><td valign="top"><a href="#inits-1">inits/1</a></td><td><code>List</code>の全ての先頭部分リストを長さの増加する順に並べて返す.</td></tr><tr><td valign="top"><a href="#longest_common_prefix-1">longest_common_prefix/1</a></td><td><code>Lists</code>内のリスト群のLongestCommonPrefixの長さを返す.</td></tr><tr><td valign="top"><a href="#maybe_foldl-3">maybe_foldl/3</a></td><td>lists:foldl/3 の maybe版: 関数適用結果が<code>{error, Reason}</code>となる要素があれば、そこで走査が中断される.</td></tr><tr><td valign="top"><a href="#maybe_foldr-3">maybe_foldr/3</a></td><td>lists:foldr/3 の maybe版: 関数適用結果が<code>{error, Reason}</code>となる要素があれば、そこで走査が中断される.</td></tr><tr><td valign="top"><a href="#maybe_foreach-2">maybe_foreach/2</a></td><td>lists:foreach/2 の maybe版: 関数適用結果が<code>{error, Reason}</code>となる要素があれば、そこで走査が中断される.</td></tr><tr><td valign="top"><a href="#maybe_map-2">maybe_map/2</a></td><td>lists:map/2 の maybe版: 関数適用結果が<code>{error, Reason}</code>となる要素があれば、そこで走査が中断される.</td></tr><tr><td valign="top"><a href="#maybe_pmap-2">maybe_pmap/2</a></td><td>Equivalent to <a href="#maybe_pmap-3"><tt>maybe_pmap(Fun, List, infinity)</tt></a>.</td></tr><tr><td valign="top"><a href="#maybe_pmap-3">maybe_pmap/3</a></td><td><a href="#maybe_map-2"><code>maybe_map/2</code></a>の並列版.</td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td><code>List</code>内で最初に<code>Value</code>が出現する位置を返す.</td></tr><tr><td valign="top"><a href="#replace_if-3">replace_if/3</a></td><td><code>PredFun</code>の結果が<code>true</code>となった最初の要素を<code>Value</code>で置換する.</td></tr><tr><td valign="top"><a href="#shuffle-1">shuffle/1</a></td><td>入力リストの順番を無作為に並べ替える.</td></tr><tr><td valign="top"><a href="#split_longest_common_prefix-1">split_longest_common_prefix/1</a></td><td><code>Lists</code>内の各リストを'LongestCommonPrefix部分'と'それ以降のSuffix部分'に分割する.</td></tr><tr><td valign="top"><a href="#tails-1">tails/1</a></td><td><code>List</code>の全ての末尾部分リストを長さの減少する順に並べて返す.</td></tr><tr><td valign="top"><a href="#take-2">take/2</a></td><td><code>Element</code>と一致する最初の要素を検索し、その値を除いたリストを返す.</td></tr><tr><td valign="top"><a href="#take_if-2">take_if/2</a></td><td><code>PredFun</code>の結果が<code>true</code>となる<code>List</code>内の最初の要素を検索し, その値とその値を除いたリストを返す.</td></tr><tr><td valign="top"><a href="#uniq-1">uniq/1</a></td><td><code>List</code>内で重複要素を削除する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="adjacent_uniq-1"></a>

### adjacent_uniq/1 ###

<pre><code>
adjacent_uniq(List::[term()]) -&gt; [term()]
</code></pre>
<br />

`List`内で連接する重複要素を削除する

リスト全体を通して各要素をユニークにしたい場合は、事前にリストをソートしておくか、
この関数の代わりに[`lists:usort/1`](lists.md#usort-1)または[`moyo_list:uniq/1`](moyo_list.md#uniq-1)を使う必要がある。

なお、要素の一致判定は`=:=`にて行われる (`1.0`と`1`は別要素扱い)

```
  > moyo_list:adjacent_uniq([a, a, b, b, c, c]).
  [a, b, c]
  > moyo_list:adjacent_uniq([a, a, b, b, a, a]).
  [a, b, a]
```

<a name="delete_all-2"></a>

### delete_all/2 ###

<pre><code>
delete_all(Element, List1) -&gt; List2
</code></pre>

<ul class="definitions"><li><code>Element = term()</code></li><li><code>List1 = [Element]</code></li><li><code>List2 = [Element]</code></li></ul>

`List`内に存在する全ての`Element`を削除する

```
  > moyo_list:delete_all(aaa, [aaa, bbb, ccc, bbb, aaa]).
  [bbb, ccc, bbb]
```

<a name="find_if-2"></a>

### find_if/2 ###

<pre><code>
find_if(PredFun, List) -&gt; {ok, Element} | error
</code></pre>

<ul class="definitions"><li><code>PredFun = fun((Element) -&gt; boolean())</code></li><li><code>List = [Element]</code></li><li><code>Element = term()</code></li></ul>

`PredFun`の結果が`true`となる`List`内の最初の要素を検索する

<a name="foldl_while-3"></a>

### foldl_while/3 ###

<pre><code>
foldl_while(Fun, Initial::term(), List) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element, Acc::term()) -&gt; {true, Result} | {false, Result})</code></li><li><code>List = [Element]</code></li><li><code>Element = term()</code></li><li><code>Result = term()</code></li></ul>

lists:foldl/3 の中断機能追加版: 関数適用後の結果が`{false, _}`となった場合は、そこで走査が中断される.

`Fun`の結果は`{true, Result}` or `{false, Result}`のいずれかである必要がある.

<a name="foldr_while-3"></a>

### foldr_while/3 ###

<pre><code>
foldr_while(Fun, Initial::term(), List) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element, Acc::term()) -&gt; {true, Result} | {false, Result})</code></li><li><code>List = [Element]</code></li><li><code>Element = term()</code></li><li><code>Result = term()</code></li></ul>

lists:foldr/3 の中断機能追加版: 関数適用後の結果が`{false, _}`となった場合は、そこで走査が中断される.

`Fun`の結果は`{true, Result}` or `{false, Result}`のいずれかである必要がある.

<a name="group_by-2"></a>

### group_by/2 ###

<pre><code>
group_by(N::non_neg_integer(), TupleList::[tuple()]) -&gt; [{term(), [tuple()]}]
</code></pre>
<br />

tuple の N 番目の値でグループ化する.

<a name="inits-1"></a>

### inits/1 ###

<pre><code>
inits(List::[Element]) -&gt; [[Element]]
</code></pre>

<ul class="definitions"><li><code>Element = term()</code></li></ul>

`List`の全ての先頭部分リストを長さの増加する順に並べて返す

関数名およびインタフェースはHaskellの`Data.List.inits関数`に倣った

```
  > moyo_list:inits("abc").
  ["", "a", "ab", "abc"]
```

<a name="longest_common_prefix-1"></a>

### longest_common_prefix/1 ###

<pre><code>
longest_common_prefix(Lists) -&gt; LongestCommonPrefixLength
</code></pre>

<ul class="definitions"><li><code>Lists = [List]</code></li><li><code>List = [term()]</code></li><li><code>LongestCommonPrefixLength = non_neg_integer()</code></li></ul>

`Lists`内のリスト群のLongestCommonPrefixの長さを返す

[`binary:longest_common_prefix/1`](binary.md#longest_common_prefix-1)のリスト版

```
  > moyo_list:longest_common_prefix(["erlang", "ergonomy"]).
  2
  > moyo_list:longest_common_prefix(["erlang", "perl"]).
  0
```

<a name="maybe_foldl-3"></a>

### maybe_foldl/3 ###

<pre><code>
maybe_foldl(Fun, Initial::term(), List) -&gt; {ok, Result::term()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element, Acc::term()) -&gt; {ok, AccNext::term()} | {error, Reason})</code></li><li><code>List = [Element]</code></li><li><code>Element = term()</code></li><li><code>Reason = term()</code></li></ul>

lists:foldl/3 の maybe版: 関数適用結果が`{error, Reason}`となる要素があれば、そこで走査が中断される.

`Fun`の結果は `{ok, Result}` or `{error, Reason}` のいずれかである必要がある.

<a name="maybe_foldr-3"></a>

### maybe_foldr/3 ###

<pre><code>
maybe_foldr(Fun, Initial::term(), List) -&gt; {ok, Result::term()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element, Acc::term()) -&gt; {ok, AccNext::term()} | {error, Reason})</code></li><li><code>List = [Element]</code></li><li><code>Element = term()</code></li><li><code>Reason = term()</code></li></ul>

lists:foldr/3 の maybe版: 関数適用結果が`{error, Reason}`となる要素があれば、そこで走査が中断される.

`Fun`の結果は `{ok, Result}` or `{error, Reason}` のいずれかである必要がある.

<a name="maybe_foreach-2"></a>

### maybe_foreach/2 ###

<pre><code>
maybe_foreach(Fun, List) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element) -&gt; {error, Reason} | any())</code></li><li><code>List = [Element]</code></li><li><code>Element = term()</code></li><li><code>Reason = term()</code></li></ul>

lists:foreach/2 の maybe版: 関数適用結果が`{error, Reason}`となる要素があれば、そこで走査が中断される.

`Fun`の結果が`{error, Reason}`の場合はそこで走査が中断され、それ以外の場合は継続される.

<a name="maybe_map-2"></a>

### maybe_map/2 ###

<pre><code>
maybe_map(Fun, List) -&gt; {ok, Result} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element) -&gt; {ok, PerResult} | {error, Reason})</code></li><li><code>List = [Element]</code></li><li><code>Element = term()</code></li><li><code>Result = [PerResult]</code></li><li><code>PerResult = term()</code></li><li><code>Reason = term()</code></li></ul>

lists:map/2 の maybe版: 関数適用結果が`{error, Reason}`となる要素があれば、そこで走査が中断される.

`Fun`の結果は `{ok, Result}` or `{error, Reason}` のいずれかである必要がある.

<a name="maybe_pmap-2"></a>

### maybe_pmap/2 ###

<pre><code>
maybe_pmap(Fun, List) -&gt; {ok, Values} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Arg) -&gt; {ok, Value} | {error, Reason})</code></li><li><code>List = [Arg]</code></li><li><code>Values = [Value]</code></li><li><code>Arg = term()</code></li><li><code>Value = term()</code></li><li><code>Reason = ExitError | term()</code></li><li><code>ExitError = {'EXIT', {ExitReason::term(), StackTrace::term()}}</code></li></ul>

Equivalent to [`maybe_pmap(Fun, List, infinity)`](#maybe_pmap-3).

<a name="maybe_pmap-3"></a>

### maybe_pmap/3 ###

<pre><code>
maybe_pmap(Fun, List, Timeout) -&gt; {ok, Values} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Arg) -&gt; {ok, Value} | {error, Reason})</code></li><li><code>List = [Arg]</code></li><li><code>Timeout = timeout()</code></li><li><code>Values = [Value]</code></li><li><code>Arg = term()</code></li><li><code>Value = term()</code></li><li><code>Reason = ExitError | ExitTimeout | term()</code></li><li><code>ExitError = {'EXIT', {ExitReason::term(), StackTrace::term()}}</code></li><li><code>ExitTimeout = {'EXIT', timeout}</code></li></ul>

[`maybe_map/2`](#maybe_map-2)の並列版.

`Fun`の実行中のエラーが発生した場合は`ExitError`、タイムアウトが発生した場合は`ExitTimeout`が結果として返される.
また, 1つでも関数適用結果が`{error, Reason}`となる要素があれば, そこで走査を中断し、残ったプロセスを終了させる.

<a name="position-2"></a>

### position/2 ###

<pre><code>
position(Value::term(), List::list()) -&gt; {ok, Position::pos_integer()} | error
</code></pre>
<br />

`List`内で最初に`Value`が出現する位置を返す

`Value`が存在しない場合は`error`が返される

<a name="replace_if-3"></a>

### replace_if/3 ###

<pre><code>
replace_if(PredFun, Value::term(), List::list()) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>PredFun = fun((term()) -&gt; boolean())</code></li></ul>

`PredFun`の結果が`true`となった最初の要素を`Value`で置換する

<a name="shuffle-1"></a>

### shuffle/1 ###

<pre><code>
shuffle(List::[Element]) -&gt; [Element]
</code></pre>

<ul class="definitions"><li><code>Element = term()</code></li></ul>

入力リストの順番を無作為に並べ替える

<a name="split_longest_common_prefix-1"></a>

### split_longest_common_prefix/1 ###

<pre><code>
split_longest_common_prefix(Lists) -&gt; {LongestCommonPrefix, [Suffix]}
</code></pre>

<ul class="definitions"><li><code>Lists = [List]</code></li><li><code>List = [term()]</code></li><li><code>LongestCommonPrefix = List</code></li><li><code>Suffix = List</code></li></ul>

`Lists`内の各リストを'LongestCommonPrefix部分'と'それ以降のSuffix部分'に分割する

なお'LongestCommonPrefix部分'は全てのリストで共通のため、結果では一つにまとめられている

```
  > moyo_list:split_longest_common_prefix(["erlang", "ergonomy"]).
  {"er", ["lang", "gonomy"]}
  > moyo_list:split_longest_common_prefix(["erlang", "perl"]).
  {"", ["erlang", "perl"]}
```

<a name="tails-1"></a>

### tails/1 ###

<pre><code>
tails(List::[Element]) -&gt; [[Element]]
</code></pre>

<ul class="definitions"><li><code>Element = term()</code></li></ul>

`List`の全ての末尾部分リストを長さの減少する順に並べて返す

関数名およびインタフェースはHaskellの`Data.List.tails関数`に倣った

```
  > moyo_list:tails("abc").
  ["abc", "ab", "a", ""]
```

<a name="take-2"></a>

### take/2 ###

<pre><code>
take(Element, List1) -&gt; {ok, List2} | error
</code></pre>

<ul class="definitions"><li><code>Element = term()</code></li><li><code>List1 = [Element]</code></li><li><code>List2 = [Element]</code></li></ul>

`Element`と一致する最初の要素を検索し、その値を除いたリストを返す

```
  > moyo_list:take(bbb, [aaa, bbb, ccc]).
  {ok, [aaa, ccc]}.
  > moyo_list:take(bbb, [111, 222, 333]).
  error
```

<a name="take_if-2"></a>

### take_if/2 ###

<pre><code>
take_if(PredFun, List1) -&gt; {ok, Element, List2} | error
</code></pre>

<ul class="definitions"><li><code>PredFun = fun((Element) -&gt; boolean())</code></li><li><code>List1 = [Element]</code></li><li><code>Element = term()</code></li><li><code>List2 = [Element]</code></li></ul>

`PredFun`の結果が`true`となる`List`内の最初の要素を検索し, その値とその値を除いたリストを返す.

<a name="uniq-1"></a>

### uniq/1 ###

<pre><code>
uniq(List::[term()]) -&gt; [term()]
</code></pre>
<br />

`List`内で重複要素を削除する

計算量は`O(n log n)`. 要素の出現順は保存される.
リストがソートされてもよい場合は[`lists:usort/1`](lists.md#usort-1)の方が高速.
連接する重複要素のみを削除したい場合はこの関数の代わりに[`moyo_list:adjacent_uniq/1`](moyo_list.md#adjacent_uniq-1)を使う.

なお, 要素の一致判定は`=:=`にて行われる (`1.0`と`1`は別要素扱い)

```
  > moyo_list:uniq([a, a, b, b, c, c]).
  [a, b, c]
  > moyo_list:uniq([c, a, c, b, b, a]).
  [c, a, b]
```

