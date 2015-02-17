

# Module moyo_assoc #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


連想リストに関する処理を集めたユーティリティモジュール.
Copyright (c) 2013-2015 DWANGO Co., Ltd. All Rights Reserved.

<a name="description"></a>

## Description ##


連想リストとは、以下のようにキーと値が対となったタプルを要素として保持するリストのことを表す。

```
  [
    {key, value},
    {"キー", "値"},
    {editors, [<<"vim">>, <<"emacs">>, <<"notepad">>]}
  ]
```




#### <a name="【重複キーの扱いについて】">【重複キーの扱いについて】</a> ####


重複したキーを持つ要素を複数リスト内に保持することは可能だが、<br />
その場合、連想リストの操作時には、同じキーを有する要素群の内の最初のもの以外は無視される。<br />



例えば、検索関数の場合は、最初に出現した要素の値が採用され、<br />
削除関数では、最初に出現した要素のみが削除されることになる。<br />
(つまり、一回の削除関数呼び出しで、重複キーを持つ全ての要素が除去されることはない)



ただし、明示的に重複キーの扱い方が定義されている関数に関しては、その限りではない。


#### <a name="【要素の並び順に関して】">【要素の並び順に関して】</a> ####


本モジュールは連想リストをセットの一種として扱うため、原則として要素の順番は考慮されない。


そのため、ある関数を適用した結果、連想リストの論理的な内容は同一でも、<br />
実際の内容(要素の並び順)は、適用前とは変わっていることもあるので、注意が必要。 <br />
※ ただし、例外として連想リストが重複キーを含む場合は、それらの間の順番(前後関係)は常に維持される。
<a name="types"></a>

## Data Types ##




### <a name="type-assoc_list">assoc_list()</a> ###



<pre><code>
assoc_list() = <a href="#type-assoc_list">assoc_list</a>(<a href="#type-key">key()</a>, <a href="#type-value">value()</a>)
</code></pre>





### <a name="type-assoc_list">assoc_list()</a> ###



<pre><code>
assoc_list(Key, Value) = [{Key, Value}]
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = term()
</code></pre>





### <a name="type-validate_entry_spec">validate_entry_spec()</a> ###



<pre><code>
validate_entry_spec() = {KeySpec::(<a href="#type-key">key()</a> | {<a href="#type-key">key()</a>, <a href="#type-key">key()</a>}), ValueSpec::(<a href="moyo_validator.md#type-spec">moyo_validator:spec()</a>), Options::([<a href="moyo_validator.md#type-option">moyo_validator:option()</a> | <a href="#type-validate_option_ext">validate_option_ext()</a>])}
</code></pre>




  要素のバリデーション指定. <br />



[KeySpec] <br />
対象要素のキー名を指定する. <br />
`{From, To}`形式で指定した場合は、検索は`From`で行われ、結果としては`To`がキー名として使用される. (キー名のリネーム)<br />



[ValueSpec] <br />
値のバリデーション方法を指定する. <br />
詳細は`moyo_validator:spec/0`を参照のこと. <br />


[Options] <br />
バリデーションオプションを指定する. <br />
詳細は`moyo_validator:option/0`および'moyo_assoc:validate_option_ext/0'を参照のこと. <br />




### <a name="type-validate_option_ext">validate_option_ext()</a> ###



<pre><code>
validate_option_ext() = {default, DefaultValue::term()} | optional
</code></pre>



`moyo_assoc`独自のバリデートオプション: <br />
- {default, DefaultValue}: 指定された場合は、要素が存在しない場合に、エラーではなく`DefaultValue`を返す <br />
- optional: 指定された場合は、要素が存在しない場合に、エラーではなく単に結果から除外される <br />




### <a name="type-value">value()</a> ###



<pre><code>
value() = term()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>キーに対応する要素を削除する.</td></tr><tr><td valign="top"><a href="#diff-2">diff/2</a></td><td>2つの連想リストから共通のタプルとどちらかのリストにしかないタプルとキーがList1とLIst2で異なるタプルを分ける.</td></tr><tr><td valign="top"><a href="#equal-2">equal/2</a></td><td>2つの連想リストが同じかどうかを比較する.</td></tr><tr><td valign="top"><a href="#fetch-2">fetch/2</a></td><td>キーに対応する値を取得する.</td></tr><tr><td valign="top"><a href="#fetch-3">fetch/3</a></td><td>キーに対応する値を取得する.</td></tr><tr><td valign="top"><a href="#fetch_as-3">fetch_as/3</a></td><td>キーに対応する値を<code>ValueSpec</code>で指定された方式で取得する.</td></tr><tr><td valign="top"><a href="#fetch_values-2">fetch_values/2</a></td><td>複数の値を一度に取得する.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>mapから連想リストを生成する.</td></tr><tr><td valign="top"><a href="#from_record-2">from_record/2</a></td><td>レコードを連想リスト形式に変換する.</td></tr><tr><td valign="top"><a href="#intersection_and_differences-2">intersection_and_differences/2</a></td><td>2つの連想リストから共通のタプルとどちらかのリストにしかないタプルを分ける.</td></tr><tr><td valign="top"><a href="#is_assoc_list-1">is_assoc_list/1</a></td><td>引数の値が連想リストかどうかを判定する.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>キーのリストを生成する.</td></tr><tr><td valign="top"><a href="#keys_as_set-1">keys_as_set/1</a></td><td>キーの集合を生成する.</td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td>キーに対応する値を検索する.</td></tr><tr><td valign="top"><a href="#lookup_as-3">lookup_as/3</a></td><td>キーに対応する値を<code>ValueSpec</code>で指定された方式で取得する.</td></tr><tr><td valign="top"><a href="#lookup_entries-2">lookup_entries/2</a></td><td><code>KeyList</code>で指定されたエントリー(要素)一覧を取得する.</td></tr><tr><td valign="top"><a href="#lookup_entries_as-2">lookup_entries_as/2</a></td><td><code>EntrySpecList</code>で指定された方式で、エントリー(要素)一覧を取得する.</td></tr><tr><td valign="top"><a href="#lookup_values-2">lookup_values/2</a></td><td><code>KeyList</code>で指定されたキーに対応する値一覧を取得する.</td></tr><tr><td valign="top"><a href="#lookup_values_as-2">lookup_values_as/2</a></td><td><code>EntrySpecList</code>で指定された方式で、値一覧を取得する.</td></tr><tr><td valign="top"><a href="#merge-2">merge/2</a></td><td>2つの連想リストをマージする.</td></tr><tr><td valign="top"><a href="#pop-2">pop/2</a></td><td>キーに対応するリストの先頭から値を取り出す.</td></tr><tr><td valign="top"><a href="#push-3">push/3</a></td><td>キーに対応するリストの先頭に値を追加する.</td></tr><tr><td valign="top"><a href="#rfetch-2">rfetch/2</a></td><td>再帰的にキーに対応する値を取得する.</td></tr><tr><td valign="top"><a href="#rfetch-3">rfetch/3</a></td><td>再帰的にキーに対応する値を取得する.</td></tr><tr><td valign="top"><a href="#rupdate-4">rupdate/4</a></td><td>キーリストに対応する要素の値を更新する.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td>連想リストに要素を追加する.</td></tr><tr><td valign="top"><a href="#store_if_not_exist-3">store_if_not_exist/3</a></td><td>既にキーが存在しない場合にのみ、連想リストに要素を追加する.</td></tr><tr><td valign="top"><a href="#take-2">take/2</a></td><td>キーに対応する要素を連想リストから取り出す(取り除く).</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>連想リストからmapを生成する.</td></tr><tr><td valign="top"><a href="#to_record-3">to_record/3</a></td><td>連想リストからレコードを生成する.</td></tr><tr><td valign="top"><a href="#to_record_as-4">to_record_as/4</a></td><td>連想リストからレコードを生成する.</td></tr><tr><td valign="top"><a href="#unique_by_key-1">unique_by_key/1</a></td><td>重複したキーを持つ要素を除去した連想リストを生成する.</td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td>キーに対応する要素の値を更新する.</td></tr><tr><td valign="top"><a href="#values-1">values/1</a></td><td>値のリストを生成する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-2"></a>

### delete/2 ###


<pre><code>
delete(Key::<a href="#type-key">key()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-assoc_list">assoc_list()</a>
</code></pre>
<br />

キーに対応する要素を削除する.
<a name="diff-2"></a>

### diff/2 ###


<pre><code>
diff(AssocList1, AssocList2) -&gt; {EqualList, ValueDiffList, Only1List, Only2List}
</code></pre>

<ul class="definitions"><li><code>AssocList1 = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>AssocList2 = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>EqualList = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>ValueDiffList = <a href="#type-assoc_list">assoc_list</a>(Key::term(), {Before::term(), After::term()})</code></li><li><code>Only1List = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>Only2List = <a href="#type-assoc_list">assoc_list()</a></code></li></ul>


2つの連想リストから共通のタプルとどちらかのリストにしかないタプルとキーがList1とLIst2で異なるタプルを分ける



出力は, {共通のタプル, キーが同じでvalueが変更されたタプル, リスト1にだけあるタプル, リスト2にだけあるタプル}.

最初に2つのリストの中身をdictに展開し、他方のリストの探索をしやすいようにする。<br />
gb_treesをdictの代わりに使わないのは、gb_treesでは1と1.0が同一視される(=:=ではなく==での比較が行われているため)が、1.0と1は違うものと扱いたいから。<br />
それらを元に、重複する要素を消したリストを生成する。<br />
そのあと、EqualList,ValueDiffList,Only1List,Only2Listをリスト内包表記でフィルタリングしながら生成する。<br />
key,valueともに値の比較は=:=で行っているため、1と1.0は別物として扱われる点に注意すること。<br />


ex:

```
  1> moyo_assoc:diff
  1> ([{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}, {key7, value7}, {key1, value2} ],
  1> [{key1, value2}, {key2, value2}, {key3, value3}, {key4, value4}, {key6, value6}]).
  {[{key2, value2}, {key3, value3}, {key4, value4}],
   [{key1, {value1, value2}}],
   [{key5, value5}, {key7, value7}],
   [{key6, value6}]}
```

<a name="equal-2"></a>

### equal/2 ###


<pre><code>
equal(AssocList1, AssocList2) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>AssocList1 = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>AssocList2 = <a href="#type-assoc_list">assoc_list()</a></code></li></ul>


2つの連想リストが同じかどうかを比較する.



同じ場合は, true, 異なる場合はfalse.
ただし, 重複キーや順の扱いは他の連想リストと同じ扱いである.


ex:

```
  1> moyo_assoc:equal([{key1, value1}, {key2, value2}, {key1, different_value}], [{key2, value2}, {key1, value1}]).
  true
```

<a name="fetch-2"></a>

### fetch/2 ###


<pre><code>
fetch(Key::<a href="#type-key">key()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />


キーに対応する値を取得する.


キーが存在しない場合は、例外が送出される.
<a name="fetch-3"></a>

### fetch/3 ###


<pre><code>
fetch(Key::<a href="#type-key">key()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>, DefaultValue::<a href="#type-value">value()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />


キーに対応する値を取得する.


キーが存在しない場合は、デフォルト値が代わりに返される.
<a name="fetch_as-3"></a>

### fetch_as/3 ###


<pre><code>
fetch_as(Key, ValueSpec, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>

<ul class="definitions"><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>ValueSpec = {<a href="moyo_validator.md#type-spec">moyo_validator:spec()</a>, [<a href="moyo_validator.md#type-option">moyo_validator:option()</a>]}</code></li></ul>


キーに対応する値を`ValueSpec`で指定された方式で取得する


`ValuesSpec`の詳細は[`moyo_validator:validate/3`](moyo_validator.md#validate-3)を参照のこと
<a name="fetch_values-2"></a>

### fetch_values/2 ###


<pre><code>
fetch_values(KeyList::[<a href="#type-key">key()</a>], AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; [<a href="#type-value">value()</a>]
</code></pre>
<br />

複数の値を一度に取得する
<a name="from_map-1"></a>

### from_map/1 ###


<pre><code>
from_map(Map::#{}) -&gt; <a href="moyo_assoc.md#type-assoc_list">moyo_assoc:assoc_list()</a>
</code></pre>
<br />


mapから連想リストを生成する.



maps:to_list/1 の結果と同じ.


ex:

```
  1> moyo_assoc:from_map(#{key1 => value1,key2 => value2,key3 => value3,key4 => value4,key5 => value5}).
  [{key1,value1},
   {key2,value2},
   {key3,value3},
   {key4,value4},
   {key5,value5}]
```

<a name="from_record-2"></a>

### from_record/2 ###


<pre><code>
from_record(Fields::[atom()], Record) -&gt; <a href="#type-assoc_list">assoc_list()</a>
</code></pre>

<ul class="definitions"><li><code>Record = tuple()</code></li></ul>


レコードを連想リスト形式に変換する.


`Fields` の値は `record_info(fields, RecordName)` で取得できる. <br />
以下、使用例:

```
  > rd(sample, {a, b, c}).
  > Record = #sample{a = 10, b = 20, c = 30}.
  #sample{a = 10,b = 20,c = 30}
  > Fields = record_info(fields, sample).
  [a,b,c]
  > moyo_assoc:from_record(Fields, Record).
  [{a,10},{b,20},{c,30}]
```

<a name="intersection_and_differences-2"></a>

### intersection_and_differences/2 ###


<pre><code>
intersection_and_differences(AssocList1, AssocList2) -&gt; {Intersec, Diff1, Diff2}
</code></pre>

<ul class="definitions"><li><code>AssocList1 = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>AssocList2 = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>Intersec = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>Diff1 = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>Diff2 = <a href="#type-assoc_list">assoc_list()</a></code></li></ul>


2つの連想リストから共通のタプルとどちらかのリストにしかないタプルを分ける.



出力は, {共通のタプル, リスト1にだけあるタプル, リスト2にだけあるタプル}.


ex:

```
  1> moyo_assoc:intersection_and_differences
  1> ([{key2, value2}, {key1, value1}, {key4, value4}],
  1> [{key3, value3}, {key1, value1}, {key5, value5}, {key2, value4}, {key2, value2}, {key4, value4}]).
  {[{key1,value1},{key4,value4}],
   [{key2,value2}],
   [{key2,value4},{key3,value3},{key5,value5}]}
```

<a name="is_assoc_list-1"></a>

### is_assoc_list/1 ###


<pre><code>
is_assoc_list(Value::term()) -&gt; boolean()
</code></pre>
<br />

引数の値が連想リストかどうかを判定する.
<a name="keys-1"></a>

### keys/1 ###


<pre><code>
keys(AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; [<a href="#type-key">key()</a>]
</code></pre>
<br />


キーのリストを生成する.



この関数は重複キーを考慮しない.<br />
重複キーが除去された情報が欲しい場合は, 下記を検討すること.



1. [`unique_by_key/1`](#unique_by_key-1) との併用
1. [`keys_as_set/1`](#keys_as_set-1) の利用


ex:

```
  1> moyo_assoc:keys([{key1, value1}, {key2, value2}]).
  [key1, key2]
```

<a name="keys_as_set-1"></a>

### keys_as_set/1 ###


<pre><code>
keys_as_set(AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="gb_sets.md#type-set">gb_sets:set</a>(<a href="#type-key">key()</a>)
</code></pre>
<br />


キーの集合を生成する.


gb_sets:set() の性質上, 重複キーは除去される

<a name="lookup-2"></a>

### lookup/2 ###


<pre><code>
lookup(Key::<a href="#type-key">key()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; error | {ok, <a href="#type-value">value()</a>}
</code></pre>
<br />

キーに対応する値を検索する.
<a name="lookup_as-3"></a>

### lookup_as/3 ###


<pre><code>
lookup_as(Key, EntrySpec, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>EntrySpec = {<a href="moyo_validator.md#type-spec">moyo_validator:spec()</a>, [<a href="moyo_validator.md#type-option">moyo_validator:option()</a>]}</code></li><li><code>Reason = not_found | term()</code></li></ul>


キーに対応する値を`ValueSpec`で指定された方式で取得する


`EntrySpec`の詳細は[`moyo_validator:validate/3`](moyo_validator.md#validate-3)を参照のこと
<a name="lookup_entries-2"></a>

### lookup_entries/2 ###


<pre><code>
lookup_entries(KeyList, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; {ok, <a href="#type-assoc_list">assoc_list()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>KeyList = [<a href="#type-key">key()</a>]</code></li><li><code>Reason = term()</code></li></ul>

`KeyList`で指定されたエントリー(要素)一覧を取得する
<a name="lookup_entries_as-2"></a>

### lookup_entries_as/2 ###


<pre><code>
lookup_entries_as(EntrySpecList, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; {ok, <a href="#type-assoc_list">assoc_list()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>EntrySpecList = [<a href="#type-validate_entry_spec">validate_entry_spec()</a>]</code></li><li><code>Reason = term()</code></li></ul>


`EntrySpecList`で指定された方式で、エントリー(要素)一覧を取得する



指定方法の詳細に関しては`validate_entry_spec()`のドキュメント及び`moyo_validator`モジュールを参照のこと. <br />


使用例:

```
  > AssocList = [{key1, 10}, {key2, abc}].
  　
  %% 基本的な使用方法
  > lookup_entries_as([{key1, integer, []},
                       {key2, atom, []}],
                      AssocList).
  {ok, [{key1, 10}, {key2, abc}]}
  　
  %% キー名を変更する
  > lookup_entries_as([{{key1, new_key1}, integer, []}], AssocList).
  {ok, [{new_key1, 10}]}
  　
  %% デフォルト値を指定する
  > lookup_entries_as([{key3, integer, [{default, 30}]}], AssocList).
  {ok, [{key3, 30}]}
```

<a name="lookup_values-2"></a>

### lookup_values/2 ###


<pre><code>
lookup_values(KeyList, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; {ok, [<a href="#type-value">value()</a>]} | {error, Reason::term()}
</code></pre>

<ul class="definitions"><li><code>KeyList = [<a href="#type-key">key()</a>]</code></li></ul>

`KeyList`で指定されたキーに対応する値一覧を取得する
<a name="lookup_values_as-2"></a>

### lookup_values_as/2 ###


<pre><code>
lookup_values_as(EntrySpecList, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; {ok, [<a href="#type-value">value()</a>]} | {error, Reason::term()}
</code></pre>

<ul class="definitions"><li><code>EntrySpecList = [{<a href="#type-key">key()</a>, <a href="moyo_validator.md#type-spec">moyo_validator:spec()</a>, [<a href="moyo_validator.md#type-option">moyo_validator:option()</a> | <a href="#type-validate_option_ext">validate_option_ext()</a>]}]</code></li></ul>


`EntrySpecList`で指定された方式で、値一覧を取得する


以下のコードとほぼ透過:

```
  > {ok, Entries} = lookup_entries_as(EntrySpecList, AssocList).
  > {ok, [V || {_, V} <- Entries]}.
```

<a name="merge-2"></a>

### merge/2 ###


<pre><code>
merge(AssocList1::<a href="#type-assoc_list">assoc_list()</a>, AssocList2::<a href="#type-assoc_list">assoc_list()</a>) -&gt; Result::<a href="#type-assoc_list">assoc_list()</a>
</code></pre>
<br />


2つの連想リストをマージする.



2つの連想リストにおいて、片方のリストにしかkeyが存在しないものは、そのまま結果のリストに加える。両方のリストに同じkeyがある場合、List1の方のkey、valueペアを結果のリストに加える。この関数において、keyの同値判定は=:=ではなく==で行っている。



出力は, {演算した結果の連想リスト}



２つのリストを++で連結したあと、ukeysortで重複するkeyは最初のもののみ考慮するようにするという実装方法で実現している。


ex:

```
  1> moyo_assoc:merge
  1> ([{key1, value11}, {key3, value31}, {1, value01}, {key3, value312}], [{key1, value12}, {key2, value22}, {1.0, value02}]).
  [{key1, value11}, {key3, value31}, {1, value01}, {key2, value22}]
```

<a name="pop-2"></a>

### pop/2 ###


<pre><code>
pop(Key::<a href="#type-key">key()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; {Result, <a href="#type-assoc_list">assoc_list()</a>}
</code></pre>

<ul class="definitions"><li><code>Result = {value, <a href="#type-value">value()</a>} | empty</code></li></ul>


キーに対応するリストの先頭から値を取り出す.


キーが存在しない場合は、要素の値が空リストとして扱われる (つまり結果として empty が返される).<br />
キーに対応する値が存在し、かつリスト以外の場合は、例外が送出される.
<a name="push-3"></a>

### push/3 ###


<pre><code>
push(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-assoc_list">assoc_list()</a>
</code></pre>
<br />


キーに対応するリストの先頭に値を追加する.


キーが存在しない場合は、追加する値のみを含むリストが新規に生成される.<br />
キーに対応する値が存在し、かつリスト以外の場合は、例外が送出される.
<a name="rfetch-2"></a>

### rfetch/2 ###


<pre><code>
rfetch(KeyList::[<a href="#type-key">key()</a>], AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />


再帰的にキーに対応する値を取得する.


`KeyList`の先頭から順番に対応する値を取得し、その値に対して`fetch`を適用する<br />
キーが存在しない場合は、例外が送出される.
以下、使用例:

```
  > rfetch([key1_1, key2_1, key3_1],
  >        [{key1_1, [{key2_1, [{key3_1, value3_1}, {key3_2, value3_2}]}, {key2_2, value2_2}]}, {key1_2, value1_2}]
  >       ).
  value3_1
```

<a name="rfetch-3"></a>

### rfetch/3 ###


<pre><code>
rfetch(KeyList::[<a href="#type-key">key()</a>], AssocList::<a href="#type-assoc_list">assoc_list()</a>, DefaultValue::<a href="#type-value">value()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />


再帰的にキーに対応する値を取得する.


`KeyList`の先頭から順番に対応する値を取得し、その値に対して`fetch`を適用する<br />
キーが存在しない場合は、デフォルト値が代わりに返される.
以下、使用例:

```
  > rfetch([key1_1, key2_1, key3_1, key4_1],
  >        [{key1_1, [{key2_1, [{key3_1, value3_1}, {key3_2, value3_2}]}, {key2_2, value2_2}]}, {key1_2, value1_2}],
  >        default_value
  >       ).
  default_value
```

<a name="rupdate-4"></a>

### rupdate/4 ###


<pre><code>
rupdate(KeyList::[<a href="#type-key">key()</a>], UpdateFun, Initial, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-assoc_list">assoc_list()</a>
</code></pre>

<ul class="definitions"><li><code>UpdateFun = fun((OldValue::<a href="#type-value">value()</a>) -&gt; NewValue::<a href="#type-value">value()</a>)</code></li><li><code>Initial = <a href="#type-value">value()</a></code></li></ul>


キーリストに対応する要素の値を更新する.


キー(リスト)に対応する要素が存在しない場合は`Initial`を値とする要素が新たに追加される.<br />
キーリストの初めの方でタプルがなかった場合でも追加される.<br />
以下、キーがない場合の例:

```
  > rupdate([banana, color], fun(X) -> X end, yellow, []).
  [{banana, [{color, yellow}]}]
```

<a name="store-3"></a>

### store/3 ###


<pre><code>
store(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-assoc_list">assoc_list()</a>
</code></pre>
<br />


連想リストに要素を追加する.


追加しようとしている要素のキーが既に存在している場合は、その要素が取り除かれた上で、新しい要素が追加される。
<a name="store_if_not_exist-3"></a>

### store_if_not_exist/3 ###


<pre><code>
store_if_not_exist(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; {Stored::boolean(), <a href="#type-assoc_list">assoc_list()</a>}
</code></pre>
<br />

既にキーが存在しない場合にのみ、連想リストに要素を追加する.
<a name="take-2"></a>

### take/2 ###


<pre><code>
take(Key::<a href="#type-key">key()</a>, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; error | {ok, <a href="#type-value">value()</a>, <a href="#type-assoc_list">assoc_list()</a>}
</code></pre>
<br />

キーに対応する要素を連想リストから取り出す(取り除く)
<a name="to_map-1"></a>

### to_map/1 ###


<pre><code>
to_map(Fields::<a href="moyo_assoc.md#type-assoc_list">moyo_assoc:assoc_list()</a>) -&gt; #{}
</code></pre>
<br />


連想リストからmapを生成する.



連想リスト内に重複するキーが存在した場合は先に現れた値が使われる.


ex:

```
  1> moyo_assoc:to_map([{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}]).
  #{key1 => value1,key2 => value2,key3 => value3,key4 => value4,key5 => value5}
```

<a name="to_record-3"></a>

### to_record/3 ###


<pre><code>
to_record(RecordName, Fields, Params) -&gt; Record
</code></pre>

<ul class="definitions"><li><code>RecordName = atom()</code></li><li><code>Fields = [atom()]</code></li><li><code>Params = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>Record = tuple()</code></li></ul>


連想リストからレコードを生成する.


`Fields` の値は `record_info(fields, RecordName)` で取得できる. <br />
以下、使用例:

```
  > rd(sample, {a, b, c}).
  > Params = [{a, 10}, {b, 20}, {c, 30}].
  > moyo_assoc:to_record(sample, record_info(fields, sample), Params).
  #sample{a = 10,b = 20,c = 30}
```

<a name="to_record_as-4"></a>

### to_record_as/4 ###


<pre><code>
to_record_as(RecordName, Fields, FieldSpecList, Params) -&gt; {ok, <a href="#type-assoc_list">assoc_list()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>RecordName = atom()</code></li><li><code>Fields = [atom()]</code></li><li><code>FieldSpecList = [<a href="#type-validate_entry_spec">validate_entry_spec()</a>]</code></li><li><code>Params = <a href="#type-assoc_list">assoc_list()</a></code></li><li><code>Reason = term()</code></li></ul>


連想リストからレコードを生成する.


連想リストから要素を取得する際には`moyo_validator`を使用して、値の検証および変換が行われる. <br />
以下のコードとほぼ等価:

```
  > {ok, Entries} = lookup_entries_as(FieldSpecList, Params).
  > to_record(RecordName, Fields, Entries).
```

<a name="unique_by_key-1"></a>

### unique_by_key/1 ###


<pre><code>
unique_by_key(AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-assoc_list">assoc_list()</a>
</code></pre>
<br />


重複したキーを持つ要素を除去した連想リストを生成する.



重複するキーが存在した場合は先に現れた値が使われる.


ex:

```
  1> moyo_assoc:unique_by_key([{key1, value1}, {key1, value2}]).
  [{key1, value1}]
```

<a name="update-4"></a>

### update/4 ###


<pre><code>
update(Key::<a href="#type-key">key()</a>, UpdateFun, Initial, AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; <a href="#type-assoc_list">assoc_list()</a>
</code></pre>

<ul class="definitions"><li><code>UpdateFun = fun((OldValue::<a href="#type-value">value()</a>) -&gt; NewValue::<a href="#type-value">value()</a>)</code></li><li><code>Initial = <a href="#type-value">value()</a></code></li></ul>


キーに対応する要素の値を更新する.


キーに対応する要素が存在しない場合は`Initial`を値とする要素が新たに追加される.
<a name="values-1"></a>

### values/1 ###


<pre><code>
values(AssocList::<a href="#type-assoc_list">assoc_list()</a>) -&gt; [<a href="#type-value">value()</a>]
</code></pre>
<br />


値のリストを生成する.



この関数は重複キーを考慮しない.<br />
重複キーが除去された情報が欲しい場合は, [`unique_by_key/1`](#unique_by_key-1) との併用を検討すること.


ex:

```
  1> moyo_assoc:values([{key1, value1}, {key2, value2}]).
  [value1, value2]
```

