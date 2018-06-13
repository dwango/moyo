

# Module moyo_graph #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

マップに関する処理を集めたユーティリティモジュール.

Copyright (c) 2017 DWANGO Co., Ltd. All Rights Reserved.

<a name="types"></a>

## Data Types ##




### <a name="type-any_error">any_error()</a> ###


<pre><code>
any_error() = {error, term()}
</code></pre>




### <a name="type-graph">graph()</a> ###


<pre><code>
graph() = <a href="#type-graph">graph</a>(<a href="#type-vertex">vertex()</a>)
</code></pre>




### <a name="type-graph">graph()</a> ###


<pre><code>
graph(V) = #{V =&gt; [V]}
</code></pre>




### <a name="type-vertex">vertex()</a> ###


<pre><code>
vertex() = term()
</code></pre>




### <a name="type-vertex_not_found">vertex_not_found()</a> ###


<pre><code>
vertex_not_found() = {error, {vertex_not_found, <a href="#type-vertex">vertex()</a>}}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dfs_post_order-3">dfs_post_order/3</a></td><td>有向グラフに対してpost-orderedな深さ優先探索を行う.</td></tr><tr><td valign="top"><a href="#graph_with_vertices-2">graph_with_vertices/2</a></td><td><code>map</code>で表現されたグラフに、存在しない頂点を追加する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dfs_post_order-3"></a>

### dfs_post_order/3 ###

<pre><code>
dfs_post_order(Graph::<a href="#type-graph">graph</a>(<a href="#type-vertex">vertex()</a>), RootVertex::<a href="#type-vertex">vertex()</a>, VertexFun) -&gt; DFSRet
</code></pre>

<ul class="definitions"><li><code>VertexFun = fun((<a href="#type-vertex">vertex()</a>, [VertexFunOK]) -&gt; VertexFunRet)</code></li><li><code>VertexFunRet = VertexFunOK | <a href="#type-any_error">any_error()</a></code></li><li><code>VertexFunOK = term()</code></li><li><code>DFSRet = VertexFunRet | <a href="#type-vertex_not_found">vertex_not_found()</a></code></li></ul>

`Graph`: `map`形式で格納されたグラフ.<br />`RootVertex`: 探索を開始する頂点.<br />`VertexFun`: 訪れた頂点を処理する関数.<br />

有向グラフに対してpost-orderedな深さ優先探索を行う.

`Graph`は`始点=>[終点のリスト]`という要素を持つ`map`で頂点間の接続関係が記述されている.

一度訪問した頂点は訪問対象から除外されるため,結果として行われるのは,
`RootVertex`から到達可能なサブグラフから得られるスパニングツリーに対する探索となる.

post-orderedな探索であるため,葉から先に探索され,次に共通の先祖へと遡ってゆく.

探索された頂点に対しては順次`VertexFun`が適用される.
兄弟関係にある頂点について適用された`VertexFun`の戻り値は`list`でコレクションされ,
親の頂点に適用される`VertexFun`の第二引数へと渡される.

時間計算量: `O(|E|+|V|log|V|), |E|:枝の数, |V|:頂点の数`

空間計算量: `O(|V|), |V|:頂点の数`

<a name="graph_with_vertices-2"></a>

### graph_with_vertices/2 ###

<pre><code>
graph_with_vertices(Graph::<a href="#type-graph">graph</a>(<a href="#type-vertex">vertex()</a>), Vertices::[<a href="#type-vertex">vertex()</a>]) -&gt; <a href="#type-graph">graph</a>(<a href="#type-vertex">vertex()</a>)
</code></pre>
<br />

`Graph`: `map`形式で格納されたグラフ.<br />`Vertices`: 追加したい頂点のリスト.<br />

`map`で表現されたグラフに、存在しない頂点を追加する.

`Graph`は`頂点=>[頂点のリスト]`という要素を持つ`map`で頂点間の接続関係が記述されている.

追加する頂点は,`Vertices`としてリストの形で渡す.

`Graph`が既に持っている頂点は追加しない.

時間計算量: `O(|V|log|Vall|), |V|:追加しようとしている頂点の数, |Vall|:すべての頂点の数`

空間計算量: `O(|Vall|), |Vall|:すべての頂点の数`

