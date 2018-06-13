%% @copyright 2017 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc マップに関する処理を集めたユーティリティモジュール.
-module(moyo_graph).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         dfs_post_order/3,
         graph_with_vertices/2
        ]).

-export_type([
              graph/1,
              graph/0,
              vertex/0,
              vertex_not_found/0,
              any_error/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-type graph(V)           :: #{V => [V]}.
-type graph()            :: graph(vertex()).
-type vertex()           :: term().
-type vertex_not_found() :: {error, {vertex_not_found, vertex()}}.
-type any_error()        :: {error, term()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc 有向グラフに対してpost-orderedな深さ優先探索を行う.
%%
%% `Graph'は`始点=>[終点のリスト]'という要素を持つ`map'で頂点間の接続関係が記述されている.
%%
%% 一度訪問した頂点は訪問対象から除外されるため,結果として行われるのは,
%% `RootVertex'から到達可能なサブグラフから得られるスパニングツリーに対する探索となる.
%%
%% post-orderedな探索であるため,葉から先に探索され,次に共通の先祖へと遡ってゆく.
%%
%% 探索された頂点に対しては順次`VertexFun'が適用される.
%% 兄弟関係にある頂点について適用された`VertexFun'の戻り値は`list'でコレクションされ,
%% 親の頂点に適用される`VertexFun'の第二引数へと渡される.
%%
%% 時間計算量: `O(|E|+|V|log|V|), |E|:枝の数, |V|:頂点の数'
%%
%% 空間計算量: `O(|V|), |V|:頂点の数'
%%
%% @param Graph      `map'形式で格納されたグラフ.
%% @param RootVertex 探索を開始する頂点.
%% @param VertexFun  訪れた頂点を処理する関数.
-spec dfs_post_order(graph(vertex()), vertex(), VertexFun) -> DFSRet when
      VertexFun      :: fun((vertex(), [VertexFunOK]) -> VertexFunRet),
      VertexFunRet   :: VertexFunOK | any_error(),
      VertexFunOK    :: term(),
      DFSRet         :: VertexFunRet | vertex_not_found().
dfs_post_order(Graph, RootVertex, VertexFun) ->
    case dfs_post_order(Graph, RootVertex, VertexFun, sets:add_element(RootVertex, sets:new())) of
        {error, Reason} ->
            {error, Reason};
        {Ret, _} ->
            Ret
    end.

%% @doc `map'で表現されたグラフに、存在しない頂点を追加する.
%%
%% `Graph'は`頂点=>[頂点のリスト]'という要素を持つ`map'で頂点間の接続関係が記述されている.
%%
%% 追加する頂点は,`Vertices'としてリストの形で渡す.
%%
%% `Graph'が既に持っている頂点は追加しない.
%%
%% 時間計算量: `O(|V|log|Vall|), |V|:追加しようとしている頂点の数, |Vall|:すべての頂点の数'
%%
%% 空間計算量: `O(|Vall|), |Vall|:すべての頂点の数'
%%
%% @param Graph    `map'形式で格納されたグラフ.
%% @param Vertices 追加したい頂点のリスト.
-spec graph_with_vertices(graph(vertex()), [vertex()]) -> graph(vertex()).
graph_with_vertices(Graph, Vertices) ->
    lists:foldl(fun (V, Acc) -> maps:put(V, [], Acc) end, Graph,
                lists:filter(fun (V) -> not maps:is_key(V, Graph) end, Vertices)).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------------------------------

%% @doc dfs_post_order/3の実体.
-spec dfs_post_order(graph(vertex()), vertex(), VertexFun, sets:set(vertex())) -> DFSRet when
      VertexFun       :: fun((vertex(), [VertexFunOK]) -> VertexFunRet),
      VertexFunRet    :: VertexFunOK | any_error(),
      VertexFunOK     :: term(),
      DFSRet          :: {VertexFunOK, sets:set(vertex())} | any_error().
dfs_post_order(Graph, RootVertex, VertexFun, VisitedVertices) ->
    case maps:find(RootVertex, Graph) of
        error ->
            {error, {vertex_not_found, RootVertex}};
        {ok, Children} ->
            FoldedFun = fun (V, Acc) ->
                {ElderSibsRet, VisitedVerticesBefore} = Acc,
                case sets:is_element(V, VisitedVerticesBefore) of
                    false ->
                        case dfs_post_order(Graph, V, VertexFun, sets:add_element(V, VisitedVerticesBefore)) of
                            {error, Reason} ->
                                {error, Reason};
                            {Ret, VisitedVerticesAfter} ->
                                {ok, {[Ret | ElderSibsRet], VisitedVerticesAfter}}
                        end;
                    true ->
                        {ok, Acc}
                end
            end,
            case moyo_list:maybe_foldl(FoldedFun, {[], VisitedVertices}, Children) of
                {error, Reason} ->
                    {error, Reason};
                {ok, {ConvertedChildren, VisitedVerticesRet}} ->
                    case VertexFun(RootVertex, lists:reverse(ConvertedChildren)) of
                        {error, Reason} ->
                            {error, Reason};
                        VertexFunRet ->
                            {VertexFunRet, VisitedVerticesRet}
                    end
            end
    end.
