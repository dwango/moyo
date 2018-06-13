%% coding: latin-1
%%
%% @copyright 2017 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_graph_tests).

-include_lib("moyo/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

dfs_post_order_test_() ->
    [
     {"単純な木構造をタプルに変換するテスト",
      fun () ->
              G = #{a => [b, c], b => [d], c => [], d => []},
              F = fun(V, Cs) -> {vertex, V, children, Cs} end,
              ?assertEqual({vertex, a, children, [{vertex, b, children, [{vertex, d, children, []}]},
                                                  {vertex, c, children, []}]},
                           moyo_graph:dfs_post_order(G, a, F)),
              ?assertEqual({vertex, b, children, [{vertex, d, children, []}]},
                           moyo_graph:dfs_post_order(G, b, F))
      end},
     {"ループ時停止性の確認",
      fun () ->
              G = #{a => [b, c], b => [d], c => [], d => [a, b]},
              F = fun(V, Cs) -> {vertex, V, children, Cs} end,
              ?assertEqual({vertex, a, children, [{vertex, b, children, [{vertex, d, children, []}]},
                                                  {vertex, c, children, []}]},
                            moyo_graph:dfs_post_order(G, a, F)),
              ?assertEqual({vertex, b, children, [{vertex, d, children,
                                                   [{vertex, a, children, [{vertex, c, children, []}]}]}]},
                           moyo_graph:dfs_post_order(G, b, F)),
              ?assertEqual({vertex, c, children, []}, moyo_graph:dfs_post_order(G, c, F)),
              ?assertEqual({vertex, d, children, [{vertex, a, children, [{vertex, b, children, []},
                                                                         {vertex, c, children, []}]}]},
                            moyo_graph:dfs_post_order(G, d, F))
      end},
     {"木ではない真性のDAGをタプルに変換するテスト",
      fun () ->
              G = #{a => [b, c], b => [d], c => [d], d => []},
              F = fun (V, Cs) -> {vertex, V, children, Cs} end,
              ?assertEqual({vertex, a, children, [{vertex, b, children, [{vertex, d, children, []}]},
                                                  {vertex, c, children, []}]},
                           moyo_graph:dfs_post_order(G, a, F))
      end},
     {"存在しない頂点を引いた時のテスト",
      fun () ->
              G = #{a => [b, c], b => [d], c => [d], d => [xxx]},
              F = fun (V, Cs) -> {vertex, V, children, Cs} end,
              ?assertEqual({error, {vertex_not_found, xxx}}, moyo_graph:dfs_post_order(G, a, F)),
              ?assertEqual({error, {vertex_not_found, yyy}}, moyo_graph:dfs_post_order(G, yyy, F))
      end},
     {"多重辺で重複した訪問が起らないことを確認するテスト",
      fun () ->
              G = #{a => [b, b, c], b => [d], c => [], d => []},
              F = fun (V, Cs) -> {vertex, V, children, Cs} end,
              ?assertEqual({vertex, a, children, [{vertex, b, children, [{vertex, d, children, []}]},
                                                  {vertex, c, children, []}]},
                           moyo_graph:dfs_post_order(G, a, F))
      end},
     {"VertexFunがエラーを返した時のテスト",
      fun () ->
              G = #{a => [b, c], b => [d], c => [e], d => [], e => []},
              F = fun (V, Cs) -> case V of c -> {error, {break, V, Cs}}; _ -> {vertex, V, children, Cs} end end,
              ?assertEqual({error, {break, c, [{vertex, e, children, []}]}}, moyo_graph:dfs_post_order(G, a, F))
      end}
    ].


graph_with_vertices_test_() ->
    [
     {"存在しない頂点が追加され、存在する頂点は変更されないことを確認するテスト",
      fun () ->
              ?assertEqual(#{a => [b, c], b => [c], c => [], d => []},
                           moyo_graph:graph_with_vertices(#{a => [b, c], b => [c], c => []}, [a, b, c, d]))
      end}
    ].
