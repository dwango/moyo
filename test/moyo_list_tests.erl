%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
-module(moyo_list_tests).

-include_lib("eunit/include/eunit.hrl").

find_if_test_() ->
    [
     {"述語関数の適用結果がtrueとなる最初の要素を検索する",
      fun () ->
              PredFun = fun (N) -> (N rem 2) =:= 0 end, % 偶数を探す

              ?assertEqual(error, moyo_list:find_if(PredFun, [1, 3, 5])), % 奇数しかない
              ?assertEqual({ok, 2}, moyo_list:find_if(PredFun, [1, 2, 3, 4]))
      end}
    ].

delete_all_test_() ->
    [
     {"一致する全ての要素がリストから削除される",
      ?_assertEqual([aaa, ccc, aaa], moyo_list:delete_all(bbb, [aaa, bbb, ccc, bbb, aaa]))
     },
     {"一致する要素が存在しない場合は、入力リストがそのまま返される",
      ?_assertEqual([aaa, bbb, ccc], moyo_list:delete_all(ddd, [aaa, bbb, ccc]))
     }
    ].

take_test_() ->
    [
     {"最初に一致する要素が削除されたリストが返される",
      ?_assertEqual({ok, [aaa, ccc, bbb]}, moyo_list:take(bbb, [aaa, bbb, ccc, bbb]))
     },
     {"一致する要素がない場合は'error'が返される",
      ?_assertEqual(error, moyo_list:take(bbb, [111, 222, 333]))
     }
    ].

take_if_test_() ->
    [
     {"述語関数の適用結果がtrueとなる最初の要素を検索し, その値とその値を除いたリストを返す",
      fun () ->
              PredFun = fun (N) -> (N rem 2) =:= 0 end, % 偶数を探す

              ?assertEqual(error, moyo_list:take_if(PredFun, [1, 3, 5])), % 奇数しかない
              ?assertEqual({ok, 2, [1, 3, 4]}, moyo_list:take_if(PredFun, [1, 2, 3, 4]))
      end}
    ].

replace_if_test_() ->
    [
     {"述語関数の適用結果が`true'を返す要素は置換される",
      fun () ->
              Fun = fun (X) -> X =:= 2 end,
              Input    = [1, 2, 3],
              Expected = [1, hoge, 3],
              ?assertEqual(Expected, moyo_list:replace_if(Fun, hoge, Input))
      end},
     {"述語関数の適用結果が`true'となる要素が存在しない場合は、入力リストがそのまま返される",
      fun () ->
              Fun = fun (X) -> X =:= 2 end,
              Input = [a, b, c],
              ?assertEqual(Input, moyo_list:replace_if(Fun, hoge, Input))
      end}
    ].

position_test_() ->
    [
     {"要素がリスト内の何番目に位置しているかを取得できる",
      fun () ->
              Input    = [a, b, c],
              Expected = {ok, 2},
              ?assertEqual(Expected, moyo_list:position(b, Input))
      end},
     {"指定された要素がリスト内に存在しない場合は`error'が返される",
      fun () ->
              Input    = [a, b, c],
              Expected = error,
              ?assertEqual(Expected, moyo_list:position(hoge, Input))
      end}
    ].

shuffle_test_() ->
    [
     {"リストをシャッフルする",
      fun () ->
              rand:seed(exs64, {0, 0, 0}),  % テストの度に結果が変わらないように乱数生成のシードは固定しておく
              List = [1, 2, 3, 4, 5],
              ?assert(List =/= moyo_list:shuffle(List))
      end}
    ].

foldl_while_test_() ->
    [
     {"左畳み込みを途中で中断する",
      fun () ->
              List    = [1, 2, a, 3],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {true, Acc * X + X};
                            (X,   _)                   -> {false, {not_a_number, X}}
                        end,

              ?assertEqual({not_a_number, a}, moyo_list:foldl_while(Fun, Initial, List))
      end},
     {"左畳み込みを最後まで行う",
      fun () ->
              List    = [1, 2, 3],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {true, Acc * X + X};
                            (X,   _)                   -> {false, {not_a_number, X}}
                        end,

              ?assertEqual(21, moyo_list:foldl_while(Fun, Initial, List))
      end}
    ].

foldr_while_test_() ->
    [
     {"右畳み込みを途中で中断する",
      fun () ->
              List    = [1, 2, a, 3],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {true, Acc * X + X};
                            (X,   _)                   -> {false, {not_a_number, X}}
                        end,

              ?assertEqual({not_a_number, a}, moyo_list:foldr_while(Fun, Initial, List))
      end},
     {"右畳み込みを最後まで行う",
      fun () ->
              List    = [1, 2, 3],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {true, Acc * X + X};
                            (X,   _)                   -> {false, {not_a_number, X}}
                        end,

              ?assertEqual(15, moyo_list:foldr_while(Fun, Initial, List))
      end}
    ].

maybe_map_foldl_test_() ->
    [
     {"左畳み込みの成功時",
      fun () ->
              List    = [1, 2, 3],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {ok, Acc * X + X};
                            (X,   _)                   -> {error, {not_a_number, X}}
                        end,
              Result  = 21,

              ?assertEqual({ok, Result}, moyo_list:maybe_foldl(Fun, Initial, List))
      end},
     {"左畳み込みの失敗時",
      fun () ->
              List    = [1, a, b],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {ok, Acc * X + X};
                            (X,   _)                   -> {error, {not_a_number, X}}
                        end,

              ?assertEqual({error, {not_a_number, a}}, moyo_list:maybe_foldl(Fun, Initial, List))
      end}
    ].

maybe_map_foldr_test_() ->
    [
     {"右畳み込みの成功時",
      fun () ->
              List    = [1, 2, 3],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {ok, Acc * X + X};
                            (X,   _)                   -> {error, {not_a_number, X}}
                        end,
              Result  = 15,

              ?assertEqual({ok, Result}, moyo_list:maybe_foldr(Fun, Initial, List))
      end},
     {"右畳み込みの失敗時",
      fun () ->
              List    = [1, a, b],
              Initial = 1,
              Fun     = fun (X, Acc) when is_number(X) -> {ok, Acc * X + X};
                            (X,   _)                   -> {error, {not_a_number, X}}
                        end,

              ?assertEqual({error, {not_a_number, b}}, moyo_list:maybe_foldr(Fun, Initial, List))
      end}
    ].

maybe_map_test_() ->
    [
     {"リストの要素をマッピングする",
      fun () ->
              List   = [1, 2, 3],
              Fun    = fun (X) -> {ok, X * 2} end,
              Result = [2, 4, 6],

              ?assertEqual({ok, Result}, moyo_list:maybe_map(Fun, List))
      end},
     {"マッピングに失敗した場合",
      fun () ->
              List = [1, a, 3],
              Fun  = fun (X) when is_number(X) -> {ok, X * 2};
                         (X)                   -> {error, {not_a_number, X}}
                     end,

              ?assertMatch({error, {not_a_number, _}}, moyo_list:maybe_map(Fun, List))
      end}
    ].

maybe_foreach_test_() ->
    [
     {"要素の走査成功時",
      fun () ->
              List = [1, 2, 3],
              Fun  = fun (X) when is_number(X) -> X;
                         (X)                   -> {error, {not_a_number, X}}
                     end,

              ?assertMatch(ok, moyo_list:maybe_foreach(Fun, List))
      end},
     {"要素の走査失敗時",
      fun () ->
              List = [1, a, 3],
              Fun  = fun (X) when is_number(X) -> X;
                         (X)                   -> {error, {not_a_number, X}}
                     end,

              ?assertEqual({error, {not_a_number, a}}, moyo_list:maybe_foreach(Fun, List))
      end}
    ].

maybe_pmap_test_() ->
    [
     {"空のリストの要素をマッピングする",
      fun () ->
              List   = [],
              Fun    = fun (X) -> {ok, X * 2} end,
              Result = [],

              ?assertEqual({ok, Result}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"リストの要素をマッピングする",
      fun () ->
              List   = [1, 2, 3, 4, 5, 6],
              Fun    = fun (X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2} end,
              Result = [2, 4, 6, 8, 10, 12],

              ?assertEqual({ok, Result}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングに失敗した場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (X)                   -> timer:sleep(rand:uniform(4) * 100), {error, {not_a_number, X}}
                     end,

              ?assertMatch({error, {not_a_number, a}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングプロセスがerrorになった場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (X)                   -> timer:sleep(rand:uniform(4) * 100), error({not_a_number, X}) % error
                     end,

              ?assertMatch({error, {'EXIT', error, {not_a_number, a}, _}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングプロセスが死んだ場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (X)                   -> timer:sleep(rand:uniform(4) * 100), exit({not_a_number, X}) % exit
                     end,

              ?assertMatch({error, {'EXIT', exit, {not_a_number, a}, _}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングプロセスがexit(normal)で死んだ場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (_)                   -> timer:sleep(rand:uniform(4) * 100), exit(normal) % exit
                     end,

              ?assertMatch({error, {'EXIT', exit, normal, _}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングプロセスがexit(self(), normal)で死んだ場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (_)                   -> timer:sleep(rand:uniform(4) * 100), exit(self(), normal) % exit
                     end,

              ?assertMatch({error, {'EXIT', true}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングプロセスがexit(kill)で死んだ場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (_)                   -> timer:sleep(rand:uniform(4) * 100), exit(kill) % exit
                     end,

              ?assertMatch({error, {'EXIT', exit, kill, _}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングプロセスがexit(self(), kill)で死んだ場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (_)                   -> timer:sleep(rand:uniform(4) * 100), exit(self(), kill) % exit
                     end,

              ?assertMatch({error, {'EXIT', killed}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"マッピングプロセスがthrowを投げた場合",
      fun () ->
              List = [1, 2, 3, a, 5, 6],
              Fun  = fun (X) when is_number(X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2};
                         (X)                   -> timer:sleep(rand:uniform(4) * 100), throw({not_a_number, X}) % throw
                     end,

              ?assertMatch({error, {'EXIT', throw, {not_a_number, a}, _}}, moyo_list:maybe_pmap(Fun, List))
      end},
     {"タイムアウトした場合",
      fun () ->
              List = [1, 2, 3, 4, 5, 6],
              Fun  = fun (X) -> timer:sleep(rand:uniform(X) * 100), {ok, X * 2} end,

              ?assertMatch({error, {'EXIT', timeout}}, moyo_list:maybe_pmap(Fun, List, 0))
      end}
    ].

longest_common_prefix_test_() ->
    [
     {"二つのリストのLongestCommonPrefixの長さを返す",
      fun () ->
              ?assertEqual(6, moyo_list:longest_common_prefix(["erlang", "erlang"])),   % 完全一致
              ?assertEqual(2, moyo_list:longest_common_prefix(["erlang", "ergonomy"])), % 部分(先頭)一致
              ?assertEqual(0, moyo_list:longest_common_prefix(["erlang", "perl"])),     % 一致なし
              ?assertEqual(0, moyo_list:longest_common_prefix(["",       "perl"])),     % 最初のリストが空
              ?assertEqual(0, moyo_list:longest_common_prefix(["erlang", ""]))          % 二番目のリストが空
      end},
     {"要素の型はstring()に限らず任意のリストを渡すことが可能",
      fun () ->
              ?assertEqual(2, moyo_list:longest_common_prefix([ [[1,2,3],3,4], [[1,2,3],3,5] ]))
      end},
     {"リストが一つの場合は、常にそのリストの長さが返される",
      fun () ->
              Input = "erlang",
              ?assertEqual(length(Input), moyo_list:longest_common_prefix([Input]))
      end},
     {"リストが３つの場合",
      fun () ->
              ?assertEqual(1, moyo_list:longest_common_prefix(["erlang", "ergonomy", "eunit"]))
      end},
     {"要素数が0の場合はbadargエラーとなる",
      fun () ->
              ?assertError(badarg, moyo_list:longest_common_prefix([]))
      end},
     {"要素にリスト以外を含む場合はbadargエラーとなる",
      fun () ->
              ?assertError(badarg, moyo_list:longest_common_prefix(["erlang", erlang]))
      end}
    ].

split_longest_common_prefix_test_() ->
    [
     {"二つのリストをLongestCommonPrefixの位置で分割する",
      fun () ->
              ?assertEqual({"erlang", ["", ""]},       moyo_list:split_longest_common_prefix(["erlang", "erlang"])),   % 完全一致
              ?assertEqual({"er", ["lang", "gonomy"]}, moyo_list:split_longest_common_prefix(["erlang", "ergonomy"])), % 部分(先頭)一致
              ?assertEqual({"", ["erlang", "perl"]},   moyo_list:split_longest_common_prefix(["erlang", "perl"])),     % 一致なし
              ?assertEqual({"", ["", "perl"]},         moyo_list:split_longest_common_prefix(["",       "perl"])),     % 最初のリストが空
              ?assertEqual({"", ["erlang", ""]},       moyo_list:split_longest_common_prefix(["erlang", ""]))          % 二番目のリストが空
      end},
     {"要素の型はstring()に限らず任意のリストを渡すことが可能",
      fun () ->
              ?assertEqual({[[1,2,3],3], [[4], [5]]}, moyo_list:split_longest_common_prefix([ [[1,2,3],3,4], [[1,2,3],3,5] ]))
      end},
     {"リストが一つの場合",
      fun () ->
              ?assertEqual({"erlang", [""]}, moyo_list:split_longest_common_prefix(["erlang"]))
      end},
     {"リストが３つの場合",
      fun () ->
              ?assertEqual({"e", ["rlang", "rgonomy", "unit"]}, moyo_list:split_longest_common_prefix(["erlang", "ergonomy", "eunit"]))
      end},
     {"要素数が0の場合はbadargエラーとなる",
      fun () ->
              ?assertError(badarg, moyo_list:split_longest_common_prefix([]))
      end},
     {"要素にリスト以外を含む場合はbadargエラーとなる",
      fun () ->
              ?assertError(badarg, moyo_list:split_longest_common_prefix(["erlang", erlang]))
      end}
    ].

inits_test_() ->
    [
     {"引数のリスト内の全ての先頭部分リストを長さの増加する順に並べて返す",
      fun () ->
              ?assertEqual(["", "a", "ab", "abc"],    moyo_list:inits("abc")),  % 文字列
              ?assertEqual([[], [a], [a,b], [a,b,c]], moyo_list:inits([a,b,c])) % 文字列以外
      end},
     {"空リストの場合",
      ?_assertEqual([[]], moyo_list:inits([]))
     }
    ].

tails_test_() ->
    [
     {"引数のリスト内の全ての末尾部分リストを長さの減少する順に並べて返す",
      fun () ->
              ?assertEqual(["abc", "bc", "c", ""],    moyo_list:tails("abc")),  % 文字列
              ?assertEqual([[a,b,c], [b,c], [c], []], moyo_list:tails([a,b,c])) % 文字列以外
      end},
     {"空リストの場合",
      ?_assertEqual([[]], moyo_list:tails([]))
     }
    ].

uniq_test_() ->
    [
     {"リスト内の重複する要素を削除する",
      ?_assertEqual([a, b, c], moyo_list:uniq([a, b, a, c, c, b]))
     },
     {"入力リストの出現順が保存される",
      ?_assertEqual([c, a, b], moyo_list:uniq([c, a, c, b, b, a]))
     },
     {"'1.0'と'1'は、別要素として扱われる('=:='基準での一致判定)",
      ?_assertEqual([1, 1.0], moyo_list:uniq([1, 1.0]))
     },
     {"空リストの場合",
      ?_assertEqual([], moyo_list:uniq([]))
     }
    ].

adjacent_uniq_test_() ->
    [
     {"リスト内の連接する重複要素を削除する",
      ?_assertEqual([a, b, c], moyo_list:adjacent_uniq([a, a, b, b, c, c]))
     },
     {"入力リストがソートされていない場合",
      ?_assertEqual([a, b, a], moyo_list:adjacent_uniq([a, a, b, b, a, a])) % 連接していない重複要素は削除されない
     },
     {"'1.0'と'1'は、別要素として扱われる('=:='基準での一致判定)",
      ?_assertEqual([1, 1.0], moyo_list:adjacent_uniq([1, 1.0]))
     },
     {"空リストの場合",
      ?_assertEqual([], moyo_list:adjacent_uniq([]))
     }
    ].

group_by_test_() ->
    [
     {"Keyを１番目の要素の文字とした時",
      fun () ->
              ?assertEqual([{b, [{b, 456}]},
                            {a, [{a, 789}, {a, 123}]}],
                           moyo_list:group_by(1, [{a, 123}, {b, 456}, {a, 789}]))
      end},

     {"Keyを2番目の要素の数値とした時",
      fun () ->
              ?assertEqual([{789, [{a, 789}]},
                            {456, [{b, 456}]},
                            {123, [{a, 123}]}],
                           moyo_list:group_by(2, [{a, 123}, {b, 456}, {a, 789}]))
      end},

     {"Keyが文字となる時で、１つのKeyに対応したタプルが複数ある場合",
      fun () ->
              ?assertEqual([
                            {d, [{d, 400}]},
                            {c, [{c, 700}, {c, 500}, {c, 300}]},
                            {b, [{b, 200}]},
                            {a, [{a, 600}, {a, 100}]}
                            ],
                           moyo_list:group_by(1, [  {a, 100}, {b, 200}, {c, 300}, {d, 400},
                                                    {c, 500}, {a, 600}, {c, 700}]))
      end},

     {"Keyが数値となる時で、１つのKeyに対応したタプルが複数ある場合",
      fun () ->
              ?assertEqual([
                            {300, [{a, 300}, {c, 300}, {b, 300}]},
                            {200, [{c, 200}]},
                            {100, [{d, 100}, {c, 100}, {a, 100}]}
                            ],
                           moyo_list:group_by(2, [  {a, 100}, {b, 300}, {c, 100}, {d, 100},
                                                    {c, 300}, {a, 300}, {c, 200}]))
      end},

     {"同じ値のタプルが複数ある場合",
      fun () ->
              ?assertEqual([
                            {b, [{b, 2}, {b, 3}, {b, 2}, {b, 1}, {b, 2}]},
                            {a, [{a, 1}, {a, 2}, {a, 3}, {a, 1}]}
                            ],
                           moyo_list:group_by(1, [  {a, 1}, {b, 2}, {a, 3}, {b, 1},
                                                    {b, 2}, {a, 2}, {a, 1}, {b, 3}, {b, 2}]))
      end}
    ].
