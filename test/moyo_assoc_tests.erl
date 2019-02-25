%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_assocモジュールのユニットテスト
-module(moyo_assoc_tests).

-include_lib("eunit/include/eunit.hrl").

is_assoc_list_test_() ->
    [
     {"値が連想リスト形式かどうかを判定する",
      fun () ->
              ?assert(not moyo_assoc:is_assoc_list(1)),
              ?assert(not moyo_assoc:is_assoc_list([{1, 2, 3}])),
              ?assert(not moyo_assoc:is_assoc_list([{1}])),

              ?assert(moyo_assoc:is_assoc_list([])),
              ?assert(moyo_assoc:is_assoc_list([{1, 2}])),
              ?assert(moyo_assoc:is_assoc_list([{1, 2}, {a, b}]))
      end}
    ].

store_test_() ->
    [
     {"連想リストに新規要素を追加する",
      fun () ->
              AssocList0 = [{color, red}],
              AssocList1 = moyo_assoc:store(lang, erlang, AssocList0),

              ?assertEqual(erlang, moyo_assoc:fetch(lang, AssocList1))
      end},
     {"連想リストの要素を更新する",
      fun () ->
              AssocList0 = [{color, red},
                            {lang, erlang}],
              AssocList1 = moyo_assoc:store(lang, scala, AssocList0),

              ?assertEqual(scala, moyo_assoc:fetch(lang, AssocList1)),

              %% 古い要素は削除される
              ?assertEqual([color, lang],
                           lists:sort([K || {K, _} <- AssocList1]))
      end},
     {"重複したキーが存在する場合、要素の更新時に正規化は行われない",
      fun () ->
              AssocList0 = [{color, red},
                            {lang, ruby},
                            {lang, erlang}],
              AssocList1 = moyo_assoc:store(lang, scala, AssocList0),

              ?assertEqual(scala, moyo_assoc:fetch(lang, AssocList1)),

              %% もともと重複していたキーは、そのまま残る
              ?assertEqual([color, lang, lang],
                           lists:sort([K || {K, _} <- AssocList1]))
      end}
    ].

store_if_not_exist_test_() ->
    [
     {"キーが存在しない場合のみだけ要素を追加する",
      fun () ->
              AssocList0 = [],

              Result1 = moyo_assoc:store_if_not_exist(lang, scala, AssocList0),
              ?assertMatch({true, _}, Result1),
              {_, AssocList1} = Result1,
              ?assertEqual(scala, moyo_assoc:fetch(lang, AssocList1)),

              Result2 = moyo_assoc:store_if_not_exist(lang, erlang, AssocList1),
              ?assertMatch({false, _}, Result2),
              {_, AssocList2} = Result2,
              ?assertEqual(scala, moyo_assoc:fetch(lang, AssocList2)) % 既にキーが存在するので更新されていない
      end}
    ].

take_test_() ->
    [
     {"キーに紐づく要素を連想リストから取り出す",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}],
              ?assertEqual({ok, red, [{lang, ruby}]}, moyo_assoc:take(color, AssocList))
      end},
     {"対応するキーが存在しない場合は`error'が返る",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}],
              ?assertEqual(error, moyo_assoc:take(size, AssocList))
      end}
    ].

take3_test_() ->
    [
     {"キーに紐づく要素を連想リストから取り出す",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}],
              ?assertEqual({ok, red, [{lang, ruby}]}, moyo_assoc:take(color, AssocList, default))
      end},
     {"対応するキーが存在しない場合はリストはそのままで`default'が返る",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}],
              ?assertEqual({ok, default, AssocList}, moyo_assoc:take(size, AssocList, default))
      end}
    ].

delete_test_() ->
    [
     {"キーに紐づく要素を削除する",
      fun () ->
              AssocList0 = [{color, red}, {lang, ruby}],

              AssocList1 = moyo_assoc:delete(lang, AssocList0),
              ?assertEqual([{color, red}], AssocList1),

              AssocList2 = moyo_assoc:delete(lang, AssocList1),
              ?assertEqual([{color, red}], AssocList2) % 削除対象が存在しない場合は、入力リストがそのまま返る
      end}
    ].

update_test_() ->
    [
     {"連想リストの既存要素の値を更新する",
      fun () ->
              AssocList0 = [{color, red}],
              AssocList1 = moyo_assoc:update(color, fun (_Old) -> blue end, initial, AssocList0),

              ?assertEqual(blue, moyo_assoc:fetch(color, AssocList1))
      end},
     {"連想リストに存在しないキーに対して update関数 を呼び出した場合は、新たに要素が追加される",
      fun () ->
              AssocList0 = [{color, red}],
              AssocList1 = moyo_assoc:update(lang, fun (_Old) -> erlang end, scala, AssocList0),

              ?assertEqual(scala, moyo_assoc:fetch(lang, AssocList1))
      end},
     {"再帰的に連想リストの既存要素の値を更新する",
      fun () ->
              AssocList0 = [{apple, [{color, red}]}],
              AssocList1 = moyo_assoc:rupdate([apple, color], fun (_Old) -> blue end, initial, AssocList0),

              ?assertEqual(blue, moyo_assoc:rfetch([apple, color], AssocList1))
      end},
     {"連想リストに存在しないキーに対して rupdate関数 を呼び出した場合は、新たに要素が追加される",
      fun () ->
              AssocList0 = [{color, red}],
              AssocList1 = moyo_assoc:rupdate([program, lang], fun (_Old) -> erlang end, scala, AssocList0),

              ?assertEqual(scala, moyo_assoc:rfetch([program, lang], AssocList1))
      end},
     {"KeyListが空の場合、errorを出力する",
      fun () ->
              ?assertError(_, moyo_assoc:rupdate([], fun (X) -> X end, initial, []))
      end}
    ].

fetch_test_() ->
    [
     {"連想リストから値を取得する",
      fun () ->
              AssocList0 = [{color, red},
                            {lang, erlang}],

              ?assertEqual(red, moyo_assoc:fetch(color, AssocList0)),
              ?assertEqual(erlang, moyo_assoc:fetch(lang, AssocList0))
      end},
     {"fetch/2では、キーが存在しない場合、エラーが送出される",
      fun () ->
              AssocList0 = [{color, red}],

              ?assertError(_, moyo_assoc:fetch(lang, AssocList0))
      end},
     {"fetch/3では、キーが存在しない場合、デフォルト値が返される",
      fun () ->
              AssocList0 = [{color, red}],

              ?assertEqual(red, moyo_assoc:fetch(color, AssocList0, default_value)),
              ?assertEqual(default_value, moyo_assoc:fetch(lang, AssocList0, default_value))
      end},
     {"複数要素のキーが重複している場合は、最初に出現した要素の値が取得される",
      fun () ->
              AssocList0 = [{color, red},
                            {color, blue}],

              ?assertEqual(red, moyo_assoc:fetch(color, AssocList0))
      end},
     {"rfetch/2を使って、再帰的にfetchし値が返される",
      fun () ->
              AssocList0 = [{fruit, [{apple, [{color, red}]}, {banana, [{color, yellow}]}]},
                            {vegetable, [{radish, [{color, white}]}, {burdock, [{color, black}]}]}],

              ?assertEqual(yellow, moyo_assoc:rfetch([fruit, banana, color], AssocList0))
      end},
     {"rfetch/2では、キーが存在しない場合、エラーが送出される",
      fun () ->
              AssocList0 = [{fruit, [{apple, [{color, red}]}]},
                            {vegetable, [{radish, [{color, white}]}, {burdock, [{color, black}]}]}],

              ?assertError(_, moyo_assoc:rfetch([fruit, banana, color], AssocList0))
      end},
     {"rfetch/3では、キーが存在しない場合、デフォルト値が返される",
      fun () ->
              AssocList0 = [{fruit, [{apple, [{color, red}]}, {banana, [{color, yellow}]}]},
                            {vegetable, [{radish, [{color, white}]}, {burdock, [{color, black}]}]}],

              ?assertEqual(yellow, moyo_assoc:rfetch([fruit, banana, color], AssocList0, default_value)),
              ?assertEqual(default_value,
                           moyo_assoc:rfetch([fruit, banana, size], AssocList0, default_value))
      end}
    ].

fetch_as_test_() ->
    [
     {"バリデーション付きで要素の値を取得する",
      fun () ->
              AssocList = [
                           {timestamp, 10},
                           {wrong_timestamp, <<"10">>} % 不正な値(非整数)
                          ],
              Spec = {{integer, [non_negative]}, []},

              ?assertEqual(10, moyo_assoc:fetch_as(timestamp, Spec, AssocList)),
              ?assertError({invalid_value, _}, moyo_assoc:fetch_as(wrong_timestamp, Spec, AssocList))
      end}
    ].

fetch_values_test_() ->
    [
     {"複数値を一度に取得する",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}],
              ?assertEqual([ruby, red], moyo_assoc:fetch_values([lang, color], AssocList))
      end},
     {"一つでも値の取得に失敗した場合は、処理全体が失敗する",
      fun () ->
              AssocList = [{color, red}],
              ?assertError(_, moyo_assoc:fetch_values([lang, color], AssocList)) % 'lang'は存在しない
      end}
    ].

lookup_test_() ->
    [
     {"連想リストから値を取得する",
      fun () ->
              AssocList0 = [{color, red},
                            {lang, erlang}],

              ?assertEqual({ok, red}, moyo_assoc:lookup(color, AssocList0)),
              ?assertEqual({ok, erlang}, moyo_assoc:lookup(lang, AssocList0))
      end},
     {"キーが存在しない場合は none が返される",
      fun () ->
              AssocList0 = [{color, red}],

              ?assertEqual(error, moyo_assoc:lookup(lang, AssocList0))
      end},
     {"複数要素のキーが重複している場合は、最初に出現した要素の値が取得される",
      fun () ->
              AssocList0 = [{color, red},
                            {color, blue}],

              ?assertEqual({ok, red}, moyo_assoc:lookup(color, AssocList0))
      end}
    ].

lookup_as_test_() ->
    [
     {"バリデーション付きで要素を検索する",
      fun () ->
              AssocList = [
                           {timestamp, 10},
                           {wrong_timestamp, <<"10">>} % 不正な値(非整数)
                          ],
              Spec = {{integer, [non_negative]}, []},

              ?assertEqual({ok, 10}, moyo_assoc:lookup_as(timestamp, Spec, AssocList)),
              ?assertMatch({error, _}, moyo_assoc:lookup_as(wrong_timestamp, Spec, AssocList)),
              ?assertMatch({error, _}, moyo_assoc:lookup_as(unknown_entry, Spec, AssocList)) % 存在しない要素
      end}
    ].

lookup_values_test_() ->
    [
     {"複数要素を一度に検索する",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}],
              ?assertEqual({ok, [ruby, red]}, moyo_assoc:lookup_values([lang, color], AssocList))
      end},
     {"一つでも要素の取得に失敗した場合は {error, Reason} が返される",
      fun () ->
              AssocList = [{color, red}],
              ?assertMatch({error, _}, moyo_assoc:lookup_values([lang, color], AssocList)) % 'lang'は存在しない
      end}
    ].

lookup_values_as_test_() ->
    [
     {"バリデーション付きで複数要素を一度に検索する",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}, {timestamp, 20}],
              Spec = [
                      {timestamp, {integer, [non_negative]}, []},
                      {color, {enum, [red, blue, black, white]}, []}
                     ],
              ?assertEqual({ok, [20, red]}, moyo_assoc:lookup_values_as(Spec, AssocList))
      end},
     {"指定を満たさない要素が一つでもある場合は {error, Reason} が返される",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}, {timestamp, 20}],
              Spec = [
                      {timestamp, {integer, [non_negative]}, []},
                      {color, {enum, [blue, black, white]}, []} % 'color'に'red'は含まれない
                     ],
              ?assertMatch({error, _}, moyo_assoc:lookup_values_as(Spec, AssocList))
      end}
    ].

lookup_entries_as_test_() ->
    [
     {"バリデーション付きで複数要素を一度に取得する",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}, {timestamp, 20}],
              Spec = [
                      {timestamp, {integer, [non_negative]}, []},
                      {color, {enum, [red, blue, black, white]}, []}
                     ],
              ?assertEqual({ok, [{timestamp, 20}, {color, red}]}, moyo_assoc:lookup_entries_as(Spec, AssocList))
      end},
     {"指定を満たさない要素が一つでもある場合は {error, Reason} が返される",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}, {timestamp, 20}],
              Spec = [
                      {timestamp, {integer, [non_negative]}, []},
                      {color, {enum, [blue, black, white]}, []}, % 'color'に'red'は含まれない
                      {unknown_entry, integer, []}  % 'unknown_entry'は存在しない
                     ],
              ?assertMatch({error, _}, moyo_assoc:lookup_entries_as(Spec, AssocList))
      end},
     {"取得後のキー名を変更することが可能",
      fun () ->
              AssocList = [{color, red}, {lang, ruby}, {timestamp, 20}],
              Spec = [
                      %% キー部分を {From, To} 形式にすることで、取得後の要素のキー名を変更可能
                      {{color, color_type}, {enum, [red, blue, black, white]}, []}
                     ],
              ?assertEqual({ok, [{color_type, red}]}, moyo_assoc:lookup_entries_as(Spec, AssocList))
      end},
     {"要素が存在しない場合に、デフォルト値を指定することが可能",
      fun () ->
              AssocList = [{lang, ruby}, {timestamp, 20}],
              Spec = [
                      {color, {enum, [red, blue, black, white]}, [{default, blue}]}
                     ],
              ?assertEqual({ok, [{color, blue}]}, moyo_assoc:lookup_entries_as(Spec, AssocList))
      end},
     {"要素が存在しない場合に、その要素をスキップすることが可能",
      fun () ->
              AssocList = [{lang, ruby}, {timestamp, 20}],
              Spec = [
                      {color, {enum, [red, blue, black, white]}, [optional]}
                     ],
              ?assertEqual({ok, []}, moyo_assoc:lookup_entries_as(Spec, AssocList))
      end}
    ].

push_test_() ->
    [
     {"空リストにpushする",
      fun () ->
              AssocList0 = [],
              AssocList1 = moyo_assoc:push(lang, erlang, AssocList0),

              ?assertEqual([erlang], moyo_assoc:fetch(lang, AssocList1))
      end},

     {"既に存在する要素に値を追加する (値は先頭に追加される)",
      fun () ->
              AssocList0 = [{lang, [erlang]}],
              AssocList1 = moyo_assoc:push(lang, scala, AssocList0),

              ?assertEqual([scala, erlang], moyo_assoc:fetch(lang, AssocList1))
      end},

     {"複数要素が場合のpush",
      fun () ->
              AssocList0 = [{lang,  [erlang]},
                            {color, [red, blue]}],
              AssocList1 = moyo_assoc:push(lang, scala, AssocList0),
              AssocList2 = moyo_assoc:push(color, yellow, AssocList1),

              ?assertEqual([scala, erlang], moyo_assoc:fetch(lang, AssocList2)),
              ?assertEqual([yellow, red, blue], moyo_assoc:fetch(color, AssocList2))
      end},

     {"既に存在する要素の値がリストではない場合はエラーが送出される",
      fun () ->
              AssocList0 = [{lang, erlang}],

              ?assertError(_, moyo_assoc:push(lang, scala, AssocList0))
      end}
    ].

pop_test_() ->
    [
     {"空リストからpopする",
      fun () ->
              AssocList0 = [],

              ?assertMatch({empty, _}, moyo_assoc:pop(lang, AssocList0))
      end},

     {"キーは存在するが、値が空リストの要素からpopする",
      fun () ->
              AssocList0 = [{lang, []}],

              ?assertMatch({empty, _}, moyo_assoc:pop(lang, AssocList0))
      end},

     {"キーと値リストの両方が存在する要素からpopする",
      fun () ->
              AssocList0 = [{lang, [erlang]}],

              {Result0, AssocList1} = moyo_assoc:pop(lang, AssocList0),
              ?assertMatch({value, erlang}, Result0),

              ?assertMatch({empty, _}, moyo_assoc:pop(lang, AssocList1))
      end},

     {"値リストに複数の値が入っている場合は、先頭から順番に取り出される",
      fun () ->
              AssocList0 = [{lang, [scala, erlang]}],

              {Result0, AssocList1} = moyo_assoc:pop(lang, AssocList0),
              ?assertMatch({value, scala}, Result0),

              {Result1, AssocList2} = moyo_assoc:pop(lang, AssocList1),
              ?assertMatch({value, erlang}, Result1),

              ?assertMatch({empty, _}, moyo_assoc:pop(lang, AssocList2))
      end},

     {"要素の値がリストではない場合は、エラーが送出される",
      fun () ->
              AssocList0 = [{lang, [scala | erlang]}],

              %% 最初の値取り出しはOK
              {Result0, AssocList1} = moyo_assoc:pop(lang, AssocList0),
              ?assertMatch({value, scala}, Result0),

              %% 次の値取り出しはエラー
              ?assertError(_, moyo_assoc:pop(lang, AssocList1))
      end}
    ].

-record(test_record,  % from_record_test_/0 用
        {
         a :: any(),
         b :: any(),
         c :: any()
        }).
from_record_test_() ->
    [
     {"レコードを連想リストに変換する",
      fun () ->
              Record   = #test_record{a = 10, b = 20, c = 30},
              Expected = [
                          {a, 10},
                          {b, 20},
                          {c, 30}
                         ],
              ?assertEqual(Expected, moyo_assoc:from_record(record_info(fields, test_record), Record))
      end}
    ].

to_record_test_() ->
    [
     {"連想リストをレコードに変換する",
      fun () ->
              AssocList = [{a, 10}, {b, 20}, {c, 30}],
              Expected  = #test_record{
                            a = 10,
                            b = 20,
                            c = 30
                          },
              ?assertEqual(Expected, moyo_assoc:to_record(test_record, record_info(fields, test_record), AssocList))
      end},
     {"不足しているフィールドがある場合は、レコードに変換できない",
      fun () ->
              AssocList = [{a, 10}, {c, 30}],
              ?assertError(_, moyo_assoc:to_record(test_record, record_info(fields, test_record), AssocList))
      end}
    ].

to_record_as_test_()->
    [
     {"バリデーション付きで連想リストをレコードに変換する",
      fun () ->
              AssocList = [{a, 10}, {b, 20}, {c, 30}],
              Spec = [
                      {a, integer, []},
                      {b, integer, []},
                      {c, integer, []}
                     ],
              Expected  = #test_record{
                            a = 10,
                            b = 20,
                            c = 30
                          },
              ?assertEqual({ok, Expected}, moyo_assoc:to_record_as(test_record, record_info(fields, test_record), Spec, AssocList))
      end},
     {"バリデーションに失敗した場合は {error, Reason} が返る",
      fun () ->
              AssocList = [{a, 10}, {b, 20}, {c, 30}],
              Spec = [
                      {a, integer, []},
                      {b, integer, []},
                      {c, {integer, [{range, 100, 200}]}, []} % 範囲指定あり
                     ],
              ?assertMatch({error, _}, moyo_assoc:to_record_as(test_record, record_info(fields, test_record), Spec, AssocList))
      end}
    ].

to_map_test_()->
    [
     {"連想リストをmapに変換する",
      fun ()->
              AssocList = [{a, 10}, {b, 20}, {c, 30}],
              Expected = #{
                           a => 10,
                           b => 20,
                           c => 30
                         },
              ?assertEqual(Expected, moyo_assoc:to_map(AssocList))
      end
     },
     {"連想リストのキーが重複している場合，先にあるものが優先される",
      fun ()->
              AssocList = [{a, 10}, {c, 31}, {b, 20}, {c, 30}],
              Expected = #{
                           a => 10,
                           b => 20,
                           c => 31
                         },
              ?assertEqual(Expected, moyo_assoc:to_map(AssocList))
      end
     }
    ].

from_map_test_()->
    [
     {"mapを連想リストに変換する",
      fun ()->
              Map = #{
                      a => 10,
                      b => 20,
                      c => 30
                     },
              Expected = [{a, 10}, {b, 20}, {c, 30}],
              Result = moyo_assoc:from_map(Map),
              ?assertEqual(lists:sort(Expected), lists:sort(Result))
      end
     }
    ].

equal_test_() ->
    [
     {"連想リストが等しい場合, trueが返る",
      fun () ->
              AssocList1 = [{key1, value1}, {key2, value2}, {key3, value3}],
              AssocList2 = [{key2, value2}, {key1, value1}, {key3, value3}],

              ?assert(moyo_assoc:equal(AssocList1, AssocList2))
      end},

    {"片方に同じキーの要素が複数ある場合, 前の値を比較し等しい場合はtrueを返す",
     fun () ->
             AssocList1 = [{key1, value1}, {key2, value2}, {key1, duplicated}],
             AssocList2 = [{key1, value1}, {key2, value2}],

             ?assert(moyo_assoc:equal(AssocList1, AssocList2))
     end},

    {"連想リスト内の要素が同じ並びの場合もtrueを返す",
     fun () ->
             AssocList1 = [{key1, value1}, {key2, value2}, {key3, value3}],
             AssocList2 = [{key1, value1}, {key2, value2}, {key3, value3}],

             ?assert(moyo_assoc:equal(AssocList1, AssocList2))
     end},

    {"空の連想リストを比較した時はtrueが返る",
     fun () ->
             AssocList1 = [],
             AssocList1 = [],

             ?assert(moyo_assoc:equal(AssocList1, AssocList1))
     end},

    {"2段以上の連想リストでも比較できる",
     fun () ->
             AssocList1 = [{key1, value1}, {assoc_list, [{key2_1, value2_1}, {key2_2, value2_2}]}],
             AssocList2 = [{assoc_list, [{key2_2, value2_2}, {key2_1, value2_1}]}, {key1, value1}],

             ?assert(moyo_assoc:equal(AssocList1, AssocList2))
     end},

    {"2段目の連想リストで誤っていた場合はfalseを返す",
     fun () ->
             AssocList1 = [{key1, value1}, {assoc_list, [{key2_1, value2_1}, {key2_2, value2_2}]}],
             AssocList2 = [{assoc_list, [{key2_2, value2_2}, {key_error, value_error}]}, {key1, value1}],

             ?assert(not moyo_assoc:equal(AssocList1, AssocList2))
     end},

    {"2段目のリストで片方にしかないリストがあった場合はfalseを返す",
     fun () ->
             AssocList1 = [{key1, value1}, {assoc_list, [item1, item2]}],
             AssocList2 = [{assoc_list, [item1]}, {key1, value1}],

             ?assert(not moyo_assoc:equal(AssocList1, AssocList2))
     end}
    ].

diff_test_()->
    [
     {"正しく分類できているかのテスト",
      fun () ->
              AssocList1 = [{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}, {key7, value7}, {key1, value2}],
              AssocList2 = [{key1, value2}, {key2, value2}, {key3, value3}, {key4, value4}, {key6, value6}],
              Result = moyo_assoc:diff(AssocList1,AssocList2),
              ?assert(Result=:={[{key2, value2}, {key3, value3}, {key4, value4}],[{key1, {value1, value2}}],[{key5, value5}, {key7, value7}],[{key6, value6}]})
      end},
     {"AssocList2が空の時の挙動を見るテスト",
      fun () ->
              AssocList1 = [{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}, {key7, value7}, {key1, value2}],
              AssocList2 = [],
              Result = moyo_assoc:diff(AssocList1,AssocList2),
              ?assert(Result=:={[],[],[{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}, {key7, value7}],[]})
      end},
     {"AssocList1が空の時の挙動を見るテスト",
      fun () ->
              AssocList1 = [],
              AssocList2 = [{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}, {key7, value7}, {key1, value2}],
              Result = moyo_assoc:diff(AssocList1,AssocList2),
              ?assert(Result=:={[],[],[],[{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}, {key7, value7}]})
      end},
     {"AssocList1,2ともに空の時の挙動を見るテスト",
      fun () ->
              AssocList1 = [],
              AssocList2 = [],
              Result = moyo_assoc:diff(AssocList1,AssocList2),
              ?assert(Result=:={[],[],[],[]})
      end},
     {"keyにおける1と1.0を区別できているかのテスト",
      fun () ->
              AssocList1 = [{1.0, 2}],
              AssocList2 = [{1, 10},{1.0, 4}],
              Result = moyo_assoc:diff(AssocList1,AssocList2),
              ?assert(Result=:={[], [{1.0, {2,4}}], [], [{1, 10}]})
      end},
     {"valueにおける2と2.0を区別できているかのテスト",
      fun () ->
              AssocList1 = [{1, 2.0}],
              AssocList2 = [{1, 2}],
              Result = moyo_assoc:diff(AssocList1,AssocList2),
              ?assert(Result=:={[], [{1, {2.0,2}}], [], []})
      end}
    ].

merge_test_()->

    [
     {"=:=ではなく==で一致確認ができているか、AssocList1のkey値が優先されているか、同じリストに複数のkey値がある場合、最初のほうが優先されているか",
      fun() ->
              AssocList1 = [{key1, value11}, {key3, value31}, {1, value01}, {key3, value312}],
              AssocList2 = [{key1, value12}, {key2, value22}, {1.0, value02}],
              Result = moyo_assoc:merge(AssocList1, AssocList2),
              Expected = [{1, value01}, {key1, value11}, {key2, value22}, {key3, value31}],
              ?assertEqual(Expected, Result)
      end},
     {"AssocList1もAssocList2も空の時の挙動",
      fun() ->
              AssocList1 = [],
              AssocList2 = [],
              Result = moyo_assoc:merge(AssocList1, AssocList2),
              Expected = [],
              ?assertEqual(Expected, Result)
      end},
     {"AssocList1が空の時の挙動",
      fun() ->
              AssocList1 = [],
              AssocList2 = [{key1, value1}, {key2, value2}, {key3, value3}],
              Result = moyo_assoc:merge(AssocList1, AssocList2),
              Expected = AssocList2,
              ?assertEqual(Expected, Result)
      end},
     {"AssocList2が空の時の挙動",
      fun() ->
              AssocList1 = [{key1, value1}, {key2, value2}, {key3, value3}],
              AssocList2 = [],
              Result = moyo_assoc:merge(AssocList1, AssocList2),
              Expected = AssocList1,
              ?assertEqual(Expected, Result)
      end},
     {"AssocList1=:=AssocList2の時の挙動",
      fun() ->
              AssocList1 = [{key1, value1}, {key2, value2}, {key3, value3}],
              AssocList2 = [{key1, value1}, {key2, value2}, {key3, value3}],
              Result = moyo_assoc:merge(AssocList1, AssocList2),
              Expected = AssocList1,
              ?assertEqual(Expected, Result)
      end}
    ].

intersection_and_differences_test_() ->
    [
     {"適切にタプルを分けることができる",
      fun () ->
              AList1 = [{key2, value2}, {key1, value1},                                                 {key4, value4}],
              AList2 = [{key3, value3}, {key1, value1}, {key5, value5}, {key2, value4}, {key2, value2}, {key4, value4}],

              Expected = {[{key1,value1},{key4,value4}],
                          [{key2,value2}],
                          [{key2,value4},{key3,value3},{key5,value5}]},
              ?assertEqual(Expected, moyo_assoc:intersection_and_differences(AList1, AList2))
      end},

     {"第1引数のkeyの方が小さい場合",
      ?_assertEqual({[], [{1, value}], [{2, value}]},
                    moyo_assoc:intersection_and_differences([{1, value}], [{2, value}]))},

     {"第2引数のkeyの方が小さい場合",
      ?_assertEqual({[], [{2, value}], [{1, value}]},
                    moyo_assoc:intersection_and_differences([{2, value}], [{1, value}]))},

     {"第1引数が空リストの場合",
      ?_assertEqual({[], [], [{key, value}]}, moyo_assoc:intersection_and_differences([], [{key, value}]))},

     {"第2引数が空リストの場合",
      ?_assertEqual({[], [{key, value}], []}, moyo_assoc:intersection_and_differences([{key, value}], []))}
    ].

unique_by_key_test_() ->
    [
        {"空の連想リストのときは空のリストが生成される",
            fun () ->
                AssocList = [],
                Expected = [],
                ?assertEqual(Expected, moyo_assoc:unique_by_key(AssocList))
            end},
        {"重複がない場合は同一のリストが返却される",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}],
                ?assertEqual(AssocList, moyo_assoc:unique_by_key(AssocList))
            end},
        {"キーが重複する場合、先に現れた値が残り、後の値は除去される",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}, {key1, value4}],
                Expected = [{key1, value1}, {key2, value2}, {key3, value3}],
                ?assertEqual(Expected, moyo_assoc:unique_by_key(AssocList))
            end}
    ].

keys_test_() ->
    [
        {"キーを過不足なく取り出すことができる",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}],
                Expected = [key1, key2, key3],
                ?assertEqual(Expected, moyo_assoc:keys(AssocList))
            end},
        {"空の連想リストのときは空のリストが生成される",
            fun () ->
                AssocList = [],
                Expected = [],
                ?assertEqual(Expected, moyo_assoc:keys(AssocList))
            end},
        {"キーが重複する場合、生成後のリストにも重複したキーが残る",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}, {key1, value4}],
                Expected = [key1, key2, key3, key1],
                ?assertEqual(Expected, moyo_assoc:keys(AssocList))
            end},
        {"キーが重複する場合、生成後のリストにも重複したキーが残らないようにする",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}, {key1, value4}],
                Expected = [key1, key2, key3],
                ?assertEqual(Expected, moyo_assoc:keys(moyo_assoc:unique_by_key(AssocList)))
            end}
    ].

keys_as_set_test_() ->
    [
        {"gb_sets:set()を生成することができる",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key1, value3}],
                KeySet = moyo_assoc:keys_as_set(AssocList),
                ?assert(gb_sets:is_element(key1, KeySet)),
                ?assert(gb_sets:is_element(key2, KeySet)),
                ?assertNot(gb_sets:is_element(key3, KeySet))
            end}
    ].

values_test_() ->
    [
        {"値を過不足なく取り出すことができる",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}],
                Expected = [value1, value2, value3],
                ?assertEqual(Expected, moyo_assoc:values(AssocList))
            end},
        {"空の連想リストのときは空のリストが生成される",
            fun () ->
                AssocList = [],
                Expected = [],
                ?assertEqual(Expected, moyo_assoc:values(AssocList))
            end},
        {"キーが重複する場合、生成後のリストにも重複したキーの値が残る",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}, {key1, value4}],
                Expected = [value1, value2, value3, value4],
                ?assertEqual(Expected, moyo_assoc:values(AssocList))
            end},
        {"キーが重複する場合、生成後のリストにも重複したキーの値が残らないようにする",
            fun () ->
                AssocList = [{key1, value1}, {key2, value2}, {key3, value3}, {key1, value4}],
                Expected = [value1, value2, value3],
                ?assertEqual(Expected, moyo_assoc:values(moyo_assoc:unique_by_key(AssocList)))
            end}
    ].

from_map_recur_test_() ->
    [
        {"mapからassocListに再起的に変換できる",
            fun() ->
                NestAL = lists:foldl(
                    fun(_, Res) -> [{0, Res}] end, 0, lists:seq(1, 10)
                ),
                NestMap = lists:foldl(
                    fun(_, Res) -> #{0=>Res} end, 0, lists:seq(1, 10)
                ),
                ?assertEqual(NestAL, moyo_assoc:from_map_recur(NestMap)),
                ?assert(moyo_assoc:equal(
                    [{#{}, []}, {[], []}, {a, a}],
                    moyo_assoc:from_map_recur(#{#{}=>#{}, []=>#{}, a=>a})
                ))
            end}
    ].

to_map_recur_test_() ->
    [
        {"assocListからmapに再起的に変換できる",
            fun() ->
                NestAL = lists:foldl(
                    fun(_, Res) -> [{0, Res}] end, 0, lists:seq(1, 10)
                ),
                NestMap = lists:foldl(
                    fun(_, Res) -> #{0=>Res} end, 0, lists:seq(1, 10)
                ),
                ?assertEqual(NestMap, moyo_assoc:to_map_recur(NestAL)),
                ?assertEqual(
                    #{#{}=>#{}, []=>#{}, a=>a}
                    , moyo_assoc:to_map_recur([{#{}, []}, {[], []}, {a, a}]))
            end},
        {"assocListでないものを変換しない",
            fun() ->
                Check = fun(A) -> ?assertEqual(#{0=>A}, moyo_assoc:to_map_recur([{0, A}])) end,
                Check(#{}),
                Check([{1, 2}, 3]),
                Check([0, {1, 2}])
            end},
        {"Keyが同じなら前の値で変換される",
            fun() ->
                ?assertEqual(#{1=>#{1=>2}}, moyo_assoc:to_map_recur([{1, [{1, 2}, {1, 3}]}, {1, 3}]))
            end}
    ].
