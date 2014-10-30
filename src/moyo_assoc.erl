%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 連想リストに関する処理を集めたユーティリティモジュール.
%%
%% 連想リストとは、以下のようにキーと値が対となったタプルを要素として保持するリストのことを表す。
%% ```
%% [
%%   {key, value},
%%   {"キー", "値"},
%%   {editors, [<<"vim">>, <<"emacs">>, <<"notepad">>]}
%% ]
%% '''
%%
%% === 【重複キーの扱いについて】 ===
%% 重複したキーを持つ要素を複数リスト内に保持することは可能だが、<br />
%% その場合、連想リストの操作時には、同じキーを有する要素群の内の最初のもの以外は無視される。<br />
%%
%% 例えば、検索関数の場合は、最初に出現した要素の値が採用され、<br />
%% 削除関数では、最初に出現した要素のみが削除されることになる。<br />
%% (つまり、一回の削除関数呼び出しで、重複キーを持つ全ての要素が除去されることはない)
%%
%% ただし、明示的に重複キーの扱い方が定義されている関数に関しては、その限りではない。
%%
%%
%% === 【要素の並び順に関して】 ===
%% 本モジュールは連想リストをセットの一種として扱うため、原則として要素の順番は考慮されない。
%%
%% そのため、ある関数を適用した結果、連想リストの論理的な内容は同一でも、<br />
%% 実際の内容(要素の並び順)は、適用前とは変わっていることもあるので、注意が必要。 <br />
%% ※ ただし、例外として連想リストが重複キーを含む場合は、それらの間の順番(前後関係)は常に維持される。
-module(moyo_assoc).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         is_assoc_list/1,

         store/3,
         store_if_not_exist/3,
         delete/2,
         take/2,
         update/4,
         rupdate/4,

         fetch/2, fetch/3,
         fetch_as/3,
         fetch_values/2,
         rfetch/2, rfetch/3,

         lookup/2,
         lookup_as/3,
         lookup_entries/2,
         lookup_entries_as/2,
         lookup_values/2,
         lookup_values_as/2,

         push/3,
         pop/2,

         from_record/2,
         to_record/3,
         to_record_as/4,

         equal/2,
         intersection_and_differences/2,
         diff/2,
         merge/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Types
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              assoc_list/0, assoc_list/2,
              key/0,
              value/0,

              validate_option_ext/0,
              validate_entry_spec/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type assoc_list(Key, Value) :: [{Key, Value}].
-type assoc_list()           :: assoc_list(key(), value()).

-type key()   :: term().
-type value() :: term().

-type validate_entry_spec() :: {KeySpec::(key() | {key(), key()}), ValueSpec::(moyo_validator:spec()), Options::([moyo_validator:option() | validate_option_ext()])}.
%% 要素のバリデーション指定. <br />
%%
%% [KeySpec] <br />
%% 対象要素のキー名を指定する. <br />
%% `{From, To}'形式で指定した場合は、検索は`From'で行われ、結果としては`To'がキー名として使用される. (キー名のリネーム)<br />
%%
%% [ValueSpec] <br />
%% 値のバリデーション方法を指定する. <br />
%% 詳細は`moyo_validator:spec/0'を参照のこと. <br />
%%
%% [Options] <br />
%% バリデーションオプションを指定する. <br />
%% 詳細は`moyo_validator:option/0'および'moyo_assoc:validate_option_ext/0'を参照のこと. <br />
%%

-type validate_option_ext() :: {default, DefaultValue::term()} | optional.
%% `moyo_assoc'独自のバリデートオプション: <br />
%% - {default, DefaultValue}: 指定された場合は、要素が存在しない場合に、エラーではなく`DefaultValue'を返す <br />
%% - optional: 指定された場合は、要素が存在しない場合に、エラーではなく単に結果から除外される <br />
%%

%%----------------------------------------------------------------------------------------------------------------------
%% Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc 引数の値が連想リストかどうかを判定する.
-spec is_assoc_list(Value::term()) -> boolean().
is_assoc_list(Value) when is_list(Value) ->
    lists:all(fun ({_, _}) -> true;
                  (_)      -> false
              end,
              Value);
is_assoc_list(_) -> false.

%% @doc 連想リストに要素を追加する.
%%
%% 追加しようとしている要素のキーが既に存在している場合は、その要素が取り除かれた上で、新しい要素が追加される。
-spec store(key(), value(), assoc_list()) -> assoc_list().
store(Key, Value, AssocList) ->
    case lists:keytake(Key, 1, AssocList) of
        false                     -> [{Key, Value} | AssocList];
        {value, _Old, AssocList2} -> [{Key, Value} | AssocList2]
    end.

%% @doc 既にキーが存在しない場合にのみ、連想リストに要素を追加する.
-spec store_if_not_exist(key(), value(), assoc_list()) -> {Stored::boolean(), assoc_list()}.
store_if_not_exist(Key, Value, AssocList) ->
    case lists:keymember(Key, 1, AssocList) of
        true  -> {false, AssocList};
        false -> {true, [{Key, Value} | AssocList]}
    end.

%% @doc キーに対応する要素を連想リストから取り出す(取り除く)
-spec take(key(), assoc_list()) -> error | {ok, value(), assoc_list()}.
take(Key, AssocList) ->
    case lists:keytake(Key, 1, AssocList) of
        false                           -> error;
        {value, {_, Value}, AssocList2} -> {ok, Value, AssocList2}
    end.

%% @doc キーに対応する要素を削除する.
-spec delete(key(), assoc_list()) -> assoc_list().
delete(Key, AssocList) ->
    lists:keydelete(Key, 1, AssocList).

%% @doc キーに対応する要素の値を更新する.
%%
%% キーに対応する要素が存在しない場合は`Initial'を値とする要素が新たに追加される.
-spec update(key(), UpdateFun, Initial, assoc_list()) -> assoc_list() when
      UpdateFun :: fun ((OldValue::value()) -> NewValue::value()),
      Initial   :: value().
update(Key, UpdateFun, Initial, AssocList) ->
    case lists:keytake(Key, 1, AssocList) of
        false                         -> [{Key, Initial}        | AssocList];
        {value, {_, Old}, AssocList2} -> [{Key, UpdateFun(Old)} | AssocList2]
    end.

%% @doc キーリストに対応する要素の値を更新する.
%%
%% キー(リスト)に対応する要素が存在しない場合は`Initial'を値とする要素が新たに追加される.<br />
%% キーリストの初めの方でタプルがなかった場合でも追加される.<br />
%% 以下、キーがない場合の例:
%% ```
%% > rupdate([banana, color], fun(X) -> X end, yellow, []).
%% [{banana, [{color, yellow}]}]
%% '''
-spec rupdate(KeyList::[key()], UpdateFun, Initial, assoc_list()) -> assoc_list() when
      UpdateFun :: fun ((OldValue::value()) -> NewValue::value()),
      Initial   :: value().
rupdate([Key], UpdateFun, Initial, AssocList) ->
    update(Key, UpdateFun, Initial, AssocList);
rupdate([Key | KeyList], UpdateFun, Initial, AssocList) ->
    case lists:keytake(Key, 1, AssocList) of
        false                               ->
            [{Key, rupdate(KeyList, UpdateFun, Initial, [])} | AssocList];
        {value, {Key, ListValue}, AssocList2} ->
            [{Key, rupdate(KeyList, UpdateFun, Initial, ListValue)} | AssocList2]
    end.

%% @doc キーに対応する値を取得する.
%%
%% キーが存在しない場合は、例外が送出される.
-spec fetch(key(), assoc_list()) -> value().
fetch(Key, AssocList) ->
    case lookup(Key, AssocList) of
        error       -> error({no_such_key, Key});
        {ok, Value} -> Value
    end.

%% @doc キーに対応する値を取得する.
%%
%% キーが存在しない場合は、デフォルト値が代わりに返される.
-spec fetch(key(), assoc_list(), value()) -> value().
fetch(Key, AssocList, DefaultValue) ->
    case lookup(Key, AssocList) of
        error       -> DefaultValue;
        {ok, Value} -> Value
    end.

%% @doc キーに対応する値を`ValueSpec'で指定された方式で取得する
%%
%% `ValuesSpec'の詳細は{@link moyo_validator:validate/3}を参照のこと
-spec fetch_as(Key, ValueSpec, assoc_list()) -> value() when
      Key       :: key(),
      ValueSpec :: {moyo_validator:spec(), [moyo_validator:option()]}.
fetch_as(Key, {ValidateSpec, ValidateOptions}, AssocList) ->
    Value = fetch(Key, AssocList),
    case moyo_validator:validate(Value, ValidateSpec, ValidateOptions) of
        {error, Reason} -> error({invalid_value, [{key, Key}, {reason, Reason}]});
        {ok, Value2}    -> Value2
    end.

%% @doc 複数の値を一度に取得する
-spec fetch_values(KeyList::[key()], assoc_list()) -> [value()].
fetch_values(KeyList, AssocList) ->
    [moyo_assoc:fetch(Key, AssocList) || Key <- KeyList].

%% @doc 再帰的にキーに対応する値を取得する.
%%
%% `KeyList'の先頭から順番に対応する値を取得し、その値に対して`fetch'を適用する<br />
%% キーが存在しない場合は、例外が送出される.
%% 以下、使用例:
%% ```
%% > rfetch([key1_1, key2_1, key3_1],
%% >        [{key1_1, [{key2_1, [{key3_1, value3_1}, {key3_2, value3_2}]}, {key2_2, value2_2}]}, {key1_2, value1_2}]
%% >       ).
%% value3_1
%% '''
-spec rfetch(KeyList::[key()], assoc_list()) -> value().
rfetch([Key | KeyList], AssocList) ->
    Value = fetch(Key, AssocList),
    rfetch(KeyList, Value);
rfetch([], AssocList) ->
    AssocList.

%% @doc 再帰的にキーに対応する値を取得する.
%%
%% `KeyList'の先頭から順番に対応する値を取得し、その値に対して`fetch'を適用する<br />
%% キーが存在しない場合は、デフォルト値が代わりに返される.
%% 以下、使用例:
%% ```
%% > rfetch([key1_1, key2_1, key3_1, key4_1],
%% >        [{key1_1, [{key2_1, [{key3_1, value3_1}, {key3_2, value3_2}]}, {key2_2, value2_2}]}, {key1_2, value1_2}],
%% >        default_value
%% >       ).
%% default_value
%% '''
-spec rfetch(KeyList::[key()], assoc_list(), value()) -> value().
rfetch([Key | KeyList], AssocList, DefaultValue) ->
    case lookup(Key, AssocList) of
        error       -> DefaultValue;
        {ok, Value} -> rfetch(KeyList, Value, DefaultValue)
    end;
rfetch([], AssocList, _) ->
    AssocList.

%% @doc キーに対応する値を検索する.
-spec lookup(key(), assoc_list()) -> error | {ok, value()}.
lookup(Key, AssocList) ->
    case lists:keyfind(Key, 1, AssocList) of
        false      -> error;
        {_, Value} -> {ok, Value}
    end.

%% @doc キーに対応する値を`ValueSpec'で指定された方式で取得する
%%
%% `EntrySpec'の詳細は{@link moyo_validator:validate/3}を参照のこと
-spec lookup_as(Key, EntrySpec, assoc_list()) -> {ok, value()} | {error, Reason} when
      Key       :: key(),
      EntrySpec :: {moyo_validator:spec(), [moyo_validator:option()]},
      Reason    :: not_found | term().
lookup_as(Key, {ValidateSpec, ValidateOptions}, AssocList) ->
    case lookup(Key, AssocList) of
        error       -> {error, not_found};
        {ok, Value} -> case moyo_validator:validate(Value, ValidateSpec, ValidateOptions) of
                           {error, Reason} -> {error, Reason};
                           {ok, Value2}    -> {ok, Value2}
                       end
    end.

%% @doc `KeyList'で指定されたキーに対応する値一覧を取得する
-spec lookup_values(KeyList, assoc_list()) -> {ok, [value()]} | {error, Reason::term()} when
      KeyList :: [key()].
lookup_values(KeyList, AssocList) ->
    case lookup_entries(KeyList, AssocList) of
        {error, Reason}  -> {error, Reason};
        {ok, AssocList2} -> {ok, [Value || {_, Value} <- AssocList2]}
    end.

%% @doc `EntrySpecList'で指定された方式で、値一覧を取得する
%%
%% 以下のコードとほぼ透過:
%% ```
%% > {ok, Entries} = lookup_entries_as(EntrySpecList, AssocList).
%% > {ok, [V || {_, V} <- Entries]}.
%% '''
-spec lookup_values_as(EntrySpecList, assoc_list()) -> {ok, [value()]} | {error, Reason::term()} when
      EntrySpecList :: [{key(), moyo_validator:spec(), [moyo_validator:option() | validate_option_ext()]}].
lookup_values_as(EntrySpecList, AssocList) ->
    case lookup_entries_as(EntrySpecList, AssocList) of
        {error, Reason}  -> {error, Reason};
        {ok, AssocList2} -> {ok, [Value || {_, Value} <- AssocList2]}
    end.

%% @doc `KeyList'で指定されたエントリー(要素)一覧を取得する
-spec lookup_entries(KeyList, assoc_list()) -> {ok, assoc_list()} | {error, Reason} when
      KeyList :: [key()],
      Reason  :: term().
lookup_entries(KeyList, AssocList) ->
    LookupFun = fun (Key, AssocList2) ->
                        case lookup(Key, AssocList2) of
                            error       -> {error, {Key, not_found}};
                            {ok, Value} -> {ok, Value}
                        end
                end,
    lookup_entries_impl(KeyList, AssocList, LookupFun).

%% @doc `EntrySpecList'で指定された方式で、エントリー(要素)一覧を取得する
%%
%% 指定方法の詳細に関しては`validate_entry_spec()'のドキュメント及び`moyo_validator'モジュールを参照のこと. <br />
%%
%% 使用例:
%% ```
%% > AssocList = [{key1, 10}, {key2, abc}].
%% 　
%% %% 基本的な使用方法
%% > lookup_entries_as([{key1, integer, []},
%%                      {key2, atom, []}],
%%                     AssocList).
%% {ok, [{key1, 10}, {key2, abc}]}
%% 　
%% %% キー名を変更する
%% > lookup_entries_as([{{key1, new_key1}, integer, []}], AssocList).
%% {ok, [{new_key1, 10}]}
%% 　
%% %% デフォルト値を指定する
%% > lookup_entries_as([{key3, integer, [{default, 30}]}], AssocList).
%% {ok, [{key3, 30}]}
%% '''
-spec lookup_entries_as(EntrySpecList, assoc_list()) -> {ok, assoc_list()} | {error, Reason} when
      EntrySpecList :: [validate_entry_spec()],
      Reason        :: term().
lookup_entries_as(EntrySpecList, AssocList) ->
    LookupFun = fun ({KeySpec, ValidateSpec, ValidateOptions}, AssocList2) ->
                        {FromKey, ToKey} = case KeySpec of
                                               {From, To} -> {From, To};
                                               Key        -> {Key, Key}
                                           end,
                        {Default, IsOptional, ValidateOptions2} = parse_validate_option_ext(ValidateOptions),
                        case lookup_as(FromKey, {ValidateSpec, ValidateOptions2}, AssocList2) of
                            {ok, Value}        -> {ok, ToKey, Value};
                            {error, not_found} -> case {Default, IsOptional} of
                                                      {{true, Value}, _} -> {ok, ToKey, Value};
                                                      {false, false}     -> {error, {FromKey, not_found}};
                                                      {false, true}      -> ignore
                                                  end;
                            {error, Reason}    -> {error, {FromKey, Reason}}
                        end
                end,
    lookup_entries_impl(EntrySpecList, AssocList, LookupFun).

%% @doc キーに対応するリストの先頭に値を追加する.
%%
%% キーが存在しない場合は、追加する値のみを含むリストが新規に生成される.<br />
%% キーに対応する値が存在し、かつリスト以外の場合は、例外が送出される.
-spec push(key(), value(), assoc_list()) -> assoc_list().
push(Key, Value, AssocList) ->
    case lists:keytake(Key, 1, AssocList) of
        false                                             -> [{Key, [Value]}        | AssocList];
        {value, {_, List}, AssocList2} when is_list(List) -> [{Key, [Value | List]} | AssocList2]
    end.

%% @doc キーに対応するリストの先頭から値を取り出す.
%%
%% キーが存在しない場合は、要素の値が空リストとして扱われる (つまり結果として empty が返される).<br />
%% キーに対応する値が存在し、かつリスト以外の場合は、例外が送出される.
-spec pop(key(), assoc_list()) -> {Result, assoc_list()} when
      Result :: {value, value()} | empty.
pop(Key, AssocList) ->
    case lists:keytake(Key, 1, AssocList) of
        false                                    -> {empty, AssocList};
        {value, {_, []},             AssocList2} -> {empty, AssocList2};
        {value, {_, [Value | Rest]}, AssocList2} -> {{value, Value}, [{Key, Rest} | AssocList2]}
    end.

%% @doc レコードを連想リスト形式に変換する.
%%
%% `Fields' の値は `record_info(fields, RecordName)' で取得できる. <br />
%% 以下、使用例:
%% ```
%% > rd(sample, {a, b, c}).
%% > Record = #sample{a = 10, b = 20, c = 30}.
%% #sample{a = 10,b = 20,c = 30}
%%
%% > Fields = record_info(fields, sample).
%% [a,b,c]
%%
%% > moyo_assoc:from_record(Fields, Record).
%% [{a,10},{b,20},{c,30}]
%% '''
-spec from_record([atom()], Record) -> assoc_list() when
      Record :: tuple(). % 汎用的なrecordを表すための型表記がないので、内部表現であるtuple()で代替している。(本来内部表現を記述するのは好ましくない)
from_record(Fields, Record) ->
    lists:zip(Fields, tl(tuple_to_list(Record))).

%% @doc 連想リストからレコードを生成する.
%%
%% `Fields' の値は `record_info(fields, RecordName)' で取得できる. <br />
%% 以下、使用例:
%% ```
%% > rd(sample, {a, b, c}).
%% > Params = [{a, 10}, {b, 20}, {c, 30}].
%% > moyo_assoc:to_record(sample, record_info(fields, sample), Params).
%% #sample{a = 10,b = 20,c = 30}
%% '''
-spec to_record(RecordName, Fields, Params) -> Record when
      RecordName :: atom(),
      Fields     :: [atom()],
      Params     :: assoc_list(),
      Record     :: tuple().
to_record(RecordName, Fields, Params) ->
    %% NOTE: レコードの内部表現を仮定した変換。本来はあまりやるべきではない。
    list_to_tuple([RecordName | [moyo_assoc:fetch(Field, Params) || Field <- Fields]]).

%% @doc 連想リストからレコードを生成する.
%%
%% 連想リストから要素を取得する際には`moyo_validator'を使用して、値の検証および変換が行われる. <br />
%% 以下のコードとほぼ等価:
%% ```
%% > {ok, Entries} = lookup_entries_as(FieldSpecList, Params).
%% > to_record(RecordName, Fields, Entries).
%% '''
-spec to_record_as(RecordName, Fields, FieldSpecList, Params) -> {ok, assoc_list()} | {error, Reason} when
      RecordName    :: atom(),
      Fields        :: [atom()],
      FieldSpecList :: [validate_entry_spec()],
      Params        :: assoc_list(),
      Reason        :: term().
to_record_as(RecordName, Fields, FieldSpecList, Params) ->
    case moyo_assoc:lookup_entries_as(FieldSpecList, Params) of
        {error, Reason} -> {error, Reason};
        {ok, Entries}   -> {ok, to_record(RecordName, Fields, Entries)}
    end.

%% @doc 2つの連想リストが同じかどうかを比較する.
%%
%% 同じ場合は, true, 異なる場合はfalse.
%% ただし, 重複キーや順の扱いは他の連想リストと同じ扱いである.
%%
%% ex:
%% ```
%% 1> moyo_assoc:equal([{key1, value1}, {key2, value2}, {key1, different_value}], [{key2, value2}, {key1, value1}]).
%% true
%% '''
-spec equal(AssocList1, AssocList2) -> boolean() when
      AssocList1 :: assoc_list(),
      AssocList2 :: assoc_list().
equal(AssocList1, AssocList2) ->
    SortedList1 = lists:ukeysort(1, AssocList1),
    SortedList2 = lists:ukeysort(1, AssocList2),
    equal_impl(SortedList1, SortedList2).

%% @doc 2つの連想リストから共通のタプルとどちらかのリストにしかないタプルを分ける.
%%
%% 出力は, {共通のタプル, リスト1にだけあるタプル, リスト2にだけあるタプル}.
%%
%% ex:
%% ```
%% 1> moyo_assoc:intersection_and_differences
%% 1> ([{key2, value2}, {key1, value1}, {key4, value4}],
%% 1> [{key3, value3}, {key1, value1}, {key5, value5}, {key2, value4}, {key2, value2}, {key4, value4}]).
%% {[{key1,value1},{key4,value4}],
%%  [{key2,value2}],
%%  [{key2,value4},{key3,value3},{key5,value5}]}
%% '''
-spec intersection_and_differences(AssocList1, AssocList2) -> {Intersec, Diff1, Diff2} when
      AssocList1 :: assoc_list(),
      AssocList2 :: assoc_list(),
      Intersec   :: assoc_list(),
      Diff1      :: assoc_list(),
      Diff2      :: assoc_list().
intersection_and_differences(AssocList1, AssocList2) ->
    SortedList1 = lists:ukeysort(1, AssocList1),
    SortedList2 = lists:ukeysort(1, AssocList2),
    intersection_and_differences_impl(SortedList1, SortedList2, [], [], []).

%% @doc 2つの連想リストから共通のタプルとどちらかのリストにしかないタプルとキーがList1とLIst2で異なるタプルを分ける
%%
%% 出力は, {共通のタプル, キーが同じでvalueが変更されたタプル, リスト1にだけあるタプル, リスト2にだけあるタプル}.
%%
%%
%% 最初に2つのリストの中身をdictに展開し、他方のリストの探索をしやすいようにする。<br />
%% gb_treesをdictの代わりに使わないのは、gb_treesでは1と1.0が同一視される(=:=ではなく==での比較が行われているため)が、1.0と1は違うものと扱いたいから。<br />
%% それらを元に、重複する要素を消したリストを生成する。<br />
%% そのあと、EqualList,ValueDiffList,Only1List,Only2Listをリスト内包表記でフィルタリングしながら生成する。<br />
%% key,valueともに値の比較は=:=で行っているため、1と1.0は別物として扱われる点に注意すること。<br />
%%
%% ex:
%% ```
%% 1> moyo_assoc:diff
%% 1> ([{key1, value1}, {key2, value2}, {key3, value3}, {key4, value4}, {key5, value5}, {key7, value7}, {key1, value2} ],
%% 1> [{key1, value2}, {key2, value2}, {key3, value3}, {key4, value4}, {key6, value6}]).
%% {[{key2, value2}, {key3, value3}, {key4, value4}],
%%  [{key1, {value1, value2}}],
%%  [{key5, value5}, {key7, value7}],
%%  [{key6, value6}]}
%% '''
-spec diff(AssocList1, AssocList2) -> {EqualList, ValueDiffList, Only1List, Only2List} when
      AssocList1    :: assoc_list(),
      AssocList2    :: assoc_list(),
      EqualList     :: assoc_list(),
      ValueDiffList   :: assoc_list(Key::term(), {Before::term(), After::term()}),
      Only1List   :: assoc_list(),
      Only2List  :: assoc_list().
diff(List1, List2) ->
    Dict1 = lists:foldl(fun({Key,Value},D) -> dict:store(Key,Value,D) end, dict:new(), lists:reverse(List1)),
    Dict2 = lists:foldl(fun({Key,Value},D) -> dict:store(Key,Value,D) end, dict:new(), lists:reverse(List2)),
    UniqueList1 = dict:to_list(Dict1),
    UniqueList2 = dict:to_list(Dict2),
    EqualList = [{Key,Value} || {Key,Value} <- UniqueList1, dict:is_key(Key,Dict2) andalso dict:fetch(Key,Dict2) =:= Value],
    UpdateList = [{Key, {Value, dict:fetch(Key,Dict2)}} || {Key, Value} <- UniqueList1, dict:is_key(Key, Dict2) andalso dict:fetch(Key, Dict2) =/= Value],
    Only1List = [{Key, Value} || {Key, Value} <- UniqueList1, dict:is_key(Key,Dict2) =:= false],
    Only2List = [{Key, Value} || {Key, Value} <- UniqueList2, dict:is_key(Key,Dict1) =:= false],
    {EqualList, UpdateList, Only1List, Only2List}.


%% @doc 2つの連想リストをマージする.
%%
%% 2つの連想リストにおいて、片方のリストにしかkeyが存在しないものは、そのまま結果のリストに加える。両方のリストに同じkeyがある場合、List1の方のkey、valueペアを結果のリストに加える。この関数において、keyの同値判定は=:=ではなく==で行っている。
%%
%% 出力は, {演算した結果の連想リスト}
%%
%% ２つのリストを++で連結したあと、ukeysortで重複するkeyは最初のもののみ考慮するようにするという実装方法で実現している。
%%
%% ex:
%% ```
%% 1> moyo_assoc:merge
%% 1> ([{key1, value11}, {key3, value31}, {1, value01}, {key3, value312}], [{key1, value12}, {key2, value22}, {1.0, value02}]).
%% [{key1, value11}, {key3, value31}, {1, value01}, {key2, value22}]
%% '''
-spec merge(AssocList1 :: assoc_list(), AssocList2 :: assoc_list()) ->  Result::assoc_list().
merge(AssocList1, AssocList2) ->
    lists:ukeysort(1, AssocList1 ++ AssocList2).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec lookup_entries_impl([Key], AssocList, LookupFun) -> {ok, assoc_list()} | {error, Reason} when
      Key       :: term(),
      AssocList :: assoc_list(),
      LookupFun :: fun ((Key, AssocList) -> {ok, value()} | {ok, NewKey::key(), value()} | {error, Reason} | ignore),
      Reason    :: term().
lookup_entries_impl(KeyList, AssocList, LookupFun) ->
    {Oks, Errors} =
        lists:foldr(fun (Key, {AccOks, AccErrors}) ->
                            case LookupFun(Key, AssocList) of
                                {error, Reason}     -> {AccOks, [Reason  | AccErrors]};
                                {ok, NewKey, Value} -> {[{NewKey, Value} | AccOks], AccErrors};
                                {ok, Value}         -> {[{Key, Value}    | AccOks], AccErrors};
                                ignore              -> {AccOks, AccErrors}
                            end
                    end,
                    {[], []},
                    KeyList),
    case Errors of
        [] -> {ok, Oks};
        _  -> {error, Errors}
    end.

-spec parse_validate_option_ext([moyo_validator:option() | validate_option_ext()]) -> Result when
      Result     :: {Default, IsOptional, [moyo_validator:option()]},
      Default    :: {true, DefaultValue::term()} | false,
      IsOptional :: boolean().
parse_validate_option_ext(Options) ->
    {IsOptional, Options2} = {lists:member(optional, Options), lists:delete(optional, Options)},
    {Default, Options4} =
        case lists:keytake(default, 1, Options2) of
            false                                      -> {false, Options2};
            {value, {default, DefaultValue}, Options3} -> {{true, DefaultValue}, Options3}
        end,
    {Default, IsOptional, Options4}.

-spec equal_impl(SortedAssocList1, SortedAssocList2) -> boolean() when
      SortedAssocList1 :: assoc_list(),
      SortedAssocList2 :: assoc_list().
equal_impl([], []) -> true;
equal_impl([{Key, AssocList1} = Tuple1 | RestList1], [{Key, AssocList2} = Tuple2 | RestList2]) ->
    case is_assoc_list(AssocList1) andalso is_assoc_list(AssocList2) of
        true                         -> equal(AssocList1, AssocList2) andalso equal_impl(RestList1, RestList2);
        false when Tuple1 =:= Tuple2 -> equal_impl(RestList1, RestList2);
        false                        -> false
    end;
equal_impl(_, _) -> false.

-spec intersection_and_differences_impl(AssocList1, AssocList2, IntersecTmp, Diff1Tmp, Diff2Tmp) ->
      {Intersec, Diff1, Diff2} when
      AssocList1   :: assoc_list(),
      AssocList2   :: assoc_list(),
      IntersecTmp      :: assoc_list(),
      Diff1Tmp :: assoc_list(),
      Diff2Tmp :: assoc_list(),
      Intersec         :: assoc_list(),
      Diff1    :: assoc_list(),
      Diff2    :: assoc_list().
intersection_and_differences_impl([Tuple | RestList1], [Tuple | RestList2], Intersec, Diff1, Diff2) ->
    intersection_and_differences_impl(RestList1, RestList2, [Tuple | Intersec], Diff1, Diff2);
intersection_and_differences_impl([{Key, _} = Tuple1 | RestList1], [{Key, _} = Tuple2 | RestList2],
                           Intersec, Diff1, Diff2) ->
    intersection_and_differences_impl(RestList1, RestList2,
                                      Intersec, [Tuple1 | Diff1], [Tuple2 | Diff2]);
intersection_and_differences_impl([{Key1, _} = Tuple1 | RestList1], [{Key2, _} | _] = AssocList2,
                                  Intersec, Diff1, Diff2) when Key1 < Key2 ->
    intersection_and_differences_impl(RestList1, AssocList2, Intersec, [Tuple1 | Diff1], Diff2);
intersection_and_differences_impl(AssocList1, [Tuple2 | RestList2], Intersec, Diff1, Diff2) ->
    intersection_and_differences_impl(AssocList1, RestList2, Intersec, Diff1, [Tuple2 | Diff2]);
intersection_and_differences_impl([Tuple1 | RestList1], [], Intersec, Diff1, Diff2) ->
    intersection_and_differences_impl(RestList1, [], Intersec, [Tuple1 | Diff1], Diff2);
intersection_and_differences_impl([], [], Intersec, Diff1, Diff2) ->
    ReversedIntersec      = lists:reverse(Intersec),
    ReversedDiff1 = lists:reverse(Diff1),
    ReversedDiff2 = lists:reverse(Diff2),
    {ReversedIntersec, ReversedDiff1, ReversedDiff2}.
