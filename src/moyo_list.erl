%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc リストに関する処理を集めたユーティリティモジュール.
-module(moyo_list).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         delete_all/2,
         find_if/2,
         take/2,
         take_if/2,
         replace_if/3,
         position/2,
         shuffle/1,

         foldl_while/3,
         foldr_while/3,

         maybe_map/2,
         maybe_foreach/2,
         maybe_foldl/3,
         maybe_foldr/3,

         maybe_pmap/2,
         maybe_pmap/3,

         longest_common_prefix/1,
         split_longest_common_prefix/1,

         inits/1,
         tails/1,
         uniq/1,
         adjacent_uniq/1,
         group_by/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc `List'内に存在する全ての`Element'を削除する
%%
%% ```
%% > moyo_list:delete_all(aaa, [aaa, bbb, ccc, bbb, aaa]).
%% [bbb, ccc, bbb]
%% '''
-spec delete_all(Element, List1) -> List2 when
      Element :: term(),
      List1   :: [Element],
      List2   :: [Element].
delete_all(Element, List) ->
    [X || X <- List, X =/= Element].

%% @doc `PredFun'の結果が`true'となる`List'内の最初の要素を検索する
-spec find_if(PredFun, List) -> {ok, Element} | error when
      PredFun :: fun ((Element) -> boolean()),
      List    :: [Element],
      Element :: term().
find_if(_PredFun, []) ->
    error;
find_if(PredFun, [X | List]) ->
    case PredFun(X) of
        false -> find_if(PredFun, List);
        true  -> {ok, X}
    end.

%% @doc `Element'と一致する最初の要素を検索し、その値を除いたリストを返す
%%
%% ```
%% > moyo_list:take(bbb, [aaa, bbb, ccc]).
%% {ok, [aaa, ccc]}.
%%
%% > moyo_list:take(bbb, [111, 222, 333]).
%% error
%% '''
-spec take(Element, List1) -> {ok, List2} | error when
      Element :: term(),
      List1   :: [Element],
      List2   :: [Element].
take(Element, List1) ->
    case take_if(fun (X) -> X =:= Element end, List1) of
        error          -> error;
        {ok, _, List2} -> {ok, List2}
    end.

%% @doc `PredFun'の結果が`true'となる`List'内の最初の要素を検索し, その値とその値を除いたリストを返す.
-spec take_if(PredFun, List1) -> {ok, Element, List2} | error when
      PredFun :: fun ((Element) -> boolean()),
      List1   :: [Element],
      Element :: term(),
      List2   :: [Element].
take_if(PredFun, List) -> take_if_impl(PredFun, List, []).

%% @doc `PredFun'の結果が`true'となった最初の要素を`Value'で置換する
-spec replace_if(PredFun, term(), list()) -> list() when
      PredFun :: fun ((term()) -> boolean()).
replace_if(PredFun, Value, List) -> replace_if_impl(PredFun, Value, List, []).

%% @doc `List'内で最初に`Value'が出現する位置を返す
%%
%% `Value'が存在しない場合は`error'が返される
-spec position(term(), list()) -> {ok, Position::pos_integer()} | error.
position(Value, List) -> position_impl(Value, List, 1).

%% @doc 入力リストの順番を無作為に並べ替える
-spec shuffle([Element]) -> [Element] when Element :: term().
shuffle(List) ->
    [E || {_, E} <- lists:ukeysort(1, [{rand:uniform(), E} || E <- List])].

%% @doc lists:foldl/3 の中断機能追加版: 関数適用後の結果が`{false, _}'となった場合は、そこで走査が中断される.
%%
%% `Fun'の結果は`{true, Result}' or `{false, Result}'のいずれかである必要がある.
-spec foldl_while(Fun, Initial::term(), List) -> Result when
      Fun     :: fun ((Element, Acc::term()) -> {true, Result} | {false, Result}),
      List    :: [Element],
      Element :: term(),
      Result  :: term().
foldl_while(_Fun, Acc, []) ->
    Acc;
foldl_while(Fun, Acc, [Element | List]) ->
    case Fun(Element, Acc) of
        {false, Result} -> Result;
        {true,  Result} -> foldl_while(Fun, Result, List)
    end.

%% @doc lists:foldr/3 の中断機能追加版: 関数適用後の結果が`{false, _}'となった場合は、そこで走査が中断される.
%%
%% `Fun'の結果は`{true, Result}' or `{false, Result}'のいずれかである必要がある.
-spec foldr_while(Fun, Initial::term(), List) -> Result when
      Fun     :: fun ((Element, Acc::term()) -> {true, Result} | {false, Result}),
      List    :: [Element],
      Element :: term(),
      Result  :: term().
foldr_while(_Fun, Acc, []) ->
    Acc;
foldr_while(Fun, Acc, List) ->
    element(2, foldr_while_impl(Fun, Acc, List)).

%% @doc lists:foldl/3 の maybe版: 関数適用結果が`{error, Reason}'となる要素があれば、そこで走査が中断される.
%%
%% `Fun'の結果は `{ok, Result}' or `{error, Reason}' のいずれかである必要がある.
-spec maybe_foldl(Fun, Initial::term(), List) -> {ok, Result::term()} | {error, Reason} when
      Fun       :: fun ((Element, Acc::term()) -> {ok, AccNext::term()} | {error, Reason}),
      List      :: [Element],
      Element   :: term(),
      Reason    :: term().
maybe_foldl(_Fun, Acc, []) ->
    {ok, Acc};
maybe_foldl(Fun, Acc, [Element | List]) ->
    case Fun(Element, Acc) of
        {error, Reason} -> {error, Reason};
        {ok, AccNext}   -> maybe_foldl(Fun, AccNext, List)
    end.

%% @doc lists:foldr/3 の maybe版: 関数適用結果が`{error, Reason}'となる要素があれば、そこで走査が中断される.
%%
%% `Fun'の結果は `{ok, Result}' or `{error, Reason}' のいずれかである必要がある.
-spec maybe_foldr(Fun, Initial::term(), List) -> {ok, Result::term()} | {error, Reason} when
      Fun       :: fun ((Element, Acc::term()) -> {ok, AccNext::term()} | {error, Reason}),
      List      :: [Element],
      Element   :: term(),
      Reason    :: term().
maybe_foldr(Fun, Initial, List) ->
    maybe_foldl(Fun, Initial, lists:reverse(List)).

%% @doc lists:map/2 の maybe版: 関数適用結果が`{error, Reason}'となる要素があれば、そこで走査が中断される.
%%
%% `Fun'の結果は `{ok, Result}' or `{error, Reason}' のいずれかである必要がある.
-spec maybe_map(Fun, List) -> {ok, Result} | {error, Reason} when
      Fun       :: fun ((Element) -> {ok, PerResult} | {error, Reason}),
      List      :: [Element],
      Element   :: term(),
      Result    :: [PerResult],
      PerResult :: term(),
      Reason    :: term().
maybe_map(Fun, List) ->
    MapResult =
        maybe_foldl(fun (Element, Acc) ->
                            case Fun(Element) of
                                {error, Reason} -> {error, Reason};
                                {ok, PerResult} -> {ok, [PerResult | Acc]}
                            end
                    end,
                    [],
                    List),
    case MapResult of
        {ok, Result}    -> {ok, lists:reverse(Result)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc lists:foreach/2 の maybe版: 関数適用結果が`{error, Reason}'となる要素があれば、そこで走査が中断される.
%%
%% `Fun'の結果が`{error, Reason}'の場合はそこで走査が中断され、それ以外の場合は継続される.
-spec maybe_foreach(Fun, List) -> ok | {error, Reason} when
      Fun     :: fun ((Element) -> {error, Reason} | any()),
      List    :: [Element],
      Element :: term(),
      Reason  :: term().
maybe_foreach(Fun, List) ->
    ForeachResult =
        maybe_foldl(fun (Element, _) ->
                            case Fun(Element) of
                                {error, Reason} -> {error, Reason};
                                _               -> {ok, ok}
                            end
                    end,
                    ok,
                    List),
    case ForeachResult of
        {ok, _}         -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @equiv maybe_pmap(Fun, List, infinity)
-spec maybe_pmap(Fun, List) -> {ok, Values} | {error, Reason} when
      Fun       :: fun ((Arg) -> {ok, Value} | {error, Reason}),
      List      :: [Arg],
      Values    :: [Value],
      Arg       :: term(),
      Value     :: term(),
      Reason    :: ExitError | term(),
      ExitError :: {'EXIT', {ExitReason::term(), StackTrace::term()}}.
maybe_pmap(Fun, List) ->
    maybe_pmap(Fun, List, infinity).

%% @doc {@link maybe_map/2}の並列版.
%%
%% `Fun'の実行中のエラーが発生した場合は`ExitError'、タイムアウトが発生した場合は`ExitTimeout'が結果として返される.
%% また, 1つでも関数適用結果が`{error, Reason}'となる要素があれば, そこで走査を中断し、残ったプロセスを終了させる.
-spec maybe_pmap(Fun, List, Timeout) -> {ok, Values} | {error, Reason} when
      Fun         :: fun ((Arg) -> {ok, Value} | {error, Reason}),
      List        :: [Arg],
      Timeout     :: timeout(),
      Values      :: [Value],
      Arg         :: term(),
      Value       :: term(),
      Reason      :: ExitError | ExitTimeout | term(),
      ExitError   :: {'EXIT', {ExitReason::term(), StackTrace::term()}},
      ExitTimeout :: {'EXIT', timeout}.
maybe_pmap(Fun, List, Timeout) ->
    maybe_map(fun (Result) -> Result end,
              pmap_monitor(Fun, List, Timeout)).

%% @doc `Lists'内のリスト群のLongestCommonPrefixの長さを返す
%%
%% {@link binary:longest_common_prefix/1}のリスト版
%%
%% ```
%% > moyo_list:longest_common_prefix(["erlang", "ergonomy"]).
%% 2
%%
%% > moyo_list:longest_common_prefix(["erlang", "perl"]).
%% 0
%% '''
-spec longest_common_prefix(Lists) -> LongestCommonPrefixLength when
      Lists                     :: [List],
      List                      :: [term()],
      LongestCommonPrefixLength :: non_neg_integer().
longest_common_prefix(Lists = [_ | _]) ->
    longest_common_prefix_impl(Lists, 0);
longest_common_prefix(Arg) ->
    error(badarg, [Arg]). % 引数は、一要素以上を含むリストである必要がある

%% @doc `Lists'内の各リストを'LongestCommonPrefix部分'と'それ以降のSuffix部分'に分割する
%%
%% なお'LongestCommonPrefix部分'は全てのリストで共通のため、結果では一つにまとめられている
%%
%% ```
%% > moyo_list:split_longest_common_prefix(["erlang", "ergonomy"]).
%% {"er", ["lang", "gonomy"]}
%%
%% > moyo_list:split_longest_common_prefix(["erlang", "perl"]).
%% {"", ["erlang", "perl"]}
%% '''
-spec split_longest_common_prefix(Lists) -> {LongestCommonPrefix, [Suffix]} when
      Lists               :: [List],
      List                :: [term()],
      LongestCommonPrefix :: List,
      Suffix              :: List.
split_longest_common_prefix(Lists) ->
    %% 若干効率は落ちるが、コードを簡潔にするために longest_common_prefix/1 を利用して実装する
    Length = longest_common_prefix(Lists),
    [List1 | Rest] = Lists,
    {CommonPrefix, Suffix1} = lists:split(Length, List1),
    SuffixList = [Suffix1 | [lists:nthtail(Length, List) || List <- Rest]],
    {CommonPrefix, SuffixList}.

%% @doc `List'の全ての先頭部分リストを長さの増加する順に並べて返す
%%
%% 関数名およびインタフェースはHaskellの`Data.List.inits関数'に倣った
%%
%% ```
%% > moyo_list:inits("abc").
%% ["", "a", "ab", "abc"]
%% '''
-spec inits([Element]) -> [[Element]] when Element :: term().
inits(List) ->
    lists:map(fun lists:reverse/1, lists:reverse(tails(lists:reverse(List)))).

%% @doc `List'の全ての末尾部分リストを長さの減少する順に並べて返す
%%
%% 関数名およびインタフェースはHaskellの`Data.List.tails関数'に倣った
%%
%% ```
%% > moyo_list:tails("abc").
%% ["abc", "ab", "a", ""]
%% '''
-spec tails([Element]) -> [[Element]] when Element :: term().
tails([])   -> [[]];
tails(List) -> [List | tails(tl(List))].

%% @doc `List'内で重複要素を削除する
%%
%% 計算量は`O(n log n)'. 要素の出現順は保存される.
%% リストがソートされてもよい場合は{@link lists:usort/1}の方が高速.
%% 連接する重複要素のみを削除したい場合はこの関数の代わりに{@link moyo_list:adjacent_uniq/1}を使う.
%%
%% なお, 要素の一致判定は`=:='にて行われる (`1.0'と`1'は別要素扱い)
%%
%% ```
%% > moyo_list:uniq([a, a, b, b, c, c]).
%% [a, b, c]
%%
%% > moyo_list:uniq([c, a, c, b, b, a]).
%% [c, a, b]
%% '''
-spec uniq([term()]) -> [term()].
uniq(List) -> uniq_impl(List, #{}, []).

%% @doc `List'内で連接する重複要素を削除する
%%
%% リスト全体を通して各要素をユニークにしたい場合は、事前にリストをソートしておくか、
%% この関数の代わりに{@link lists:usort/1}または{@link moyo_list:uniq/1}を使う必要がある。
%%
%% なお、要素の一致判定は`=:='にて行われる (`1.0'と`1'は別要素扱い)
%%
%% ```
%% > moyo_list:adjacent_uniq([a, a, b, b, c, c]).
%% [a, b, c]
%%
%% > moyo_list:adjacent_uniq([a, a, b, b, a, a]).
%% [a, b, a]
%% '''
-spec adjacent_uniq([term()]) -> [term()].
adjacent_uniq(List) ->
    lists:foldr(
      fun (X, [])        -> [X];
          (X, [X | Acc]) -> [X | Acc];
          (X, Acc)       -> [X | Acc]
      end,
      [],
      List).

%% @doc tuple の N 番目の値でグループ化する.
-spec group_by(non_neg_integer(), [tuple()]) -> [{term(), [tuple()]}].
group_by(N, TupleList) ->
    lists:foldl(fun(Tuple, [{Key, TupleListOfKey}|Rest]) when element(N, Tuple) =:= Key ->
                        [{Key, [Tuple|TupleListOfKey]}|Rest];
                   (Tuple, GroupList) ->
                        [{element(N, Tuple), [Tuple]}|GroupList]
                end,
                [], lists:keysort(N, TupleList)).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec pmap_monitor(Fun, Args, Timeout) -> [{ok, Result} | {error, Reason}] when
      Fun     :: fun((Arg) -> {ok, Result} | {error, Reason}),
      Arg     :: term(),
      Args    :: [Arg],
      Timeout :: timeout(),
      Result  :: term(),
      Reason  :: term().
pmap_monitor(Fun, Args, Timeout) ->
    {Pid, MonitorRef} = spawn_monitor(fun() -> exit(self(), pmap_link(Fun, Args)) end),
    receive
        {'DOWN', MonitorRef, _, _, Results} when is_list(Results) -> Results;
        {'DOWN', MonitorRef, _, _, SomeReason} -> [{error, {'EXIT', SomeReason}}]
    after Timeout ->
        true = exit(Pid, kill),
        receive
            {'DOWN', MonitorRef, _, _, _} -> [{error, {'EXIT', timeout}}]
        end
    end.

-spec pmap_link(Fun, Args) -> [{ok, Result} | {error, Reason}] when
      Fun    :: fun((Arg) -> {ok, Result} | {error, Reason}),
      Arg    :: term(),
      Args   :: [Arg],
      Result :: term(),
      Reason :: term().
pmap_link(Fun, Args) ->
    Self = self(),
    Ref = make_ref(),
    Pids = [spawn_link(fun() -> Self ! {Ref, self(), pmap_call(Fun, Arg)} end) || Arg <- Args],
    Results = pmap_receive(Ref, {length(Pids), #{}}),
    [maps:get(Pid, Results) || Pid <- Pids, maps:is_key(Pid, Results)].

-spec pmap_call(Fun, Arg) -> {ok, Result} | {error, Reason} when
      Fun    :: fun((Arg) -> {ok, Result} | {error, Reason}),
      Arg    :: term(),
      Result :: term(),
      Reason :: term().
pmap_call(Fun ,Arg) ->
    process_flag(trap_exit, true),
    try
        case Fun(Arg) of
            {ok,    Result} -> {ok,    Result};
            {error, Reason} -> {error, Reason};
            SomeResult      -> {error, {'EXIT', SomeResult}}
        end
    catch
        ExceptionClass:SomeReason -> {error, {'EXIT', ExceptionClass, SomeReason, erlang:get_stacktrace()}}
    end.

-spec pmap_receive(Ref, {Count, Results}) -> Results when
      Ref     :: reference(),
      Count   :: non_neg_integer(),
      Results :: #{pid() => {ok, Result} | {error, Reason}},
      Result  :: term(),
      Reason  :: term().
pmap_receive(_,   {0,      Results}) -> Results;
pmap_receive(Ref, {Count, Results}) ->
    receive {Ref, Pid, Result} when is_pid(Pid) ->
        ReceivedResults = maps:put(Pid, Result, Results),
        case Result of
            {error, _} -> ReceivedResults;
            {ok,    _} -> pmap_receive(Ref, {Count - 1, ReceivedResults})
        end
    end.

-spec take_if_impl(PredFun, List1, Acc) -> {ok, Element, List2} | error when
      PredFun :: fun ((Element) -> boolean()),
      List1   :: [Element],
      Acc     :: [Element],
      Element :: term(),
      List2   :: [Element].
take_if_impl(_PredFun, [], _Acc) -> error;
take_if_impl(PredFun, [X | List], Acc) ->
    case PredFun(X) of
        true  -> {ok, X, lists:reverse(Acc, List)};
        false -> take_if_impl(PredFun, List, [X | Acc])
    end.

-spec replace_if_impl(PredFun, term(), list(), list()) -> list() when
      PredFun :: fun ((term()) -> boolean()).
replace_if_impl(_PredFun, _Value, [], Acc)       -> lists:reverse(Acc);
replace_if_impl(PredFun, Value, [X | List], Acc) ->
    case PredFun(X) of
        false -> replace_if_impl(PredFun, Value, List, [X | Acc]);
        true  -> lists:reverse([Value | Acc], List)
    end.

-spec position_impl(term(), list(), pos_integer()) -> {ok, pos_integer()} | error.
position_impl(_Value, [], _Position)        -> error;
position_impl(Value, [Value | _], Position) -> {ok, Position};
position_impl(Value, [_ | List], Position)  -> position_impl(Value, List, Position + 1).

-spec foldr_while_impl(Fun, Initial::term(), List) -> {true, Result} | {false, Result} when
      Fun     :: fun ((Element, Acc::term()) -> {true, Result} | {false, Result}),
      List    :: [Element],
      Element :: term(),
      Result  :: term().
foldr_while_impl(_Fun, Acc, []) ->
    {true, Acc};
foldr_while_impl(Fun, Acc, [Element | List]) ->
    case foldr_while_impl(Fun, Acc, List) of
        {false, Result} -> {false, Result};
        {true,  Result} -> Fun(Element, Result)
    end.

-spec longest_common_prefix_impl(Lists, non_neg_integer()) -> non_neg_integer() when
      Lists :: [List],
      List  :: [term()].
longest_common_prefix_impl([[] | _], Acc) ->
    Acc;
longest_common_prefix_impl([[Head | Tail] | Lists], Acc) ->
    case lists:all(fun (List) -> List =/= [] andalso hd(List) =:= Head end, Lists) of
        false -> Acc;
        true  -> longest_common_prefix_impl([Tail | lists:map(fun erlang:tl/1, Lists)], Acc + 1)
    end.

-spec uniq_impl(List::[term()], Map::map(), Acc::[term()]) -> [term()].
uniq_impl([], _Map, Acc) -> lists:reverse(Acc);
uniq_impl([H|T], Map, Acc) ->
    case maps:is_key(H, Map) of
        true -> uniq_impl(T, Map, Acc);
        false -> uniq_impl(T, maps:put(H, true, Map), [H|Acc])
    end.
