%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 条件分岐処理関連のユーティリティ関数を提供するモジュール
-module(moyo_cond).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         apply_if/3,
         apply_when/2,
         apply_unless/2,
         conditional/3,
         while/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc `Condition'が`true'の場合は`ThenFun'が、`false'の場合は`ElseFun'が実行される
-spec apply_if(Condition::boolean(), ThenFun, ElseFun) -> Result when
      ThenFun :: fun (() -> Result),
      ElseFun :: fun (() -> Result),
      Result  :: term().
apply_if(true,  ThenFun, _) -> ThenFun();
apply_if(false, _, ElseFun) -> ElseFun().

%% @doc `Condition'が`true'の場合は`ThenFun'が実行される
%%
%% 返り値は常に`ok'
-spec apply_when(Condition::boolean(), ThenFun) -> ok when
      ThenFun :: fun (() -> any()).
apply_when(true, ThenFun) -> _ = ThenFun(), ok;
apply_when(false, _)      -> ok.

%% @doc `Condition'が`false'の場合は`ThenFun'が実行される
%%
%% 返り値は常に`ok'
-spec apply_unless(Condition::boolean(), ThenFun) -> ok when
      ThenFun :: fun (() -> any()).
apply_unless(true, _)        -> ok;
apply_unless(false, ThenFun) -> _ = ThenFun(), ok.

%% @doc 三項演算子と同様の機能を提供する。Conditionがtrueなら２つ目の値が、falseなら３つ目の値が返る
-spec conditional(Condition::boolean(),TValue::any(), FValue::any() ) -> any().
conditional( true, TValue, _ ) -> TValue;
conditional( false, _ , FValue) -> FValue.

%% @doc `Acc0'が`Fun'の定義により処理して`Acc1'を返す。
%%
%% `Fun'内の処理の結果が`{true, AccOut}'になると、`AccOut'が`Fun'の引数としてもう一度処理される。
%% 結果が`{false, AccOut}'になると、`AccOut'を返す。
%% ```
%% > while(fun(X) when X >= 5 -> {false, X}; (X) -> {true, X + 1} end, 1).
%% 5
%% > while(fun(X) when X < 2 -> {false, X}; (X) -> {true, X - 1} end, 10).
%% 1
%% '''
-spec while(Fun, Acc0) -> Acc1 when
    Fun :: fun ((AccIn) -> {Continue :: boolean(), AccOut}),
    Acc0   :: AccIn,
    Acc1   :: AccOut,
    AccIn  :: term(),
    AccOut :: term().
while(Fun, Acc0) ->
    case Fun(Acc0) of
        {false, Acc1} -> Acc1;
        {true, Acc1} -> while(Fun, Acc1)
    end.
