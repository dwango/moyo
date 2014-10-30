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
         conditional/3
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
