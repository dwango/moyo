%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc モナドに関する処理を集めたユーティリティモジュール.
-module(moyo_monad).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         apply_maybe/3,
         maybe_val/1,
         maybe_fun/1
        ]).

-export_type([
              maybe_val/0,
              maybe_fun/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type maybe_val() :: {just, term()} | nothing.
%% なにか or Nothing を格納するデータ構造
-type maybe_fun() :: {just, fun()} | nothing.
%% 何らかの関数 or Nothing を格納するデータ構造

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc maybe_val()を作ります.
%%
%% {just nothing} は作れないので直接作ってください.
-spec maybe_val(term()) -> maybe_val().
maybe_val(nothing) -> nothing;
maybe_val(Value) -> {just, Value}.


%% @doc maybe()を作ります.
-spec maybe_fun(nothing | fun()) -> maybe_val().
maybe_fun(nothing) -> nothing;
maybe_fun(Fun) when is_function(Fun) -> {just, Fun};
maybe_fun(Fun) -> error({invalid_function, Fun}).


%% @doc MaybeFunが{just, Function}なら apply(Function, ArgList) を実行します. MaybeFunがnothingならDefaultValue を返します
-spec apply_maybe(maybe_fun(), list(any()), any()) -> any().
apply_maybe(MaybeFun, ArgList, DefaultValue) ->
    case MaybeFun of
        nothing -> DefaultValue;
        {just, Fun} when is_function(Fun, length(ArgList)) ->
            apply(Fun, ArgList);
        {just, Fun} -> error({invalid_function, Fun});
        M -> error({invalid_maybe, M})
    end.


%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
