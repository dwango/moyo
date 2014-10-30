%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc モナドに関する処理を集めたユーティリティモジュール.
-module(moyo_monad).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         apply_maybe/3,
         maybe/1,
         maybe_fun/1
        ]).

-export_type([
              maybe/0,
              maybe_fun/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type maybe() :: {just, term()} | nothing.
%% なにか or Nothing を格納するデータ構造
-type maybe_fun() :: {just, fun()} | nothing.
%% 何らかの関数 or Nothing を格納するデータ構造

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc maybe()を作ります.
%%
%% {just nothing} は作れないので直接作ってください.
-spec maybe(term()) -> maybe().
maybe(nothing) -> nothing;
maybe(Value) -> {just, Value}.


%% @doc maybe()を作ります.
-spec maybe_fun(nothing | fun()) -> maybe().
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
