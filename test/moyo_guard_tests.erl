%% coding: latin-1
%%
%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc guard_macrosモジュールのユニットテスト
-module(moyo_guard_tests).
-include_lib("eunit/include/eunit.hrl").
-include("guard.hrl").


-define(checkGuard(Expr, Guard),
    (
        fun(__TMP) ->
            case __TMP of
                __TMP2 when Guard(__TMP2) -> true;
                __TMP2 when not Guard(__TMP2) -> false
            end
        end
    )(Expr)
).

is_alpha_test_() ->
    [
        {"文字が英字か調べる",
            fun() ->
                Input = "AaQqZz",
                [?assert(?checkGuard(X, ?IS_ALPHA)) || X <- Input],
                Input2="19*; ",
                [?assert(not ?checkGuard(X, ?IS_ALPHA)) || X <- Input2]
            end
        }
    ].

is_num_test_() ->
    [
        {"文字が数文字か調べる",
            fun() ->
                Input = "1234567890",
                [?assert(?checkGuard(X, ?IS_NUM)) || X <- Input],
                Input2="AaQqZz*; ",
                [?assert(not ?checkGuard(X, ?IS_NUM)) || X <- Input2]
            end
        }
    ].

is_alpha_num_test_() ->
    [
        {"文字が英数字か調べる",
            fun() ->
                Input = "AaQqZz19",
                [?assert(?checkGuard(X, ?IS_ALPHA_NUM)) || X <- Input],
                Input2="*; ",
                [?assert(not ?checkGuard(X, ?IS_ALPHA_NUM)) || X <- Input2]
            end
        }
    ].