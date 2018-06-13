%% @copyright 2015 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_char_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

is_alpha_test_() -> [
    {"文字が英字か調べる",
        fun() ->
            Input = "aAzZTest",
            [?assert(moyo_char:is_alpha(X)) || X <- Input],
            Input2 = "09 +*_~",
            [?assert(not moyo_char:is_alpha(X)) || X <- Input2]
        end}
].

is_num_test_() -> [
    {"文字が数字か調べる",
        fun() ->
            Input = "0123456789",
            [?assert(moyo_char:is_num(X)) || X <- Input],
            Input2 = "aAzZ +*_~",
            [?assert(not moyo_char:is_num(X)) || X <- Input2]
        end}
].

is_alpha_num_test_() -> [
    {"文字が英数字か調べる",
        fun() ->
            Input = "aAzZ09Test42",
            [?assert(moyo_char:is_alpha_num(X)) || X <- Input],
            Input2 = " +*_~",
            [?assert(not moyo_char:is_alpha_num(X)) || X <- Input2]
        end}
].
