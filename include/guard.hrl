%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc ガード用マクロ
%%


-ifndef(MOYO_GUARD_MACROS_HRL).
-define(MOYO_GUARD_MACROS_HRL, true).

-define(IS_ALPHA(Char), ($a =< Char andalso Char =< $z orelse $A =< Char andalso Char =< $Z)).
-define(IS_NUM(Char), ($0 =< Char andalso Char =< $9)).
-define(IS_ALPHA_NUM(Char), (?IS_ALPHA(Char) orelse ?IS_NUM(Char))).

-endif.
