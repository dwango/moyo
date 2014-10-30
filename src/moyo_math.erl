%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 数学的な関数を集めたモジュール.
-module(moyo_math).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         ceil/1,
         floor/1,

         gcd/2,
         pow_int/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 数(number)を切り上げて整数を返す.
%%
%% 指定した以上の整数で最小のものを返す.
%% ```
%% > ceil(1.0).
%% 1.
%% > ceil(0.5).
%% 1.
%% > ceil(0.0).
%% 0.
%% > ceil(-0.5).
%% 0.
%% > ceil(-1.0).
%% -1.
%% '''
-spec ceil(number()) -> integer().
ceil(Number) when Number > 0 ->
    Trunc = trunc(Number),
    case Number - Trunc == 0 of
        true  -> Trunc;
        false -> Trunc + 1
    end;
ceil(Number) -> trunc(Number).

%% @doc 数(number)を切り下げて整数を返す.
%%
%% 指定した以下の整数で最大のものを返す.
%% ```
%% > floor(1.0).
%% 1.
%% > floor(0.5).
%% 0.
%% > floor(0.0).
%% 0.
%% > floor(-0.5).
%% -1.
%% > floor(-1.0).
%% -1.
%% '''
-spec floor(number()) -> integer().
floor(Number) when Number > 0 ->
    trunc(Number);
floor(Number) ->
    Trunc = trunc(Number),
    case Number - Trunc == 0 of
        true  -> Trunc;
        false -> Trunc -1
    end.

%% @doc 最大公約数を求める.
%%
%% 両方の引数が0の場合, `both_0_error'をthrowする.
-spec gcd(A::integer(), B::integer()) -> GCD::integer().
gcd(0, 0) -> throw(both_0_error);
gcd(A, B) -> gcd_impl(abs(A), abs(B)).

%% @doc 累乗関数.
%%
%% 計算結果がinteger()になる計算のみ行える.
%% 具体的には、引数は整数のみで、第2引数は0以上のみを扱う.
-spec pow_int(Base::integer(), Exponent::non_neg_integer()) -> Value::integer().
pow_int(Base, Exponent) when Exponent >= 0, is_integer(Exponent) -> pow_int_impl(Base, Exponent, 1).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------------------------------
%% @private
gcd_impl(A, 0) -> A;
gcd_impl(A, B) ->
    Q = A div B,
    R = A - Q*B,
    gcd_impl(B, R).

%% @private
%%
%% pow_int_impl(Base, Exponent, K) = Base^Exponent*K
-spec pow_int_impl(Base::integer(), Exponent::non_neg_integer(), K::integer()) -> Value::integer().
pow_int_impl(_, 0, K) -> K;
pow_int_impl(Base, Exponent, K) when Exponent rem 2 =:= 1 -> pow_int_impl(Base*Base, Exponent div 2, Base*K);
pow_int_impl(Base, Exponent, K) -> pow_int_impl(Base*Base, Exponent div 2, K).
