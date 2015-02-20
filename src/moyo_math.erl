%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
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
         pow_int/2,
         divmod/2,
         random_sequence/1,
         random_sequence/2
        ]).

-export_type([
              random_sequence_symbols/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
%% ランダム文字列の記号
-type random_sequence_symbols() :: alphabetical | numeric | alphanumeric.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
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

%% @doc 除算した商と剰余を求める関数.
%%
%% 除数が0である場合, `badarith' errorが発生する.
-spec divmod(A::integer(), B::integer()) -> {Quotient::integer(), Remainder::integer()}.
divmod(A, B) -> {A div B, A rem B}.

%% @equiv random_sequence(Length, [])
-spec random_sequence(Length::non_neg_integer())-> binary().
random_sequence(Length) -> random_sequence(Length, [{symbol, alphabetical}]).

%% @doc ランダム文字列を返す
%%
%% この関数を利用する時は、random:seed を実行して乱数初期化をする必要があります。
%% DataTypeで出力形式を指定し、Symbolで出力内容を指定する．
-spec random_sequence(Length::non_neg_integer(), Options) -> binary() when
    Options :: [{symbol, Symbols}],
    Symbols::random_sequence_symbols().
random_sequence(Length, []) -> random_sequence(Length, [{symbol, alphabetical}]);
random_sequence(Length, [{symbol, Symbols}]) ->
    case Symbols of
        alphabetical -> random_alphabetical_sequence(Length);
        numeric -> random_numeric_sequence(Length);
        alphanumeric -> random_alphanumeric_sequence(Length)
    end.


%%----------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------------------------------
-spec gcd_impl(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
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

%% アルファベットの乱数列を返す
-spec random_alphabetical_sequence(Length::non_neg_integer()) -> binary().
random_alphabetical_sequence(Length) ->
    random_sequence_with_table(Length, <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ">>).

%% 数字の乱数列を返す
-spec random_numeric_sequence(Length::non_neg_integer()) -> binary().
random_numeric_sequence(Length) ->
    random_sequence_with_table(Length, <<"0123456789">>).

%% アルファベットと数字の乱数列を返す
-spec random_alphanumeric_sequence(Length::non_neg_integer()) -> binary().
random_alphanumeric_sequence(Length) ->
    random_sequence_with_table(Length, <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>).

%% 乱数列を返す
-spec random_sequence_with_table(Length::non_neg_integer(), Table::binary()) -> binary().
random_sequence_with_table(Length, Table) ->
    TailPos = byte_size(Table) - 1,
    << <<(binary:at(Table, random:uniform(TailPos)))>> || _ <- lists:seq(1, Length)>>.
