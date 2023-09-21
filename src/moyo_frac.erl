%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 分数を扱うモジュール.
%%
%% 分母は符号を持たないように、分子に符号を保持するようにする.
%%
-module(moyo_frac).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         %% 生成
         new/2,
         from_integer/1,
         from_float/1,
         from_number/1,

         %% 分母, 分子
         num/1,
         denom/1,

         %% 型変換
         to_tuple/1,
         to_integer/1,
         to_float/1,

         %% バイナリ関連
         from_binary/1,
         from_float_binary/1,
         to_binary/1,

         %% 四則演算
         add/2,
         sub/2,
         mul/2,
         divide/2,

         %% 分数判定
         is_fraction/1,

         %% 比較
         comp/2,

         %% 補助関数
         max/2,
         min/2
        ]).

-export_type([
              fraction/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-record(fraction, {
                   numerator   :: integer(),
                   denominator :: integer()
                  }).

-opaque fraction() :: #fraction{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 分数を生成する.
%%
%% 分母に0を与えられた場合, `denominator_0_error'をthrowする.
-spec new(Num::integer(), Denom::integer()) -> fraction().
new(_, 0)       -> throw(denominator_0_error);
new(Num, Denom) -> reduction(adjust_sign(#fraction{numerator = Num, denominator = Denom})).

%% @doc 整数を分数に変換する.
-spec from_integer(Int::integer()) -> Frac::fraction().
from_integer(Int) -> #fraction{numerator = Int, denominator = 1}.

%% @doc 浮動小数点数を分数に変換する.
%%
%% floatの精度の問題で同じと思っている値を比べても同じと判断されない場合がある.
%%
%% ex:
%% ```
%% 1> moyo_frac:comp(moyo_frac:from_float(3.14), moyo_frac:new(314, 100)).
%% 1
%% '''
%%
%% 非正規化数を分数にすることはできるが, to_floatすると割り算でerrorが発生する.
%% 無限大, NaNに関してはErlangのfloatでは使えないので考慮していない.
-spec from_float(float()) -> fraction().
from_float(Float) when Float =:= +0.0 orelse Float =:= -0.0 ->
    moyo_frac:new(0, 1);
from_float(Float) ->
    <<MaybeSign:1, MaybeExponent:11, MaybeFraction:52>> = <<Float/float>>,
    <<Fraction:53>> = <<1:1, MaybeFraction:52>>,
    % 1023: 指数部バイアス, 52: 仮数部を整数化
    Sign = case MaybeSign of 0 -> 1; 1 -> -1 end,
    case - MaybeExponent + 1023 + 52 of
        Pos when Pos >= 0 ->
            moyo_frac:new(Sign * Fraction, 1 bsl Pos);
        Neg ->
            moyo_frac:new(Sign * Fraction * (1 bsl -Neg), 1)
    end.

%% @doc 数値(整数|浮動小数点数)を分数に変換する.
-spec from_number(number()) -> fraction().
from_number(Number) when is_integer(Number) ->
    from_integer(Number);
from_number(Number) when is_float(Number) ->
    from_float(Number).

%% @doc 分子を返す.
-spec num(Frac::fraction()) -> Num::integer().
num(#fraction{numerator = Num}) -> Num.

%% @doc 分母を返す.
-spec denom(Frac::fraction()) -> Denom::integer().
denom(#fraction{denominator = Denom}) -> Denom.

%% @doc 分数をタプル({分子, 分母})に変換する.
-spec to_tuple(Frac::fraction()) -> {Num::integer(), Denom::integer()}.
to_tuple(#fraction{numerator = Num, denominator = Denom}) -> {Num, Denom}.

%% @doc 整数部分を返す.
-spec to_integer(Frac::fraction()) -> Int::integer().
to_integer(#fraction{numerator = Num, denominator = Denom}) -> Num div Denom.

%% @doc 分数を浮動小数点数に変換する.
-spec to_float(Frac::fraction()) -> Float::float().
to_float(#fraction{numerator = Num, denominator = Denom}) -> Num / Denom.

%% @doc バイナリの整数、負数、分数、小数を分数に変換する.
%%
%% ex:
%% ```
%% 1> moyo_frac:from_binary(<<"5">>).
%% {fraction, 5, 1}
%% 2> moyo_frac:from_binary(<<"-5">>).
%% {fraction, -5, 1}
%% 3> moyo_frac:from_binary(<<"1/5">>).
%% {fraction, 1, 5}.
%% 4> moyo_frac:from_binary(<<"0.25">>).
%% {fraction, 1, 4}
%% '''
-spec from_binary(binary()) -> fraction().
from_binary(Binary) ->
    case binary:match(Binary, [<<"/">>, <<".">>]) of
        nomatch ->
            [Num, Denom] = [Binary, 1],
            new(binary_to_integer(Num), Denom);
        _ ->
            case binary:split(Binary, <<"/">>) of
                [Num, Denom] -> new(binary_to_integer(Num), binary_to_integer(Denom));
                _            -> from_float_binary(Binary)
            end
    end.

%% @doc バイナリ形式の小数を分数に変換する.
%%
%% 小数(有理数)を分数にする関数.
%% 文字列で見えている範囲で分数にする.<br/>
%% ex: `3.1415 -> 31415/10000 -> 6283/2000'<br/>
%% 注: 約分は行われる.
%%
%% ex:
%% ```
%% 1> moyo_frac:from_float_binary(<<"3.1415">>).
%% {fraction, 6283, 2000}
%% '''
-spec from_float_binary(DecimalBin::binary()) -> fraction().
from_float_binary(<<"-", Bin/binary>>) ->
    Frac = from_float_binary(Bin),
    mul(-1, Frac);
from_float_binary(Bin) ->
    [Int, Dec] = binary:split(Bin, <<".">>),
    ok = if Int =:= <<>> orelse Dec =:= <<>> -> error(badarg); true -> ok end,
    %% truncを取っているのは小数から整数にするため.
    %% 小数点以下は0で, trunc前後で値は変わらないと想定している.
    Denom = trunc(math:pow(10, byte_size(Dec))),
    Num = binary_to_integer(<<Int/binary, Dec/binary>>),
    moyo_frac:new(Num, Denom).

%% @doc 分数をバイナリに変換する.
-spec to_binary(fraction()) -> binary().
to_binary(#fraction{numerator = Num, denominator = Denom}) ->
    BinNum   = integer_to_binary(Num),
    BinDenom = integer_to_binary(Denom),
    <<BinNum/binary, "/", BinDenom/binary>>.

%% @doc 足し算 数(分数|浮動小数点数|整数)同士.
%%
%% 9パターンに対応.
-spec add(A::fraction() | number(), B::fraction() | number()) -> R::fraction().
add(#fraction{numerator = NumA, denominator = DenomA}, #fraction{numerator = NumB, denominator = DenomB}) ->
    reduction(#fraction{numerator = NumA*DenomB + NumB*DenomA, denominator = DenomA*DenomB});
add(NumberA, NumberB) when is_number(NumberA), is_number(NumberB) ->
    from_number(NumberA+NumberB);
add(NumberA, FractionB = #fraction{}) when is_number(NumberA) ->
    add(FractionB, NumberA);
add(#fraction{numerator = NumA, denominator = DenomA}, IntB) when is_integer(IntB) ->
    reduction(#fraction{numerator = NumA + IntB*DenomA, denominator = DenomA });
add(FractionA = #fraction{}, FloatB) when is_float(FloatB) ->
    add(FractionA, from_float(FloatB)).


%% @doc 引き算 数(分数|浮動小数点数|整数)同士.
%%
%% 9パターンに対応.
-spec sub(A::fraction() | number(), B::fraction() | number()) -> R::fraction().
sub(#fraction{numerator = NumA, denominator = DenomA}, #fraction{numerator = NumB, denominator = DenomB}) ->
    reduction(#fraction{numerator = NumA*DenomB - NumB*DenomA, denominator = DenomA*DenomB});
sub(NumberA, NumberB) when is_number(NumberA), is_number(NumberB) ->
    from_number(NumberA-NumberB);
sub(IntA, #fraction{numerator = NumB, denominator = DenomB}) when is_integer(IntA) ->
    reduction(#fraction{numerator = IntA*DenomB - NumB, denominator = DenomB });
sub(FloatA, FractionB = #fraction{}) when is_float(FloatA) ->
    sub(from_float(FloatA), FractionB);
sub(#fraction{numerator = NumA, denominator = DenomA}, IntB) when is_integer(IntB) ->
    reduction(#fraction{numerator = NumA - IntB*DenomA, denominator = DenomA });
sub(FractionA = #fraction{}, FloatB) when is_float(FloatB) ->
    sub(FractionA, from_float(FloatB)).

%% @doc 掛け算 数(分数|浮動小数点数|整数)同士.
%%
%% 9パターンに対応.
-spec mul(A::fraction() | number(), B::fraction() | number()) -> R::fraction().
mul(#fraction{numerator = NumA, denominator = DenomA}, #fraction{numerator = NumB, denominator = DenomB}) ->
    reduction(#fraction{numerator = NumA*NumB, denominator = DenomA*DenomB});
mul(NumberA, NumberB) when is_number(NumberA), is_number(NumberB) ->
    from_number(NumberA*NumberB);
mul(NumberA, FractionB = #fraction{}) when is_number(NumberA) ->
    mul(FractionB, NumberA);
mul(#fraction{numerator = NumA, denominator = DenomA}, IntB) when is_integer(IntB) ->
    reduction(#fraction{numerator = NumA*IntB, denominator = DenomA });
mul(FractionA = #fraction{}, FloatB) when is_float(FloatB) ->
    mul(FractionA, from_float(FloatB)).

%% @doc 割り算 数(分数|浮動小数点数|整数)同士.
%%
%% 9パターンに対応.
-spec divide(A::fraction() | number(), B::fraction() | number()) -> R::fraction().
divide(#fraction{numerator = NumA, denominator = DenomA}, #fraction{numerator = NumB, denominator = DenomB}) ->
    new(NumA*DenomB, NumB*DenomA);
divide(NumberA, NumberB) when is_number(NumberA), is_number(NumberB) ->
    from_number(NumberA/NumberB);
divide(IntA, #fraction{numerator = NumB, denominator = DenomB}) when is_integer(IntA) ->
    new(IntA*DenomB, NumB);
divide(FloatA, FractionB = #fraction{}) when is_float(FloatA) ->
    divide(from_float(FloatA), FractionB);
divide(#fraction{numerator = NumA, denominator = DenomA}, IntB) when is_integer(IntB) ->
    new(NumA, DenomA*IntB);
divide(FractionA = #fraction{}, FloatB) when is_float(FloatB) ->
    divide(FractionA, from_float(FloatB)).

%% @doc 分数かどうかの判定.
-spec is_fraction(term()) -> boolean().
is_fraction(#fraction{}) -> true;
is_fraction(_)           -> false.

%% @doc 分数の比較.
%%
%% Aの方が大きい場合は正整数(1),
%% Bの方が大きい場合は負整数(-1),
%% 同じ場合は0を返す.<br/>
%% 1, -1ではなく, 正整数, 負整数でチェックすること.
-spec comp(A::fraction(), B::fraction()) -> R::integer().
comp(#fraction{numerator = NumA, denominator = DenomA}, #fraction{numerator = NumB, denominator = DenomB}) ->
    A = DenomB*NumA, B = DenomA*NumB,
    if
        A > B -> 1;
        A < B -> -1;
        true -> 0
    end.

%% @doc 大きい方の値を返す.
-spec max(A::fraction(), B::fraction()) -> M::fraction().
max(A, B) ->
    case comp(A, B) of
        Pos when Pos > 0 -> A;
        _                -> B % AとBが同じ場合はBを返す(同じなので問題ない).
    end.

%% @doc 小さい方の値を返す.
-spec min(A::fraction(), B::fraction()) -> M::fraction().
min(A, B) ->
    case comp(A, B) of
        Neg when Neg < 0 -> A;
        _                -> B % AとBが同じ場合はBを返す(同じなので問題ない).
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 約分.
-spec reduction(fraction()) -> fraction().
reduction(#fraction{numerator = Num, denominator = Denom}) ->
    GCD = moyo_math:gcd(Num, Denom),
    #fraction{numerator = Num div GCD, denominator = Denom div GCD}.

%% @doc 分母が負の場合, 分母分子それぞれの符号を反転させる.
-spec adjust_sign(TempFrac::fraction()) -> Frac::fraction().
adjust_sign(#fraction{numerator = Num, denominator = Denom}) when Denom < 0 ->
    #fraction{numerator = -1*Num, denominator = -1*Denom};
adjust_sign(Frac) -> Frac.
