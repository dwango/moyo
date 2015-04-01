%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc バリデーションに関する処理を集めたユーティリティモジュール
%%
%% パラメータに対して, 指定したデータ型かどうかを検証する.
%% また, 値に対して制限(範囲や長さ等)を与えることや, オプションを指定することもできる.
%%
%% === 【オプションについて】 ===
%% オプションを指定すると検証のために与えたパラメータと結果に返るパラメータのデータ型が異なることがある.<br />
%% しかし, 入出力でデータ型が異なっていた場合も返り値のパラメータはデータ型であることを保証する.<br />
%% オプションを複数指定した場合, 左からパラメータに適用されるため, 与えるリストの要素の順によって結果が変わる.<br />
-module(moyo_validator).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([validate/2, validate/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Types
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              spec/0,
              option/0,
              option_binary_in_output_type/0,
              option_to_datetime_input_type/0,
              custom_spec_fun/0,
              transform_fun/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type spec() :: basic_type() | type_constraints()
              | {list, spec()} | {tuple, [spec()]} | {enum, [term()]} | {equal, term()} | {'or', [spec()]} | {custom, custom_spec_fun()}.
%% {list, spec()}: 入力値が`spec()'に適合する値を要素とするリストかをチェックする<br />
%% {tuple, [spec()]}: 入力値の各要素が`spec()`と適合するかチェックする. `spec()`は`tuple`の要素数用意する必要がある.<br />
%% {enum, [term()]}: 入力値が`[term()]'のいずれかの要素と等しいかどうかをチェックする<br />
%% {equal, term()}: 入力値が`term()'と等しいかどうかをチェックする<br />
%% {'or', [spec()]}: 入力値が`[spec()]'のいずれかの条件に適合するかをチェックする<br />
%% {custom, custom_spec_fun()}: 入力値が`custom_spec_fun()'で指定の条件に適合するかどうかをチェックする<br />

-type type_constraints() ::
          {integer, [integer_constraint()]} |
          {float,   [float_constraint()]}   |
          {number,  [number_constraint()]}  |
          {string,  [string_constraint()]}  |
          {binary,  [binary_constraint()]}  |
          {datetime,[datetime_constraint()]}|
          {boolean, []}                     |
          {atom,    []}                     |
          {any,     []}.

-type basic_type() :: integer | float | number | string | binary | boolean | atom | datetime | any.
-type type()       :: basic_type() | list | tuple | enum | equal | custom.

-type constraint()         :: integer_constraint() | float_constraint() |
                              string_constraint() | binary_constraint().
-type integer_constraint() :: sign_constraint() | even | odd
                            | {range, min(), max()} | {more, integer()} | {less, integer()}
                            | {sign(), number_of_bits()}.
-type float_constraint()   :: sign_constraint() | {range, min_number(), max_number()} |
                              {more, number()} | {less, number()}.
-type number_constraint()  :: sign_constraint() | {range, min_number(), max_number()} |
                              {more, number()} | {less, number()}.
-type string_constraint()  :: {max_length, integer()} | {regexp, string()} | ascii | not_empty.
-type binary_constraint()  :: {max_length, integer()} | {regexp, binary()} | ascii | not_empty.
-type datetime_constraint():: {range, calendar:datetime(), calendar:datetime()} | {equal, calendar:datetime()} |
                              {more, calendar:datetime()} | {less, calendar:datetime()}.

-type sign_constraint() :: positive | negative | non_negative.

-type min()            :: integer().
-type max()            :: integer().
-type min_number()     :: number().
-type max_number()     :: number().
-type sign()           :: signed | unsigned.
-type number_of_bits() :: pos_integer().

-type option() :: binary_in | {binary_in, option_binary_in_output_type()} | int_to_bool |
                  {transform, transform_fun()} | allow_integer |
                  to_datetime | {to_datetime, option_to_datetime_input_type()}.

-type option_binary_in_output_type()  :: basic_type() | existing_atom.
-type option_to_datetime_input_type() :: iso8601 | unixtime.

-type custom_spec_fun() :: fun ((InputValue::term()) -> boolean()).
-type transform_fun()   :: fun ((InputValue::term()) -> TransformedValue::term()).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(TRANSFORM_FROM_BINARY(ToType, TransformFun, Binary),
       try
           {ok, TransformFun(Binary)}
       catch
           _:_ -> {error, {cannot_transform_from_binary, {type, ToType}, {binary, Binary}}}
       end).

-define(TRANSFORM(ToType, TransformFun, Value),
       try
           {ok, TransformFun(Value)}
       catch
           _:_ -> {error, {cannot_transform, {type, ToType}, {value, Value}}}
       end).

%%----------------------------------------------------------------------------------------------------------------------
%% Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc データ型, 制限を指定して, パラメータがその条件に一致するかを検証する.
%%
%% データ型だけではなく, 値等に制限を掛けて検査することができる.
%%
%% === 制限一覧 ===
%% ```
%% ● integer
%%     ○ positive
%%         パラメータが負数, または, 0の時にerror.
%%     ○ negative
%%         パラメータが正数, または, 0の時にerror.
%%     ○ non_negative
%%         パラメータが負の数の場合error.
%%     ○ even
%%         パラメータが奇数の場合error.
%%     ○ odd
%%         パラメータが偶数の場合error.
%%     ○ {range, min(), max()}
%%         パラメータがmin()からmax()の間に含まれない場合error.
%%         パラメータがmin(), max()と同じ値の場合はokを返す.
%%     ○ {more, integer()}
%%         パラメータがinteger()以下の場合error.
%%     ○ {less, integer()}
%%         パラメータがinteger()以上の場合error.
%%     ○ {signed | unsigned, Bits}
%%         `Bits' ビットの符号あり/なしで表現できる数値の範囲外の場合error.
%% ● float
%%     ○ positive
%%         パラメータが負数, または, 0の時にerror.
%%     ○ negative
%%         パラメータが正数, または, 0の時にerror.
%%     ○ non_negative
%%         パラメータが負の数の場合error.
%%     ○ {range, min_number(), max_number()}
%%         パラメータがmin_number()からmax_number()の間に含まれない場合error.
%%         パラメータがmin_number(), max_number()と同じ値の場合はokを返す.
%%     ○ {more, integer()}
%%         パラメータがinteger()以下の場合error.
%%     ○ {less, integer()}
%%         パラメータがinteger()以上の場合error.
%% ●number
%%     ○ positive
%%         パラメータが負数, または, 0の時にerror.
%%     ○ negative
%%         パラメータが正数, または, 0の時にerror.
%%     ○ non_negative
%%         パラメータが負の数の場合error.
%%     ○ {range, min_number(), max_number()}
%%         パラメータがmin_number()からmax_number()の間に含まれない場合error.
%%         パラメータがmin_number(), max_number()と同じ値の場合はokを返す.
%%     ○ {more, integer()}
%%         パラメータがinteger()以下の場合error.
%%     ○ {less, integer()}
%%         パラメータがinteger()以上の場合error.
%% ● string, binary
%%     ○ {max_length, integer()}
%%         パラメータの長さがinteger()より大きい場合error
%%     ○ {regexp, string() | binary()}
%%         パラメータが正規表現string() | binary()にマッチしない場合はerror
%%     ○ascii
%%         パラメータがASCII文字列かどうか
%%     ○not_empty
%%         パラメータが空文字じゃないかどうか
%% ●datetime
%%     ◯{more,calendar:datetime(}
%%         パラメータが値より大きいかをチェックする
%%     ◯{less,calendar:datetime()}
%%         パラメータが値より小さいかをチェックする
%%     ◯{range calendar:datetime(), calendar:datetime()}
%%         パラメータがMin以上Max以下であるかをチェックする
%%     ◯{equal,calendar:datetime()}
%% '''
-spec validate(InputValue, Spec) -> {ok, OutputValue} | {error, Reason} when
          InputValue  :: term(),
          Spec        :: spec(),
          OutputValue :: term(),
          Reason      :: term().
validate(Value, Spec) -> validate(Value, Spec, []).

%% @doc データ型, 制限を指定して, パラメータがその条件に一致するかを検証する.
%%
%% オプションを指定することができる(複数指定可能).
%%
%% === オプション一覧 ===
%% ```
%% ● binary_in
%%     バイナリタイプのパラメータのみ受け付ける.
%%     出力時はバリデートするタイプに変換される.
%%     ※ 出力タイプが`atom'の場合は、入力に対応するアトムが既にあるかどうかに関わらず、<br />
%%       強制的にアトムが生成されるのでメモリリークを防ぐため、代わりに`{binary_in, existing_atom}'を使うことを推奨
%%
%%     また, バリデーション対象の型は通さない.
%%     例として, integerのバリデーションだとしても, integer型のパラメータは通さない.
%% ● {binary_in, option_binary_in_output_type()}
%%     バイナリタイプのパラメータのみ受け付ける.
%%     出力時は`option_binary_in_output_type()'で指定されたタイプに変換される.
%% ● int_to_bool
%%     1, 0をbooleanとして扱う. 1がtrue, 0がfalse.
%%     このオプションを指定した時, true, falseは通さない.
%% ● {transform, fun()}
%%     fun()に任意の関数を指定できる. このオプションを指定した場合,
%%     パラメータをこの関数に通してからバリデーションを行う.
%% ● allow_integer
%%     typeとしてfloatを指定している時にintegerが来た場合でも許可する.
%% ● to_datetime
%%     datetime型に変換する.変換元はunixtime, iso8601が可能.
%% ● {to_datetime, option_to_datetime_input_type()}
%%     入力形式を指定した型のみに制限する.
%% '''
%%
%% 型をdatetimeに指定した時のbinary_inの詳細な動作は以下の表にまとめる。（リストに入れる順番はbinary_inが先である必要がある)
%%
%% |                         | `<<"12345">>' | `<<"2014-07-01T01:02:03">>' |
%% |:------------------------|--------------:|----------------------------:|
%% |(併記なし)               | error         | error                       |
%% |`to_datetime'            | ok            | ok                          |
%% |`{to_datetime, unixtime}'| ok            | error                       |
%% |`{to_datetime, iso8601}' | error         | ok                          |

-spec validate(InputValue, Spec, [Option]) -> {ok, OutputValue} | {error, Reason} when
          InputValue  :: term(),
          Spec        :: spec(),
          Option      :: option(),
          OutputValue :: term(),
          Reason      :: term().
validate(Value, {Type, _} = Spec, Options) ->
    case apply_options(Value, Type, Options) of
        {error, _} = Result -> Result;
        {ok, Value2}        ->
            case validate_impl(Value2, Spec) of
                ok    -> {ok, Value2};
                Other -> Other
            end
    end;
validate(Value, Type, Options) ->
    validate(Value, {Type, []}, Options).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% オプションに対する処理を実行.
-spec apply_options(InputValue, Type, [Option]) -> {ok, OutputValue} | {error, Result} when
          InputValue  :: term(),
          Type        :: type(),
          Option      :: option(),
          OutputValue :: term(),
          Result      :: term().
apply_options(Value, Type, [Option | RemainingOptions]) ->
    Result =
        case is_next_type_necessity(Option) of
            true  ->
                NextType = case RemainingOptions of
                               []            -> Type;
                               [NextOpt | _] -> get_input_type(NextOpt)
                           end,
                apply_options_impl(Value, NextType, Option);
            false ->
                apply_options_impl(Value, Option)
        end,
    case Result of
        {ok, ApplyedValue}  -> apply_options(ApplyedValue, Type, RemainingOptions);
        {error, _} = Result -> Result
    end;
apply_options(Value, _Type, []) ->
    {ok, Value}.

%% 各オプションが入力として必要とするパラメータのデータ型を返す.
-spec get_input_type(Option::option()) -> Type::type() | undefined.
get_input_type(binary_in)               -> binary;
get_input_type({binary_in, _})          -> binary;
get_input_type(int_to_bool)             -> integer;
get_input_type({transform, _})          -> undefined;
get_input_type(to_datetime)             -> datetime;
get_input_type({to_datetime, unixtime}) -> integer;
get_input_type({to_datetime, _})        -> binary;
get_input_type(_Option)                 -> undefined.

%% 各オプション適用後のデータ型を把握する必要があるかどうかを返す.
-spec is_next_type_necessity(Option::option()) -> Result::boolean().
is_next_type_necessity(binary_in)        -> true;
is_next_type_necessity({binary_in, _})   -> false;
is_next_type_necessity(int_to_bool)      -> false;
is_next_type_necessity({transform, _})   -> false;
is_next_type_necessity(allow_integer)    -> false;
is_next_type_necessity(to_datetime)      -> false;
is_next_type_necessity({to_datetime, _}) -> false;
is_next_type_necessity(_Option)          -> false.

%% 各オプションごとの実際の処理を実行. (次のデータ型の指定が必要なオプション)
%% is_next_type_necessity がtrueの物をここで定義する
-spec apply_options_impl(InputValue, NextType, Option) -> {ok, OutputValue} | {error, Reason} when
          InputValue  :: term(),
          NextType    :: type(),
          Option      :: option(),
          OutputValue :: term(),
          Reason      :: term().
apply_options_impl(Value, NextType, binary_in) when is_binary(Value) ->
    apply_binary_in(Value, NextType);
apply_options_impl(Value, NextType, Option) ->
    {error, {cannot_use_option, {value, Value}, {next_type, NextType}, {option, Option}}}.

%% to_datetimeオプション指定時の入力データ型毎の処理.
-spec apply_to_datetime(InputValue, Type) -> {ok, OutputValue} | {error, Reason} when
      InputValue  :: term(),
      Type        :: unixtime | iso8601,
      OutputValue :: term(),
      Reason      :: term().
apply_to_datetime(InputValue, unixtime) ->
    try_unixtime_to_datetime(InputValue);
apply_to_datetime(InputValue, iso8601) ->
    try_iso8601_to_datetime(InputValue).

%% binary_inオプション指定時の出力データ型毎の処理.
-spec apply_binary_in(InputValue, Type) -> {ok, OutputValue} | {error, Reason} when
      InputValue  :: term(),
      Type        :: basic_type(),
      OutputValue :: term(),
      Reason      :: term().
apply_binary_in(Value,  integer      ) -> try_binary_to_integer(Value);
apply_binary_in(Value,  float        ) -> try_binary_to_float(Value);
apply_binary_in(Value,  number       ) -> try_binary_to_number(Value);
apply_binary_in(Value,  string       ) -> {ok, binary_to_list(Value)};
apply_binary_in(Value,  binary       ) -> {ok, Value};
apply_binary_in(Value,  atom         ) -> try_binary_to_atom(Value);
apply_binary_in(Value,  existing_atom) -> try_binary_to_existing_atom(Value);
apply_binary_in(Value,  boolean      ) -> try_binary_to_existing_atom(Value);
apply_binary_in(Value,  datetime     ) -> try_binary_to_various_datetime(Value);
apply_binary_in(_Value, Type         ) ->
    {error, {cannot_use_option, {option, binary_in}, {next_type, Type}}}.

%% 各オプションごとの実際の処理を実行. (次のデータ型の指定が不必要なオプション)
-spec apply_options_impl(InputValue, Option) -> {ok, OutputValue} | {error, Reason} when
          InputValue  :: term(),
          Option      :: option(),
          OutputValue :: term(),
          Reason      :: term().
apply_options_impl(Value, {binary_in, NextType}) ->
    apply_binary_in(Value, NextType);
apply_options_impl(Value, int_to_bool = Option) ->
    case Value of
        1 -> {ok, true};
        0 -> {ok, false};
        _ -> {error, {illigal_value, {value, Value}, {option, Option}}}
    end;
apply_options_impl(Value, {transform, Fun}) ->
    try Fun(Value) of
        Result -> {ok, Result}
    catch
        ErrorType:Reason ->
            {error, {transform_error,
                    {function, Fun}, {value, Value}, {error_type, ErrorType}, {reason, Reason}}}
    end;
apply_options_impl(Value, allow_integer) -> ?TRANSFORM(float, fun float/1, Value);
apply_options_impl(Value, to_datetime) when is_integer(Value) ->
    apply_to_datetime(Value, unixtime);
apply_options_impl({{_,_,_},{_,_,_}} = DateTime, to_datetime) ->
    moyo_cond:conditional(moyo_clock:is_valid_datetime(DateTime),
                          {ok, DateTime},
                          {error, {cannot_transform, {type, datetime}, {value, DateTime}}});
apply_options_impl(Value, to_datetime) ->
    apply_to_datetime(Value, iso8601);
apply_options_impl(Value, {to_datetime, Type}) ->
    apply_to_datetime(Value, Type);
apply_options_impl(_Value, Option) -> {error, {illigal_option, {option, Option}}}.

%% バリデーションの実際の処理.
-spec validate_impl(InputValue, Spec) -> ok | {error, Reason} when
          InputValue  :: term(),
          Spec        :: spec(),
          Reason      :: term().
validate_impl(ValueList, {list, ElemSpec} = Spec) ->
    case is_list(ValueList) of
        false -> {error, {not_list, {value, ValueList}, {spec, Spec}}};
        true  ->
            ResultList = lists:map(fun(Value) -> validate(Value, ElemSpec) end, ValueList),
            case proplists:get_value(error, ResultList) of
                undefined -> ok;
                Reason    -> {error, Reason}
            end
    end;
validate_impl(ValueTuple, {tuple, SpecList} = Spec) ->
    case is_tuple(ValueTuple) of
        false ->
            {error, {not_tuple, {value, ValueTuple}, {spec, Spec}}};
        true when not is_list(SpecList) ->
            {error, {not_supported_spec, {value, ValueTuple}, {spec, Spec}}};
        true ->
            Fun = fun(_, [], []) -> ok;
                     (F, [Value | ValueRest], [ElemSpec | ElemRest]) ->
                         case validate(Value, ElemSpec) of
                             {ok, _}         -> F(F, ValueRest, ElemRest);
                             {error, Reason} -> {error, Reason}
                         end;
                     (_, _, _) -> {error, incorrect_spec_size}
                  end,
            Fun(Fun, tuple_to_list(ValueTuple), SpecList)
    end;
validate_impl(Value, {enum, Items}) ->
    case lists:member(Value, Items) of
        true  -> ok;
        false -> {error, {no_match_items, {items, Items}, {value, Value}}}
    end;
validate_impl(Value, {equal, Expected}) ->
    case Value =:= Expected of
        true  -> ok;
        false -> {error, {not_equal, {value, Value}, {expected, Expected}}}
    end;
validate_impl(Value, {'or', Specs}) ->
    case lists:any(fun ({_, _} = Spec) -> ok =:= validate_impl(Value, Spec);
                       (Spec)          -> ok =:= validate_impl(Value, {Spec, []}) end, Specs) of
        true  -> ok;
        false -> {error, {all_validation_failed, {value, Value}, {specs, Specs}}}
    end;
validate_impl(Value, {custom, CustomFun}) ->
    try CustomFun(Value) of
        true  -> ok;
        false -> {error, {return_false, {function, CustomFun}, {value, Value}}}
    catch
        ErrorType:Reason ->
            {error, {custom_type_error,
                    {function, CustomFun}, {value, Value}, {error_type, ErrorType}, {reason, Reason}}}
    end;
validate_impl(Value, {Type, Constraints}) ->
    case check_type(Type, Value) of
        {error, _}  = Result -> Result;
        ok ->
            check_constraints(Value, Constraints, Type)
    end.

%% パラメータとデータ型が一致するかを検査.
-spec check_type(Type, InputValue) -> ok | {error, Reason} when
          Type        :: basic_type(),
          InputValue  :: term(),
          Reason      :: term().
check_type(Type, Value) ->
    case check_type_impl(Type, Value) of
        {ok, true}          -> ok;
        {ok, false}         -> {error, {not_match, {expected_type, Type}, {value, Value}}};
        {error, _} = Result -> Result
    end.

%% データ型毎のデータ型検査処理.
-spec check_type_impl(Type, Value) -> {ok, Boolean} | {error, Reason} when
          Type    :: basic_type(),
          Value   :: term(),
          Boolean :: boolean(),
          Reason  :: term().
check_type_impl(integer, Value) -> {ok, is_integer(Value)};
check_type_impl(float,   Value) -> {ok, is_float(Value)};
check_type_impl(number,  Value) -> {ok, is_number(Value)};
check_type_impl(string,  Value) -> {ok, is_list(Value)};
check_type_impl(binary,  Value) -> {ok, is_binary(Value)};
check_type_impl(boolean, Value) -> {ok, is_boolean(Value)};
check_type_impl(atom,    Value) -> {ok, is_atom(Value)};
check_type_impl(any,    _Value) -> {ok, true};
check_type_impl(datetime,{{_,_,_},{_,_,_}} = Value) ->
    {ok, moyo_clock:is_valid_datetime(Value)};
check_type_impl(Type,   _Value) -> {error, {bad_type, {type, Type}}}.

%% 制限を検査.
-spec check_constraints(InputValue, [Constraint], Type) -> ok | {error, Reason} when
          InputValue  :: term(),
          Constraint  :: constraint(),
          Type        :: basic_type(),
          Reason      :: term().
check_constraints(Value, Constraints, Type) ->
    moyo_list:maybe_foreach(
      fun (Constraint) ->
              case check_constraints_impl(Value, Constraint, Type) of
                  {ok, true}          -> ok;
                  {ok, false}         -> {error, {not_match,
                                                  {expected_constraint, Constraint},
                                                  {value, Value}}};
                  {error, _} = Result -> Result
              end
      end,
      Constraints).

%% 各制限に対する実際の処理.
-spec check_constraints_impl(Value, Constraint, Type) -> {ok, Boolean} | {error, Reason} when
          Value      :: term(),
          Constraint :: constraint(),
          Type       :: basic_type(),
          Boolean    :: boolean(),
          Reason     :: term().
%% integer に対するconstraint
check_constraints_impl(Value, positive,          integer) -> {ok, Value >  0};
check_constraints_impl(Value, negative,          integer) -> {ok, Value <  0};
check_constraints_impl(Value, non_negative,      integer) -> {ok, Value >= 0};
check_constraints_impl(Value, even,              integer) -> {ok, Value rem 2 =:= 0};
check_constraints_impl(Value, odd,               integer) -> {ok, Value rem 2 =:= 1};
check_constraints_impl(Value, {range, Min, Max}, integer) ->
    {ok, (Min =< Value) andalso (Value =< Max)};
check_constraints_impl(Value, {more, Threshold}, integer) -> {ok, Value > Threshold};
check_constraints_impl(Value, {less, Threshold}, integer) -> {ok, Value < Threshold};
check_constraints_impl(Value, {signed,   Bits},  integer) when Bits > 0 ->
    {ok, -(1 bsl (Bits - 1)) =< Value andalso  Value < (1 bsl (Bits - 1))};
check_constraints_impl(Value, {unsigned, Bits},  integer) when Bits > 0 ->
    {ok, 0 =< Value andalso Value < 1 bsl Bits};
%% float に対するconstraint
check_constraints_impl(Value, positive,          float) -> {ok, Value >  0.0};
check_constraints_impl(Value, negative,          float) -> {ok, Value <  0.0};
check_constraints_impl(Value, non_negative,      float) -> {ok, Value >= 0.0};
check_constraints_impl(Value, {range, Min, Max}, float) ->
    {ok, (Min =< Value) andalso (Value =< Max)};
check_constraints_impl(Value, {more, Threshold}, float) -> {ok, Value > Threshold};
check_constraints_impl(Value, {less, Threshold}, float) -> {ok, Value < Threshold};
%% number に対する constraint
check_constraints_impl(Value, positive,          number) -> {ok, Value >  0.0};
check_constraints_impl(Value, negative,          number) -> {ok, Value <  0.0};
check_constraints_impl(Value, non_negative,      number) -> {ok, Value >= 0.0};
check_constraints_impl(Value, {range, Min, Max}, number) ->
    {ok, (Min =< Value) andalso (Value =< Max)};
check_constraints_impl(Value, {more, Threshold}, number) -> {ok, Value > Threshold};
check_constraints_impl(Value, {less, Threshold}, number) -> {ok, Value < Threshold};
%% string に対するconstraint
check_constraints_impl(Value, {max_length, Len}, string) -> {ok, length(Value) =< Len};
check_constraints_impl(Value, {regexp, Pattern}, string) ->
    {ok, re:run(Value, Pattern, [{capture, none}]) =:= match};
check_constraints_impl(Value, ascii,             string) ->
    {ok, moyo_string:is_ascii_string(Value)};
check_constraints_impl("", not_empty, string) ->
    {ok, false};
check_constraints_impl(_, not_empty, string) ->
    {ok, true};
%% binary に対するconstraint
check_constraints_impl(Value, {max_length, Len}, binary) -> {ok, byte_size(Value) =< Len};
check_constraints_impl(Value, {regexp, Pattern}, binary) ->
    {ok, re:run(Value, Pattern, [{capture, none}]) =:= match};
check_constraints_impl(Value, ascii,             binary) ->
    {ok, moyo_string:is_ascii_string(binary_to_list(Value))};
check_constraints_impl(<<"">>, not_empty, binary) ->
    {ok, false};
check_constraints_impl(_, not_empty, binary) ->
    {ok, true};
%% date に対するconstraint
check_constraints_impl(Value, {range, Min, Max}, datetime) ->
    {ok, (moyo_clock:datetime_diff(Value, Min) >= 0) andalso (moyo_clock:datetime_diff(Max, Value) >= 0)};
check_constraints_impl(Value, {more, Min}, datetime) ->
    {ok, moyo_clock:datetime_diff(Value, Min) > 0};
check_constraints_impl(Value, {less, Max}, datetime) ->
    {ok, moyo_clock:datetime_diff(Max, Value) > 0};
check_constraints_impl(Value, {equal, Equal}, datetime) ->
    {ok, moyo_clock:datetime_diff(Value, Equal) == 0};
%% エラー処理
check_constraints_impl(_Value, Constraint, Type) ->
    {error, {{constraint, Constraint}, {type, Type}}}.


-spec try_binary_to_integer(binary()) -> {ok, integer()} | {error, Reason::term()}.
try_binary_to_integer(Bin) ->
    ?TRANSFORM_FROM_BINARY(integer, fun erlang:binary_to_integer/1, Bin).

-spec try_binary_to_float(binary()) -> {ok, float()} | {error, Reason::term()}.
try_binary_to_float(Bin) ->
    ?TRANSFORM_FROM_BINARY(float, fun erlang:binary_to_float/1, Bin).

-spec try_binary_to_number(binary()) -> {ok, number()} | {error, Reason::term()}.
try_binary_to_number(Bin) ->
    ?TRANSFORM_FROM_BINARY(float,
                           fun (Bin1) ->
                                   try binary_to_integer(Bin1) of
                                       Integer -> Integer
                                   catch
                                       error:badarg -> binary_to_float(Bin1)
                                   end
                           end,
                           Bin).

-spec try_binary_to_existing_atom(binary()) -> {ok, atom()} | {error, Reason::term()}.
try_binary_to_existing_atom(Bin) ->
    ?TRANSFORM_FROM_BINARY(existing_atom, fun (_) -> binary_to_existing_atom(Bin, utf8) end, Bin).

-spec try_binary_to_atom(binary()) -> {ok, atom()} | {error, Reason::term()}.
try_binary_to_atom(Bin) ->
    ?TRANSFORM_FROM_BINARY(atom, fun (_) -> binary_to_atom(Bin, utf8) end, Bin).

%% datetimeの場合はbinaryでもdatetime形式の場合がある為、ここではOKで全て返す
-spec try_binary_to_various_datetime(binary()) -> {ok, non_neg_integer()}.
try_binary_to_various_datetime(Bin) ->
    try {ok, binary_to_integer(Bin)} catch _:_ -> {ok, Bin} end.

-spec try_unixtime_to_datetime(non_neg_integer()) -> {ok, calendar:datetime()} | {error, Reason::term()}.
try_unixtime_to_datetime(Int) when is_integer(Int), Int > 0 ->
    {ok, moyo_clock:seconds_to_datetime(Int)};
try_unixtime_to_datetime(Int) ->
    {error, {cannot_transform, {type, datetime}, {value, Int}}}.

-spec try_iso8601_to_datetime(binary()) -> {ok, calendar:datetime()} | {error, Reason::term()}.
try_iso8601_to_datetime(Bin) ->
    ?TRANSFORM(datetime, fun moyo_clock:iso8601_to_datetime/1, Bin).
