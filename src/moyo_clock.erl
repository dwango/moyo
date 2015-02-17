%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc 日付や時間に関する処理を集めたユーティリティモジュール.
%%
-module(moyo_clock).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         now/0,
         now_seconds/0,
         seconds_to_datetime/1,
         seconds_to_now/1,
         datetime_to_seconds/1,
         datetime_to_now/1,
         datetime_diff/2,
         datetime_diff/1,
         datetime_add/2,
         now_format/1,
         now_format/2,
         datetime_format/2,
         now_unix_time_in_float/0,
         datetime_to_iso8601ext/1,
         iso8601ext_to_datetime/1,
         iso8601_to_datetime/1,
         is_date/1,
         is_date/2,
         parse_iso8601/1,
         parse_iso8601_date/1,
         parse_iso8601_time/1,
         parse_iso8601_timezone/1,
         is_valid_datetime/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Types
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              datetime/0,
              seconds/0,
              milliseconds/0,
              pos_seconds/0,
              pos_milliseconds/0,
              non_neg_seconds/0,
              non_neg_milliseconds/0,
              unix_timestamp/0,
              timezone/0,
              iso8601type/0,
              iso8601datetype/0,
              iso8601timetype/0,
              iso8601zonetype/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type datetime()             :: calendar:datetime().

-type seconds()              :: integer().
-type milliseconds()         :: integer().
-type pos_seconds()          :: pos_integer().
-type pos_milliseconds()     :: pos_integer().
-type non_neg_seconds()      :: non_neg_integer().
-type non_neg_milliseconds() :: non_neg_integer().

-type unix_timestamp()       :: non_neg_seconds().  % 1970-01-01T00:00:00Z からの経過秒数

-type data_in_format() :: {now, erlang:timestamp()} | {datetime, calendar:datetime()}.

-type timezone()    :: {Sign:: 1 | -1, calendar:time()}.
-type iso8601type() :: iso8601datetype() | {iso8601datetype(), iso8601timetype()} |
                       {iso8601datetype(), iso8601timetype(), iso8601zonetype()}.
-type iso8601datetype() :: yyyy | yyyy_mm | yyyy_mm_dd | yyyy_ddd | yyyy_Www_d | yyyymmdd | yyyyWwwd | yyyyddd.
-type iso8601timetype() :: hh | hhmm | hhmmss | hh_mm | hh_mm_ss |
                           hh_s | hhmm_s | hh_mm_s | hhmmss_s | hh_mm_ss_s.
-type iso8601zonetype() :: z | hh_mm | hhmm | hh.

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(UNIX_TIMESTAMP_EPOCH, {{1970, 1, 1}, {0, 0, 0}}).

-define(IS_VALID_HMS(Hour, Min, Sec),
        is_integer(Hour) andalso is_integer(Min) andalso is_integer(Sec)
        andalso (Hour >= 0) andalso (Hour < 24) andalso (Min >= 0)
        andalso (Min < 60) andalso (Sec >= 0) andalso (Sec < 60)
       ).

-define(IS_VALID_DATE(Year, Month, Day),
        is_integer(Year) andalso is_integer(Month) andalso is_integer(Day)
        andalso Year > 0 andalso Month > 0 andalso Month =< 12 andalso Day > 0
        andalso Day  =< calendar:last_day_of_the_month(Year, Month)
       ).

-define(IS_DIGIT(X),
        (X >= $0) andalso (X =< $9)
       ).

-define(COND(Flag, TValue, FValue),
        case Flag of true -> TValue; false -> FValue end
       ).

-define(BIN_LIST_TO_INT_TUPLE(BinList),
        list_to_tuple([binary_to_integer(X) || X <- BinList])
       ).

-define(MATCH_DATE_OPTION(Type, TypeList),
        lists:any(fun(X) -> X =:= Type orelse X =:= any end, TypeList)
       ).

-define(THURSDAY, 4).

%%----------------------------------------------------------------------------------------------------------------------
%% Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 現在時刻を`erlang:timestamp()'形式で返す
%%
%% 動作的には{@link erlang:now/0}および{@link os:timestamp/0}と同様. <br />
%% ユニットテストでモックを行いやすくすることができるのがこの関数の存在意義.
-spec now() -> erlang:timestamp().
now() ->
    os:timestamp().

%% @doc UNIXタイムスタンプ形式の現在時間(ローカル時刻)を取得する.
-spec now_seconds() -> unix_timestamp().
now_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) - unix_timestamp_epoch_local().

%% @doc UNIXタイムスタンプ形式の日時を`datetime()'形式に変換する.
-spec seconds_to_datetime(unix_timestamp()) -> datetime().
seconds_to_datetime(Seconds) when is_integer(Seconds) andalso Seconds >= 0 ->
    calendar:gregorian_seconds_to_datetime(Seconds + unix_timestamp_epoch_local()).

%% @doc UNIXタイムスタンプ形式の日時を`erlang:timestamp()'形式に変換する.
-spec seconds_to_now(unix_timestamp()) -> erlang:timestamp().
seconds_to_now(Seconds) when is_integer(Seconds) andalso Seconds >= 0 ->
    {Seconds div (1000 * 1000), Seconds rem (1000 * 1000), 0}.

%% @doc `datetime()'形式の日時をUNIXタイプスタンプ形式の数値に変換する.
%%
%% 変換可能な範囲外の日時が渡された場合は、例外が送出される
-spec datetime_to_seconds(datetime()) -> unix_timestamp().
datetime_to_seconds(DateTime) ->
    case calendar:datetime_to_gregorian_seconds(DateTime) - unix_timestamp_epoch_local() of
        Seconds when Seconds < 0 -> error({too_old_datetime, DateTime});
        Seconds                  -> Seconds
    end.

%% @doc 二つの日時の差を求める.
%%
%% `DateTime1' - `DateTime2' = 秒数
-spec datetime_diff(datetime(), datetime()) -> seconds().
datetime_diff(DateTime1, DateTime2) ->
    calendar:datetime_to_gregorian_seconds(DateTime1) - calendar:datetime_to_gregorian_seconds(DateTime2).

%% @doc 引数の日時と現在時刻の差を求める.
%%
%% `Datetime' - 現在時刻 = 秒数
-spec datetime_diff(datetime()) -> seconds().
datetime_diff(DateTime) ->
    datetime_diff(DateTime, calendar:local_time()).

%% @doc 引数の日時に指定秒数を加算する.
-spec datetime_add(datetime(), seconds()) -> datetime().
datetime_add(DateTime, Seconds) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(DateTime) + Seconds).

%% @equiv now_format(Format, now())
-spec now_format(binary()) -> binary().
now_format(Format) ->
    now_format(Format, erlang:now()).

%% @doc 日付/時刻を書式化する.
%%
%% 以下の文字が `format' パラメータ文字列として認識されます.<br />
%% 参考: php date関数<br />
%%       <a href="http://php.net/manual/ja/function.date.php#refsect1-function.date-parameters">
%%       http://php.net/manual/ja/function.date.php#refsect1-function.date-parameters
%%       </a>
%% <table class="line">
%%   <tr><td class="line">Y</td><td class="line">年.4桁の数字.</td></tr>
%%   <tr><td class="line">m</td><td class="line">月.数字.先頭にゼロをつける.</td></tr>
%%   <tr><td class="line">d</td><td class="line">日.二桁の数字(先頭にゼロが付く場合も)</td></tr>
%%   <tr><td class="line">H</td><td class="line">時.24時間単位.</td></tr>
%%   <tr><td class="line">i</td><td class="line">分.先頭にゼロをつける.</td></tr>
%%   <tr><td class="line">s</td><td class="line">秒.先頭にゼロをつける.</td></tr>
%%   <tr><td class="line">p</td><td class="line">ミリ秒.3桁の数字.先頭にゼロをつける.</td></tr>
%% </table>
%%
%% ex:
%% ```
%% 1> moyo_clock:now_format(<<"Y/m/d-H:i:s(p)">>, now()).
%% <<"2013/10/09-11:59:32(131)">>
%% '''
%%
%% formatパラメータ文字列にあたる文字をそのまま表示したい場合は, `\\' でエスケープできる.
%% ( `\\' の後ろの1文字はどの文字でもそのまま表示される. `\' も例外ではない.)
%%
%% ex:
%% ```
%% 2> moyo_clock:now_format(<<"\\Year: Y\\\\">>, now()).
%% <<"Year: 2013\\">>
%% '''
-spec now_format(binary(), erlang:timestamp()) -> binary().
now_format(Format, Now) ->
    LocalTime = calendar:now_to_local_time(Now),
    now_format_impl(Format, [], [{now, Now}, {datetime, LocalTime}]).

%% @equiv now_format(Format, datetime_to_now(DateTime))
-spec datetime_format(binary(), calendar:datetime()) -> binary().
datetime_format(Format, DateTime) ->
    now_format(Format, datetime_to_now(DateTime)).

%% @doc `calendar:datetime()'形式の日時を`erlang:timestamp()'形式に変換する.
-spec datetime_to_now(calendar:datetime()) -> erlang:timestamp().
datetime_to_now(DateTime) ->
    seconds_to_now(datetime_to_seconds(DateTime)).

%% @doc UNIX Time をfloatで返す
-spec now_unix_time_in_float() -> float().
now_unix_time_in_float() ->
  {MegaSec, Sec, MicroSec} = os:timestamp(),
  MegaSec * 1000000 + Sec + MicroSec / 1000000.

%% @doc `datetime()'型のローカル時刻をISO8601の拡張表記の日付文字列(バイナリ)に変換する
%%
%% ```
%% > datetime_to_iso8601ext({{2014,4,20}, {9,9,9}}).
%% <<"2014-04-20T09:09:09+09:00">>
%% '''
-spec datetime_to_iso8601ext(calendar:datetime()) -> binary().
datetime_to_iso8601ext(DataTime = {{Y, Mo, D}, {H, Mi, S}}) ->
    %% 最初の結果を常に使うという雑な実装 (ロケールにJSTを仮定するなら問題ないはず)
    [UniversalTime | _] = calendar:local_time_to_universal_time_dst(DataTime),

    DiffSeconds = datetime_diff(DataTime, UniversalTime),
    Sign = moyo_cond:conditional(DiffSeconds < 0, "-", "+"),
    {DiffHour, DiffMinute, 0} = calendar:seconds_to_time(abs(DiffSeconds)),
    moyo_binary:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s~2..0B:~2..0B",
                       [Y, Mo, D, H, Mi, S, Sign, DiffHour, DiffMinute]);
datetime_to_iso8601ext(Value) ->
    error(badarg, [Value]).

%% @doc ISO8601の拡張表記の日付文字列をローカル時刻の`datetime()'型に変換する
%%
%% なお、現状許容している形式は'YYYY-MM-DDThh:mm:ss(Z|(+|-)hh:mm)'のみで、月や時間等の省略は不可。
%%
%% 不正な文字列が渡された場合は、エラーが送出される.
%%
%% ```
%% > iso8601ext_to_datetime(<<"2014-04-20T09:09:09+09:00">>).
%% {{2014,4,20}, {9,9,9}}
%% '''
-spec iso8601ext_to_datetime(binary()) -> calendar:datetime().
iso8601ext_to_datetime(<<Text/binary>>) ->
    case io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2d", binary_to_list(Text)) of
        {error, _}                                               -> error(badarg, [Text]);
        {ok, [Year, Month, Day, Hour, Minute, Second], TimeZone} ->
            DateTime0 = {{Year, Month, Day}, {Hour, Minute, Second}},
            DateTime1 = calendar:universal_time_to_local_time(DateTime0), % ローカル時刻で持ち回す
            case list_to_binary(TimeZone) of
                <<"Z">>                                     -> DateTime1;
                <<S:1/binary, H:2/binary, ":", M:2/binary>> ->
                    %% UTCからズレている分の時間を補正する
                    Offset = ((binary_to_integer(H) * 60 * 60) + (binary_to_integer(M) * 60)),
                    Sign = case S of
                               <<"+">> -> -1;
                               <<"-">> -> +1;
                               _       -> error(badarg, [Text])
                           end,
                    moyo_clock:datetime_add(DateTime1, Sign * Offset);
                _ ->
                    error(badarg, [Text])
            end
    end;
iso8601ext_to_datetime(Value) -> error(badarg, [Value]).

%% @doc iso8601を`datetime()'形式に変換する。
%%
%% iso8601のtimezoneがある場合はUTCに変換する。
%%
%% また、iso8601形式のbinary以外はerror(badarg, [Bin])が投げられる

-spec iso8601_to_datetime(binary()) -> datetime().

iso8601_to_datetime(Bin) when is_binary(Bin) ->
    case parse_iso8601(Bin) of
        {ok, {_, DateTime, {Sign, {H, M, S}}}} ->
            datetime_add(DateTime, Sign * -1 * (((H * 60) + M) * 60 + S));
        error ->
            error(badarg, [Bin])
    end;
iso8601_to_datetime(Input) -> error(badarg, [Input]).

%% @doc iso8601もしくはunixtime形式、date()、datetime()、time()であるかを判定する。
%%
%% 対応するiso8601の形式は{@link parse_iso8601/1}に準拠する。
%%
%% ```
%% > is_date(<<"2014-04-01T14-15-16Z">>).
%% true.
%% '''

-spec is_date(Value) -> true | false when
      Value :: binary() | datetime() | calendar:date() | calendar:time() | non_neg_integer().

is_date(Value) -> is_date(Value, [any]).

%% @doc Optionsに指定したiso8601形式のどれかであるかを判定する.
%%
%% iso8601形式のどれであるかを判定した後, Optionsの判定を行う為, 複数のOptionを指定することによってコストはそれほど増えない.
%% また, 日付部分は`yyyymmdd'時間部分は何でもよいといった指定はできない.
%%

-spec is_date(Value, Type) -> true | false when
      Value :: binary() | datetime() | calendar:date() | calendar:time() | non_neg_integer(),
      Type  :: [iso8601type() | date | datetime | time | unixtime | any].

is_date(<<Text/binary>>, TypeList) ->
    case parse_iso8601(Text) of
        {ok, {Type, _, _}} -> ?MATCH_DATE_OPTION(Type, TypeList);
        _ -> false
    end;
is_date(Int, TypeList) when is_integer(Int), Int >= 0 ->
    ?MATCH_DATE_OPTION(unixtime, TypeList);
is_date({{_,_,_},{_,_,_}} = DateTime, TypeList) ->
    is_valid_datetime(DateTime) andalso ?MATCH_DATE_OPTION(datetime, TypeList);
is_date({Y,M,D}, TypeList) ->
    case ?IS_VALID_DATE(Y,M,D) of
        true  -> ?MATCH_DATE_OPTION(date, TypeList);
        false -> ?IS_VALID_HMS(Y,M,D) andalso ?MATCH_DATE_OPTION(time, TypeList)
    end;
is_date(_, _) -> false.

%% @doc iso8601形式をパースし, 結果をタプルで返す。
%%
%% 正しい時間であることも保証する。
%%
%% パース可能な書式は{@link iso8601type/0}の通り.
%%
%% 20-- (2000年代)などの省略形式 や/を使用した期間表記には対応していない.
%%
%% 返り値`Decimal::number'の値域は[0,1) doubleとして表現される
%% 入力に小数点表記がない場合この値は0となる
%%
%% ```
%% > parse_iso8601(<<"2014-04-02T14:15:16Z">>).
%% {ok, {{{yyyymmdd, hhmmdd, Z}, {{2014, 4, 2}, {14, 15, 16}}, {1, {0, 0, 0}}}}
%% > parse_iso8601(<<"hoge">>).
%% error
%% '''

-spec parse_iso8601(binary()) -> {ok, {iso8601type(), datetime(), timezone()}} | error.

parse_iso8601(Bin) when is_binary(Bin) ->
    case parse_iso8601_date(Bin) of
        error ->
            error;
        {ok, {DateType, Date}, <<>>} ->
            {ok, {DateType, {Date, {0, 0, 0}}, {1, {0,0,0}} }};
        {ok, {DateType, Date}, <<"T", Rest0/binary>>} ->
            case parse_iso8601_time(Rest0) of
                {ok, {TimeType, Time}, <<>>} ->
                    {ok, {{DateType, TimeType}, {Date, Time}, {1, {0,0,0}} }};
                {ok, {TimeType, Time}, Rest1} ->
                    case parse_iso8601_timezone(Rest1) of
                        {ok, {ZoneType, TimeZone}, <<>>} ->
                            {ok, {{DateType, TimeType, ZoneType}, {Date, Time}, TimeZone}};
                        _ ->
                            error
                    end;
                error ->
                    error
            end;
        _ ->
            error
    end;
parse_iso8601(Bin) -> error(badarg, [Bin]).

%% @doc iso8601の日付部分のパースを提供する。
%%
%% 入力された時間の正しさも保証する。
%%
%% ```
%% > parse_iso8601_date(<<"2014-W01">>).
%% {ok, {yyyy, {2014, 1, 1}}, <<"-W01">>}
%% > parse_iso8601_date(<<"2014021234">>).
%% {ok, {yyyymmdd, {2014, 2, 12}}, <<"34">>}
%% > parse_iso8601_date(<<"20141321">>).
%% error
%% '''
%% 上記のように長く一致する形式として認識する。
%% ただし、3つ目の例のように、最長一致(2014-13-21)が正常な日付であった場合、
%% 部分一致(2014-132)が正常な日付であってもerrorとして判定する.
%% その為、意図した通りの動作を期待するのであれば、日付の後ろに記述されている文字が0-9であるべきではない.
%% また、入力が日付部分のみであることを期待している場合、下記を用いるべきである.
%%
%% ```
%% > {ok, _, <<>>} = parse_iso8691_date(<<"20140102">>).
%% {ok, {yyyymmdd, {2014, 1, 2}}, <<>>}
%% '''

-spec parse_iso8601_date(binary()) -> {ok, {iso8601datetype(), calendar:date()}, binary()} | error.

parse_iso8601_date(<<Input/binary>>) ->
    ParseResult = case parse_datetype_binary(Input) of
                      {error, _} ->
                          error;
                      {ok, {Type, {Y}, Rest}} when Type =:= yyyy ->
                          {ok, {Type, ?BIN_LIST_TO_INT_TUPLE([Y, <<$1>>, <<$1>>])}, Rest};
                      {ok, {Type, {Y, M}, Rest}} when Type =:= yyyy_mm ->
                          {ok, {Type, ?BIN_LIST_TO_INT_TUPLE([Y, M, <<$1>>])}, Rest};
                      {ok, {Type, {Y, D}, Rest}} when Type =:= yyyy_ddd; Type =:= yyyyddd ->
                          case countday_to_date(?BIN_LIST_TO_INT_TUPLE([Y, D])) of
                              {_, _, _} = Date -> {ok, {Type, Date}, Rest};
                              error            -> error
                          end;
                      {ok, {Type, {Y, M, D}, Rest}} when Type =:= yyyy_mm_dd ; Type =:= yyyymmdd ->
                          {ok, {Type, ?BIN_LIST_TO_INT_TUPLE([Y,M,D])}, Rest};
                      {ok, {Type, {Y, W, D}, Rest}} when Type =:= yyyy_Www_d ; Type =:= yyyyWwwd ->
                          case weekday_to_date(?BIN_LIST_TO_INT_TUPLE([Y,W,D])) of
                              error -> error;
                              Date  -> {ok, {Type, Date}, Rest}
                          end
                  end,
    case ParseResult of
        error ->
            error;
        {ok, {_, {Year, Month, Day}}, _} = Ok ->
            moyo_cond:conditional(?IS_VALID_DATE(Year, Month, Day), Ok, error)
    end;
parse_iso8601_date(Bin) -> error(badarg, [Bin]).

%% @doc iso8601形式の時刻部分のパースを提供する。
%%
%% 時間の正しさも保証される。
%%
%% 日付の変わり目の表現については、24:00:00は許容していない.
%% これは、`calendar:time()'型で許容されていない為である.
%%
%% また、小数点表記(hh:mm:ss.sなど)の小数は何桁でも許容されるが、
%% `calendar:time()'型で表現できない端数については切り捨てられる.

-spec parse_iso8601_time(binary()) -> {ok, {iso8601timetype(), calendar:time()}, binary()} | error.

parse_iso8601_time(<<Bin/binary>>) ->
    case parse_iso8601_time_sub(Bin, 0) of
        {[], _, _} -> error;
        {TypeStr, TimeList, TimeZone} ->
            {H, M, S} = Time = time_normalize(list_to_tuple(TimeList)),
            _ = [hh, hhmm, hhmmss, hh_mm, hh_mm_ss, hh_s, hhmm_s, hh_mm_s, hhmmss_s, hh_mm_ss_s], %% existing_atom対策
            Type = list_to_existing_atom(TypeStr),
            moyo_cond:conditional(?IS_VALID_HMS(H, M, S),
                                  {ok, {Type, Time}, TimeZone},
                                  error)
    end;
parse_iso8601_time(Bin) -> error(badarg, [Bin]).


%% @doc タイムゾーンをtime形式に変換する
%%
%% 対応形式は `Z', `+hh:mm', `-hh:mm', `+hh', `-hh', `+hhmm', `-hhmm'
%%
%% ```
%% > parse_iso8601_timezone(<<"hoge">>).
%% error
%% > parse_iso8601_timezone(<<"+09:00">>).
%% {ok, {hh_mm {1, {9, 0, 0}}}, <<>>}
%% '''

-spec parse_iso8601_timezone(binary()) -> {ok, {iso8601zonetype(), timezone()}, binary()} | error.

parse_iso8601_timezone(<<"Z", Rest/binary>>)  -> {ok, {z, {1, {0, 0, 0}}}, Rest};
parse_iso8601_timezone(<<Signed:8, Rest/binary>>) when Signed =:= $+ ; Signed =:= $- ->
    {Type, Hour, Min, Trush}
        = case Rest of
              <<H:2/binary, ":", M:2/binary, Rest0/binary>> -> {hh_mm, H, M, Rest0};
              <<H:2/binary, M:2/binary, Rest0/binary>> ->
                  case is_num_binary(M) of
                      true  -> {hhmm, H, M, Rest0};
                      false -> {hh, H, <<"0">>, <<M/binary, Rest0/binary>>}
                  end;
              <<H:2/binary, Rest0/binary>> ->
                  {hh, H, <<"0">>, Rest0};
              _  ->
                  {error, 0, 0, <<>>}
          end,
    if
        Type =:= error -> error;
        true ->
            case is_num_binary(<<Hour/binary, Min/binary>>) of
                true  ->
                    {HourI, MinI} = {binary_to_integer(Hour), binary_to_integer(Min)},
                    moyo_cond:conditional(?IS_VALID_HMS(HourI, MinI, 0),
                                          {ok, {Type, {moyo_cond:conditional(Signed =:= $+, 1, -1), {HourI, MinI, 0}}}, Trush},
                                          error
                                         );
                false ->
                    error
            end
    end;
parse_iso8601_timezone(Bin) when is_binary(Bin) -> error;
parse_iso8601_timezone(Bin) -> error(badarg, [Bin]).

%% @doc 正しい日時になっているかどうかを判定する
%%

-spec is_valid_datetime(datetime()) -> true | false.

is_valid_datetime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    ?IS_VALID_DATE(Year, Month, Day) andalso ?IS_VALID_HMS(Hour, Min, Sec);
is_valid_datetime(Bin) -> error(badarg, [Bin]).


%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec unix_timestamp_epoch_local() -> unix_timestamp().
unix_timestamp_epoch_local() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time(?UNIX_TIMESTAMP_EPOCH)).


%% @doc escape(\\). \\の次の文字がない場合はエラー.
-spec now_format_impl(Format, Acc, Data) -> binary() when
      Format :: binary(),
      Acc    :: [io_lib:chars()],
      Data   :: [data_in_format()].
now_format_impl(<<$\\, Rest/binary>>, Acc, Data) ->
    <<C, Rest1/binary>> = Rest,
    now_format_impl(Rest1, [C | Acc], Data);
now_format_impl(<<C:8, Rest/binary>>, Acc, Data) ->
    R = convert_in_format(C, Data),
    now_format_impl(Rest, [R | Acc], Data);
now_format_impl(<<>>, Acc, _) ->
    %% 終端処理
    IoList = lists:reverse(Acc),
    list_to_binary(IoList).

%% @doc 実際の文字の変換関数
-spec convert_in_format(C, Data) -> io_lib:chars() when
      C :: char(),
      Data :: [data_in_format()].
convert_in_format($Y, Data) ->
    {{Year, _, _}, {_, _, _}} = moyo_assoc:fetch(datetime, Data),
    io_lib:format("~4..0B", [Year]);
convert_in_format($m, Data) ->
    {{_, Month, _}, {_, _, _}} = moyo_assoc:fetch(datetime, Data),
    io_lib:format("~2..0B", [Month]);
convert_in_format($d, Data) ->
    {{_, _, Day}, {_, _, _}} = moyo_assoc:fetch(datetime, Data),
    io_lib:format("~2..0B", [Day]);
convert_in_format($H, Data) ->
    {{_, _, _}, {Hour, _, _}} = moyo_assoc:fetch(datetime, Data),
    io_lib:format("~2..0B", [Hour]);
convert_in_format($i, Data) ->
    {{_, _, _}, {_, Minute, _}} = moyo_assoc:fetch(datetime, Data),
    io_lib:format("~2..0B", [Minute]);
convert_in_format($s, Data) ->
    {{_, _, _}, {_, _, Second}} = moyo_assoc:fetch(datetime, Data),
    io_lib:format("~2..0B", [Second]);
convert_in_format($p, Data) ->
    {_, _, MicroSecs} = moyo_assoc:fetch(now, Data),
    MilliSecs = MicroSecs div 1000,
    io_lib:format("~3..0B", [MilliSecs]);
convert_in_format(C, _) -> C.

%% @private
%% @doc YYYY-Www-D表記で用いられる{ww, D}から日付を出す. 正しくない日付の場合はerrorにする
%% この表記は「年と週と曜日」による表記のことを指す
%% - 曜日は[1,7]の値域で月曜はじまりである
%% - 第1週は1月4日を含む週(月曜はじまり)である
%%  - 本当の規格では1月の初めの木曜日がある週と定義されているが同義
%%
%% 必ず引数にはintegerを入れること.

-spec weekday_to_date({non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> calendar:date() | error.

weekday_to_date({Year, Week, DoW}) ->
    D1_1 = calendar:date_to_gregorian_days(Year, 1, 1),
    %% 第1週の0日(前年最終週の最後の日)
    W1_0 = case calendar:day_of_the_week(Year, 1, 1) of
               DoW1_1 when DoW1_1 =< ?THURSDAY -> D1_1 - DoW1_1;
               DoW1_1                          -> D1_1 + 7 - DoW1_1
           end,
    Date = calendar:gregorian_days_to_date(W1_0 + (Week - 1) * 7 + DoW),
    moyo_cond:conditional({Year, Week} =:= calendar:iso_week_number(Date), Date, error).

%% @private
%% @doc YYYY-DDD表記からdateに変換する. 正しくない場合はerrorにする
%% この表記はその年の通算日を用いた表記方法である.
%% DoYの値域は[1, 366]. 平年は[1, 365]となる.
%% 必ず引数にはintegerを入れること.

-spec countday_to_date({non_neg_integer(), non_neg_integer()}) -> calendar:date() | error.

countday_to_date({Year, DoY}) ->
    G1_1 = calendar:date_to_gregorian_days({Year, 1, 1}),
    Date = calendar:gregorian_days_to_date(G1_1 + DoY - 1),
    case Date of
        {Year, _, _} -> Date;
        _            -> error
    end.


%% @private
%% @doc iso8601の日付部分がどの形式かを判定し、パース結果をタプルにして返す。
%% タプルに格納されているデータはbinary()である。

-spec parse_datetype_binary(binary()) -> {ok, {iso8601datetype(), tuple(), binary()}} | {error, binary()}.

parse_datetype_binary(<<Y:4/binary, Bin/binary>> = Input) ->
    WhileCount = fun(Limit) ->
                         fun(A, B) -> ?COND(B < Limit andalso ?IS_DIGIT(A), {true, B + 1}, {false, B}) end
                 end,
    case is_num_binary(Y) andalso Bin of
        false ->
            {error, Input};
        <<"W", W:2/binary, D:8, Rest/binary>> ->
            ?COND(D > $0 andalso D =< $7 andalso is_num_binary(W),
                  {ok, {yyyyWwwd, {Y, W, <<D>>}, Rest}}, {error, Input});
        <<"-", "W", W:2/binary, "-", D:8, Rest/binary>> ->
            ?COND(D > $0 andalso D =< $7 andalso is_num_binary(W),
                  {ok, {yyyy_Www_d, {Y, W, <<D>>}, Rest}}, {error, Input});
        <<"-", Bin1/binary>> -> % W以外のハイフンありをまとめて判定する
            YYYY_MM_DD = case Bin1 of
                             <<_M:2/binary, "-", _D:2/binary, _Rest/binary>> ->
                                 ?COND(is_num_binary(<<_M/binary, _D/binary>>),
                                       {ok, {yyyy_mm_dd, {Y, _M, _D}, _Rest}}, false);
                             _ -> false
                         end,
            if
                YYYY_MM_DD =/= false -> YYYY_MM_DD;
                true -> % yyyy_mm, yyyy_ddd は連続する数字の数で判定できる
                    case {moyo_list:foldl_while(WhileCount(3), 0, binary_to_list(Bin1)), Bin1} of
                        {2, <<M:2/binary, Rest/binary>>} ->
                            {ok, {yyyy_mm, {Y, M}, Rest}};
                        {3, <<DDD:3/binary, Rest/binary>>} ->
                            {ok, {yyyy_ddd, {Y, DDD}, Rest}};
                        _ ->
                            {ok, {yyyy, {Y}, Bin}}
                    end
            end;
        _ -> % ハイフンなしをyyyy以降の数字の数を使って判定する
            case {moyo_list:foldl_while(WhileCount(4), 0, binary_to_list(Bin)), Bin} of
                {4, <<M:2/binary, D:2/binary, Rest/binary>>} ->
                    {ok, {yyyymmdd, {Y, M, D}, Rest}};
                {3, <<DDD:3/binary, Rest/binary>>} ->
                    {ok, {yyyyddd, {Y, DDD}, Rest}};
                _ ->
                    {ok, {yyyy, {Y}, Bin}}
            end
    end.

%% @private
%% @doc parse_iso8601_time/1 から呼ばれる関数
%%
%% 引数のCountは何周目かを表す.
%% つまり、Count = 0から始まり、Count = 3になった時もしくは形式に一致しなくなった時に終了する.
%%
%% 小数に関しては[2.5, 0, 0] のように一緒に格納される.
%% 返り値のType が[]であった場合、errorである.
%%
%% Typeは　[h,h | [m,m | [s,s]]]のように連結されていく
%% HMSも同様である.
-spec parse_iso8601_time_sub(binary(), Count :: non_neg_integer()) ->
                                    {Type:: string(), HMS :: [number()], Rest:: binary()}.

parse_iso8601_time_sub(<<T:2/binary, Rest/binary>> = Input, Count) when Count < 3 ->
    TypeChar = time_type_string(Count),
    case {is_num_binary(T), Rest} of
        {false, _} ->
            %% 終了条件
            {[], lists:duplicate(3 - Count, 0), Input};
        {true, <<":", Next/binary>>} ->
            {TypeTail, TimeTail, RestTail} = parse_iso8601_time_sub(Next, Count + 1),
            {[TypeChar, TypeChar, $_ | TypeTail], [binary_to_integer(T) | TimeTail], RestTail};
        {true, <<Dot:8, RestDot/binary>>}  when Dot =:= $.; Dot =:= $, ->
            case io_lib:fread("~f", binary_to_list(<<T/binary, ".", RestDot/binary>>)) of
                {ok, [Double], TimeZone} ->
                    {[TypeChar, TypeChar, $_, $s], [Double | lists:duplicate(2 - Count, 0)], list_to_binary(TimeZone)};
                {error, _} ->
                    {[TypeChar, TypeChar], [binary_to_integer(T) | lists:duplicate(2 - Count, 0)], Rest}
            end;
        {true, _} ->
            {TypeTail, TimeTail, RestTail} = parse_iso8601_time_sub(Rest, Count + 1),
            {[TypeChar, TypeChar | TypeTail], [binary_to_integer(T) | TimeTail], RestTail}
    end;
parse_iso8601_time_sub(<<Bin/binary>>, Count) -> {[], lists:duplicate(3 - Count, 0), Bin}.

%% @private
%% @doc parse_iso8601_time用

-spec time_type_string(0..2) -> char().

time_type_string(Num) when is_integer(Num) ->
    case Num of
        0 -> $h;
        1 -> $m;
        2 -> $s
    end.

%% @private
%% @doc calendar:time()型の途中に小数がある物を、ノーマライズする。
%%
%% ```
%% > time_normalize({10.22, 0, 0}).
%% {10, 13, 12}
%% > time_normalize({10, 10.22, 0}).
%% {10, 10, 13}
%% '''

-spec time_normalize({Hour :: number(), Min :: number(), Sec :: number()}) -> calendar:time().

time_normalize({Hour, Min, Sec}) ->
    Normalize = fun(DoubleMin, OriginalSec) ->
                        Min_Int = trunc(DoubleMin), Min_Decimal = DoubleMin - Min_Int,
                        {Min_Int, OriginalSec + Min_Decimal * 60}
                end,
    {Hour_Ok, Min_h} = ?COND(is_integer(Hour),  {Hour, Min}, Normalize(Hour,  Min)),
    {Min_Ok, Sec_m}  = ?COND(is_integer(Min_h), {Min_h,Sec}, Normalize(Min_h, Sec)),
    Sec_Ok = trunc(Sec_m),
    {Hour_Ok, Min_Ok, Sec_Ok}.

%% @doc $0 ~ $9で構成されるバイナリかどうかを判定する
-spec is_num_binary(binary()) -> boolean().

is_num_binary(<<>>) -> true;
is_num_binary(<<X, Xs/binary>>) -> ?IS_DIGIT(X) andalso is_num_binary(Xs).
