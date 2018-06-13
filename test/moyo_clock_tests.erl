%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
-module(moyo_clock_tests).

-include_lib("eunit/include/eunit.hrl").

now_test_() ->
    [
     {"現在時刻をerlang:timestamp()形式で取得する",
      fun () ->
              ?assertMatch({X, Y, Z} when is_integer(X) andalso is_integer(Y) andalso is_integer(Z),
                           moyo_clock:now())
      end}
    ].
now_seconds_test_() ->
    [
     {"現在時刻をUNIXタイムスタンプ形式で取得する",
      fun () ->
              ?assertMatch(N when is_integer(N) andalso N >= 0, moyo_clock:now_seconds())
      end}
    ].

now_milliseconds_test_() ->
    [
     {"現在時刻をミリ秒形式で取得する",
      fun () ->
              ?assertMatch(N when is_integer(N) andalso N >= 0, moyo_clock:now_milliseconds())
      end}
    ].

seconds_to_datetime_test_() ->
    [
     {"時刻をUNIXタイムスタンプからdatetime()形式に変換する",
      fun () ->
              Input    = 1379985657,
              Expected = {{2013, 9, 24}, {10, 20, 57}},
              ?assertEqual(Expected, moyo_clock:seconds_to_datetime(Input))
      end},
     {"NIXタイムスタンプとして負数は非許可",
      fun () ->
              Input = -1,
              ?assertError(_, moyo_clock:seconds_to_datetime(Input))
      end}
    ].

seconds_to_datetime_tz_test_() ->
    [
     {"時刻をUNIXタイムスタンプからdatetime()形式に変換する",
      fun () ->
              Input    = 1379985657,
              ExpectedJst = {{2013, 9, 24}, {10, 20, 57}},
              ExpectedUtc = {{2013, 9, 24}, { 1, 20, 57}},
              ?assertEqual(ExpectedJst, moyo_clock:seconds_to_datetime_tz(Input, 540)),
              ?assertEqual(ExpectedUtc, moyo_clock:seconds_to_datetime_tz(Input, 0))
      end},
     {"NIXタイムスタンプとして負数は非許可",
      fun () ->
              Input = -1,
              ?assertError(_, moyo_clock:seconds_to_datetime_tz(Input, 0))
      end}
    ].

milliseconds_to_timestamp_test_() ->
    [
     {"unixtime[ミリ秒]をtimestampに変換する",
      fun () ->
              UnixtimeMs = 1504852037136, %% 2017年09月08日 15:27:17.136
              ExpectedTimestamp = {1504, 852037, 136000},
              ?assertEqual(ExpectedTimestamp, moyo_clock:milliseconds_to_timestamp(UnixtimeMs))
      end}
    ].

timestamp_to_milliseconds_test_() ->
    [
     {"timestampをunixtime[ミリ秒]に変換する(マイクロ秒以下は切り捨てられる)",
      fun () ->
              Timestamp = {1504, 852037, 136888},  %% 2017年09月08日 15:27:17.136888
              ExpectedMs = 1504852037136,
              ?assertEqual(ExpectedMs, moyo_clock:timestamp_to_milliseconds(Timestamp))
      end}
    ].

milliseconds_to_timestamp_to_milliseconds_test_() ->
    [
     {"unixtimeをtimestampに変換して元に戻す",
      fun () ->
              lists:foreach(
                fun (UnixtimeMs) ->
                        Timestamp = moyo_clock:milliseconds_to_timestamp(UnixtimeMs),
                        ?assertEqual(UnixtimeMs, moyo_clock:timestamp_to_milliseconds(Timestamp))
                end,
                [
                 0                   %% 1970年01月01日 09:00:00.000
                 , 870793200 * 1000  %% 1997年08月06日 00:00:00.000
                 , 1504852037136     %% 2017年09月08日 15:27:17.136
                ]
               )
      end}
    ].

datetime_to_seconds_test_() ->
    [
     {"時刻をdatetime()形式からUNIXタイムスタンプ形式に変換する",
      fun () ->
              Input    = {{2013, 9, 24}, {10, 20, 57}},
              Expected = 1379985657,
              ?assertEqual(Expected, moyo_clock:datetime_to_seconds(Input))
      end},
     {"UNIXタイムスタンプのエポック以前の日時を受け取るとエラーが送出される",
      fun () ->
              Input = {{1960,1,1}, {1,1,1}},
              ?assertError(_, moyo_clock:datetime_to_seconds(Input))
      end}
    ].

datetime_to_seconds_tz_test_() ->
    [
     {"時刻をdatetime()形式からUNIXタイムスタンプ形式に変換する",
      fun () ->
              Input    = {{2013, 9, 24}, {10, 20, 57}},
              ExpectedJst = 1379985657,
              ExpectedUtc = ExpectedJst + 32400,
              ?assertEqual(ExpectedJst, moyo_clock:datetime_to_seconds_tz(Input, 540)),
              ?assertEqual(ExpectedUtc, moyo_clock:datetime_to_seconds_tz(Input, 0))
      end},
     {"UNIXタイムスタンプのエポック以前の日時を受け取るとエラーが送出される",
      fun () ->
              Input = {{1960,1,1}, {1,1,1}},
              ?assertError(_, moyo_clock:datetime_to_seconds(Input, 0))
      end}
    ].

datetime_to_datetime_tz_test_() ->
    [
     {"datetime()のタイムゾーンを変換する",
      fun () ->
              Input           = {{2013, 9, 24}, {10, 20, 57}},
              ExpectedUtc2Jst = {{2013, 9, 24}, {19, 20, 57}},
              ExpectedJst2Utc = {{2013, 9, 24}, { 1, 20, 57}},
              ?assertEqual(ExpectedUtc2Jst, moyo_clock:datetime_to_datetime_tz(Input, 0, 540)),
              ?assertEqual(ExpectedJst2Utc, moyo_clock:datetime_to_datetime_tz(Input, 540, 0))
      end}
    ].

datetime_diff_test_() ->
    [
     {"二つの日時の差を求める",
      fun () ->
              Input1 = {{2013, 9, 24}, {10, 20, 57}},
              Input2 = {{2012, 10, 1}, {2, 23, 52}},
              Delta  = 30959825,

              ?assertEqual(Delta, moyo_clock:datetime_diff(Input1, Input2)),
              ?assertEqual(-Delta, moyo_clock:datetime_diff(Input2, Input1))
      end},
     {"現在時刻との差を求める",
      fun () ->
              Input = {{2013, 9, 24}, {10, 20, 57}},
              Now   = {{2012, 10, 1}, {2, 23, 52}},
              Delta = 30959825,

              %% 現在時刻はモックする
              ok = meck:new(calendar, [unstick, passthrough]),
              ok = meck:expect(calendar, local_time, 0, Now),

              ?assertEqual(Delta, moyo_clock:datetime_diff(Input)),

              ok = meck:unload(calendar)
      end}
    ].

datetime_diff_tz_test_() ->
    [
     {"二つの日時の差を求める",
      fun () ->
              Input1 = {{2013, 9, 24}, {10, 20, 57}},
              Input2 = {{2012, 10, 1}, {2, 23, 52}},
              Delta  = 30959825,

              ?assertEqual(Delta - 32400, moyo_clock:datetime_diff_tz(Input1, 540, Input2, 0)),
              ?assertEqual(-(Delta - 32400), moyo_clock:datetime_diff_tz(Input2, 0, Input1, 540))
      end},
     {"現在時刻との差を求める",
      fun () ->
              Input = {{2013, 9, 24}, {10, 20, 57}},
              Now   = {{2012, 10, 1}, {2, 23, 52}},
              Delta = 30959825,

              %% 現在時刻はモックする
              ok = meck:new(calendar, [unstick, passthrough]),
              ok = meck:expect(calendar, local_time, 0, Now),

              ?assertEqual(Delta, moyo_clock:datetime_diff_tz(Input, 540)),
              ?assertEqual(Delta + 32400, moyo_clock:datetime_diff_tz(Input, 0)),

              ok = meck:unload(calendar)
      end}
    ].

datetime_add_test_() ->
    [
     {"datetime()型の日時に、秒数を足し合わせる",
      fun () ->
              Seconds  = 30959825,
              DateTime = {{2012, 10, 1}, {2, 23, 52}},
              Expected = {{2013, 9, 24}, {10, 20, 57}},

              ?assertEqual(Expected, moyo_clock:datetime_add(DateTime, Seconds))
      end}
    ].

now_format_test_() ->
    [
     {"現在日時の書式化が可能",
      fun () ->
              %% 具体的な返り値の内容は、テスト時実施タイミングによって変わるので、ここでは型レベルのチェックまで
              ?assert(is_binary(moyo_clock:now_format(<<"Y/m/d-H:i:s(p)">>)))
      end},
     {"引数で指定した日時(タイムスタンプ)の書式化が可能",
      fun () ->
              Now      = {1381,309348,569954},  % {{2013,10,9},{18,2,28}} 相当
              Format   = <<"Y/m/d-H:i:s(p)">>,
              Expected = <<"2013/10/09-18:02:28(569)">>,
              ?assertEqual(Expected, moyo_clock:now_format(Format, Now))
      end},
     {"エスケースすることで、書式指定文字を通常の文字として扱うことが可能",
      fun () ->
              Now      = {1381,309348,569954},  % {{2013,10,9},{18,2,28}} 相当
              Format   = <<"Y\\Y">>,
              Expected = <<"2013Y">>,
              ?assertEqual(Expected, moyo_clock:now_format(Format, Now))
      end}
    ].

now_format_tz_test_() ->
    [
     {"現在日時の書式化が可能",
      fun () ->
              %% 具体的な返り値の内容は、テスト時実施タイミングによって変わるので、ここでは型レベルのチェックまで
              ?assert(is_binary(moyo_clock:now_format_tz(<<"Y/m/d-H:i:s(p)">>, 3600)))
      end},
     {"引数で指定した日時(タイムスタンプ)の書式化が可能",
      fun () ->
              Now      = {1381,309348,569954},  % {{2013,10,9},{18,2,28}} JST 相当
              Format   = <<"Y/m/d-H:i:s(p)">>,
              TzMinutes = 60, % one hour
              Expected = <<"2013/10/09-10:02:28(569)">>,
              ?assertEqual(Expected, moyo_clock:now_format_tz(Format, TzMinutes, Now))
      end},
     {"エスケースすることで、書式指定文字を通常の文字として扱うことが可能",
      fun () ->
              Now      = {1381,309348,569954},  % {{2013,10,9},{18,2,28}} JST 相当
              Format   = <<"Y\\Y">>,
              TzMinutes = 60, % one hour
              Expected = <<"2013Y">>,
              ?assertEqual(Expected, moyo_clock:now_format_tz(Format, TzMinutes, Now))
      end}
    ].

datetime_format_test_() ->
    [
     {"引数で指定した日時の書式化が可能",
      fun () ->
              DateTime = {{2013,10,9},{18,2,28}},
              Format   = <<"Y/m/d-H:i:s(p)">>,
              Expected = <<"2013/10/09-18:02:28(000)">>,
              ?assertEqual(Expected, moyo_clock:datetime_format(Format, DateTime))
      end}
    ].

datetime_format_tz_test_() ->
    [
     {"引数で指定した日時の書式化が可能",
      fun () ->
              DateTime = {{2013,10,9},{18,2,28}}, %JST
              Format   = <<"Y/m/d-H:i:s(p)">>,
              TzMinutes = 60, % one hour
              Expected = <<"2013/10/09-10:02:28(000)">>,
              ?assertEqual(Expected, moyo_clock:datetime_format_tz(Format, TzMinutes, DateTime, 540))
      end}
    ].

datetime_to_iso8601ext_test_() ->
    [
     {"`datetime()'型のローカル時刻をISO8601の拡張表記の日付文字列(バイナリ)に変換できる",
      fun () ->
              Input    = {{2004, 4, 1}, {16, 30, 0}},
              Expected = <<"2004-04-01T16:30:00+09:00">>,
              ?assertEqual(Expected, moyo_clock:datetime_to_iso8601ext(Input))
      end},
     {"入力が不正な場合はエラーとなる",
      fun () ->
              ?assertError(badarg, moyo_clock:datetime_to_iso8601ext(hoge))
      end}
    ].

datetime_to_iso8601ext_tz_test_() ->
    [
     {"`datetime()'型の指定タイムゾーン時刻をISO8601の拡張表記の日付文字列(バイナリ)に変換できる",
      fun () ->
              Input    = {{2004, 4, 1}, {16, 30, 0}},
              ExpectedUtc = <<"2004-04-01T16:30:00+00:00">>,
              ExpectedJst = <<"2004-04-01T16:30:00+09:00">>,
              ExpectedPst = <<"2004-04-01T16:30:00-08:00">>,
              ?assertEqual(ExpectedUtc, moyo_clock:datetime_to_iso8601ext_tz(Input, 0)),
              ?assertEqual(ExpectedJst, moyo_clock:datetime_to_iso8601ext_tz(Input, 540)),
              ?assertEqual(ExpectedPst, moyo_clock:datetime_to_iso8601ext_tz(Input, -480))
      end},
     {"入力が不正な場合はエラーとなる",
      fun () ->
              ?assertError(badarg, moyo_clock:datetime_to_iso8601ext_tz(hoge, 0))
      end}
    ].

iso8601ext_to_datetime_test_() ->
    [
     {"'YYYY-MM-DDThh:mm:ssZ'形式の日付文字列がパースできる",
      fun () ->
              Input    = <<"2004-04-01T12:00:00Z">>,
              Expected = calendar:universal_time_to_local_time({{2004, 4, 1}, {12, 0, 0}}),
              ?assertEqual(Expected, moyo_clock:iso8601ext_to_datetime(Input))
      end},
     {"'YYYY-MM-DDThh:mm:ss+hh:mm'形式の日付文字列がパースできる",
      fun () ->
              Input    = <<"2004-04-02T00:00:00+07:30">>,
              Expected = calendar:universal_time_to_local_time({{2004, 4, 1}, {16, 30, 0}}),
              ?assertEqual(Expected, moyo_clock:iso8601ext_to_datetime(Input))
      end},
     {"'YYYY-MM-DDThh:mm:ss-hh:mm'形式の日付文字列がパースできる",
      fun () ->
              Input    = <<"2004-04-01T23:00:00-02:00">>,
              Expected = calendar:universal_time_to_local_time({{2004, 4, 2}, {1, 0, 0}}),
              ?assertEqual(Expected, moyo_clock:iso8601ext_to_datetime(Input))
      end},
     {"不正な入力が渡された場合はエラーとなる",
      fun () ->
              %% 非バイナリ
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime(10)),

              %% iso8601拡張表記以外
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime(<<"2014/05/01 01:01:01">>)),

              %% ロケール部分が省略されている
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime(<<"2014-05-01T01:01:01">>)),

              %% ロケールの符号部分が間違っている
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime(<<"2014-05-01T01:01:01/01:00">>))
      end}
    ].

iso8601ext_to_datetime_tz_test_() ->
    [
     {"'YYYY-MM-DDThh:mm:ssZ'形式の日付文字列がパースできる",
      fun () ->
              Input    = <<"2004-04-01T12:00:00Z">>,
              ExpectedUtc = {{2004, 4, 1}, {12, 0, 0}},
              ExpectedJst = {{2004, 4, 1}, {21, 0, 0}},
              ?assertEqual(ExpectedUtc, moyo_clock:iso8601ext_to_datetime_tz(Input, 0)),
              ?assertEqual(ExpectedJst, moyo_clock:iso8601ext_to_datetime_tz(Input, 540))
      end},
     {"'YYYY-MM-DDThh:mm:ss+hh:mm'形式の日付文字列がパースできる",
      fun () ->
              Input    = <<"2004-04-02T00:00:00+07:30">>,
              ExpectedUtc = {{2004, 4, 1}, {16, 30, 0}},
              ExpectedJst = {{2004, 4, 2}, { 1, 30, 0}},
              ?assertEqual(ExpectedUtc, moyo_clock:iso8601ext_to_datetime_tz(Input, 0)),
              ?assertEqual(ExpectedJst, moyo_clock:iso8601ext_to_datetime_tz(Input, 540))
      end},
     {"'YYYY-MM-DDThh:mm:ss-hh:mm'形式の日付文字列がパースできる",
      fun () ->
              Input    = <<"2004-04-01T23:00:00-02:00">>,
              ExpectedUtc = {{2004, 4, 2}, { 1, 0, 0}},
              ExpectedJst = {{2004, 4, 2}, {10, 0, 0}},
              ?assertEqual(ExpectedUtc, moyo_clock:iso8601ext_to_datetime_tz(Input, 0)),
              ?assertEqual(ExpectedJst, moyo_clock:iso8601ext_to_datetime_tz(Input, 540))
      end},
     {"不正な入力が渡された場合はエラーとなる",
      fun () ->
              %% 非バイナリ
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime_tz(10, 0)),

              %% iso8601拡張表記以外
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime_tz(<<"2014/05/01 01:01:01">>, 0)),

              %% ロケール部分が省略されている
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime_tz(<<"2014-05-01T01:01:01">>, 0)),

              %% ロケールの符号部分が間違っている
              ?assertError(badarg, moyo_clock:iso8601ext_to_datetime_tz(<<"2014-05-01T01:01:01/01:00">>, 0))
      end}
    ].

now_unix_time_in_float_test_() ->
  [
    {"現在時刻を取得",
      fun () ->
        UnixTime = moyo_clock:now_unix_time_in_float(),
        ?assert( is_float( UnixTime ) ),
        %% 2014年4月15日~2100年1月1日ならオッケーというぐらいのテスト。
        ?assert( ( UnixTime > 1397541152 ) andalso ( UnixTime < 4102412400 ) )
    end}
   ].

is_valid_datetime_test_() ->
    [
     {"正常な日時でtrueを返す",
      fun() ->
              TrueValue = [{{2014, 1, 1}, {0, 0, 0}}, {{2100, 12, 31}, {23, 59, 59}}],
              [?assertEqual(true, moyo_clock:is_valid_datetime(X)) || X <- TrueValue]
      end},
     {"範囲外の日時にはfalseを返す",
      fun() ->
              FalseValue = [{{2014, 13, 1}, {0, 0, 0}}, {{2014, 0, 1}, { 0, 0, 0}},
                            {{2014, 10, 0}, {0, 0, 0}}, {{2014, 1, 32},{ 0, 0, 0}},
                            {{2014, 4, 31}, {0, 0, 0}}, {{2014, 1, 1}, {24, 0, 0}},
                            {{2014, 10, 1}, {0,60, 0}}, {{2014,10, 1}, { 0, 0,60}}
                           ],
              [?assertEqual(false, moyo_clock:is_valid_datetime(X)) || X <- FalseValue]
      end},
     {"負の値を入れるとfalseになる",
      fun() ->
              FalseValue = [{{  -1,10,10},{10,10,10}},{{2014,-1,10},{10,10,10}},
                            {{2014,10,-1},{10,10,10}},{{2014,10,10},{-1,10,10}},
                            {{2014,10,10},{10,-1,10}},{{2014,10,10},{10,10,-1}}
                           ],
              [?assertEqual(false, moyo_clock:is_valid_datetime(X)) || X <- FalseValue]
      end},
     {"floatを入れるとfalseになる",
      fun() ->
              FalseValue = [{{2014.0,10  ,10  },{10,10,10}},{{2014,10,10},{10.0,10  ,10  }},
                            {{2014  ,10.0,10  },{10,10,10}},{{2014,10,10},{10  ,10.0,10  }},
                            {{2014  ,10  ,10.0},{10,10,10}},{{2014,10,10},{10  ,10  ,10.0}}
                           ],
              [?assertEqual(false, moyo_clock:is_valid_datetime(X)) || X <- FalseValue]
      end},
     {"閏年チェック",
      fun() ->
              Time = {0, 0, 0},
              TrueDays  = [{1999, 2, 28}, {2004, 2, 29}, {2100, 2, 28}, {2000, 2, 29}],
              FalseDays = [{1999, 2, 29}, {2004, 2, 30}, {2100, 2, 29}, {2000, 2, 30}],
              [?assertEqual(true,  moyo_clock:is_valid_datetime({Day, Time})) || Day <- TrueDays],
              [?assertEqual(false, moyo_clock:is_valid_datetime({Day, Time})) || Day <- FalseDays]
      end},
     {"入力がdatetime型でない場合はerror",
      fun() ->
              ?assertError(badarg, moyo_clock:is_valid_datetime(<<"hoge">>))
      end}
    ].

parse_iso8601_date_test_() ->
    [
     {"各形式を認識できる",
      fun() ->
              Value = [
                       %% {Expected, Input}
                       {{ok, {yyyy,       {2014, 1, 1}}, <<>>}, <<"2014">>},
                       {{ok, {yyyy_mm,    {2014, 1, 1}}, <<>>}, <<"2014-01">>},
                       {{ok, {yyyy_mm_dd, {2014, 1, 1}}, <<>>}, <<"2014-01-01">>},
                       {{ok, {yyyymmdd,   {2014, 1, 1}}, <<>>}, <<"20140101">>},
                       {{ok, {yyyy_ddd,   {2014,12,31}}, <<>>}, <<"2014-365">>},
                       {{ok, {yyyyddd,    {2014,12,31}}, <<>>}, <<"2014365">>},
                       {{ok, {yyyy_Www_d, {2004, 4, 1}}, <<>>}, <<"2004-W14-4">>},
                       {{ok, {yyyyWwwd,   {2004, 4, 1}}, <<>>}, <<"2004W144">>}
                      ],
              [?assertEqual(Expected, moyo_clock:parse_iso8601_date(Input)) || {Expected, Input} <- Value]
      end},
     {"誤った入力への対応",
      fun() ->
              Value = [
                       %% {Expected, Input}
                       {error, <<"201A0101">>},
                       {error, <<"hoge">>},
                       {{ok, {yyyy_mm, {2014, 12, 1}}, <<"-4">>},    <<"2014-12-4">>},
                       {{ok, {yyyy, {2014, 1, 1}},     <<"11">>},    <<"201411">>},
                       {{ok, {yyyy, {2014, 1, 1}},     <<"A0101">>}, <<"2014A0101">>},
                       {{ok, {yyyy, {2014, 1, 1}},     <<"w144">>},  <<"2014w144">>},
                       {{ok, {yyyy, {2014, 1, 1}},     <<"-1-1">>},  <<"2014-1-1">>}
                      ],
              [?assertEqual(Expected, moyo_clock:parse_iso8601_date(Input)) || {Expected, Input} <- Value]
      end},
     {"binary以外が入力されるとbadarg",
      fun() ->
              [?assertError(badarg, moyo_clock:parse_iso8601_date(X)) || X <- [1, atom, "string"]]
      end},
     {"閏年チェック",
      fun() ->
              TrueDays  = [<<"19990228">>, <<"20040229">>, <<"21000228">>, <<"20000229">>],
              FalseDays = [<<"19990229">>, <<"20040230">>, <<"21000229">>, <<"20000230">>],
              [?assertMatch({ok, _, _}, moyo_clock:parse_iso8601_date(Input)) || Input <- TrueDays],
              [?assertEqual(error,      moyo_clock:parse_iso8601_date(Input)) || Input <- FalseDays]
      end},
     {"正常ではない日付を入力したときにエラーを出す",
      fun() ->
              Value = [<<"20141301">>, <<"20141232">>, <<"20140230">>, <<"2014366">>, <<"2014W531">>, <<"2014W109">>],
              [?assertEqual(error, moyo_clock:parse_iso8601_date(X)) || X <- Value]
      end},
     {"Week形式W011テスト",
      %% W011は1月で初めに木曜日がある週である
      fun() ->
              Value = [
                       %% Expected, Input
                       %% 1/1 : 木曜
                       {{ok,{yyyyWwwd, {2008,12,29}}, <<>>}, <<"2009W011">>},
                       %% 1/1 : 水曜
                       {{ok,{yyyyWwwd, {2013,12,30}}, <<>>}, <<"2014W011">>},
                       %% 1/1 : 火曜
                       {{ok,{yyyyWwwd, {2012,12,31}}, <<>>}, <<"2013W011">>},
                       %% 1/1 : 月曜
                       {{ok,{yyyyWwwd, {2007, 1, 1}}, <<>>}, <<"2007W011">>},
                       %% 1/1 : 日曜
                       {{ok,{yyyyWwwd, {2012, 1, 2}}, <<>>}, <<"2012W011">>},
                       %% 1/1 : 土曜
                       {{ok,{yyyyWwwd, {2011, 1, 3}}, <<>>}, <<"2011W011">>},
                       %% 1/1 : 金曜
                       {{ok,{yyyyWwwd, {2010, 1, 4}}, <<>>}, <<"2010W011">>}
                      ],
              [?assertEqual(Expected, moyo_clock:parse_iso8601_date(X)) || {Expected, X} <- Value]
      end}
    ].

parse_iso8601_time_test_() ->
    [
     {"iso8601の各形式を認識できる",
      fun() ->
              Value = [
                       %% {Expected, Input}
                       {{ok,{hh_mm,     {15,4, 0}}, <<>>}, <<"15:04">>},
                       {{ok,{hh_mm,     {15,4, 0}}, <<>>}, <<"15:04">>},
                       {{ok,{hh_mm_s,   {15,4,30}}, <<>>}, <<"15:04.5">>},
                       {{ok,{hh_mm_ss,  {15,4, 5}}, <<>>}, <<"15:04:05">>},
                       {{ok,{hh_mm_ss_s,{15,4, 5}}, <<>>}, <<"15:04:05.2">>},
                       {{ok,{hh_s,      {15,7,30}}, <<>>}, <<"15.125">>},
                       {{ok,{hhmm_s,    {15,4, 7}}, <<>>}, <<"1504.125">>},
                       {{ok,{hhmmss_s,  {15,4, 5}}, <<>>}, <<"150405.2">>},
                       {{ok,{hh_mm_s,   {15,4,52}}, <<>>}, <<"15:04.88">>},
                       {{ok,{hh_mm_s,   {15,0, 0}}, <<>>}, <<"15:00.00">>}
                      ],
              [?assertEqual(Expected, moyo_clock:parse_iso8601_time(Input)) || {Expected, Input} <- Value]
      end},
     {"正しくない時間の場合はエラー",
      fun() ->
              Value = [<<"24:00:00">>, <<"23:61:00">>, <<"23:59:62">>],
              [?assertEqual(error, moyo_clock:parse_iso8601_time(X)) || X <- Value]
      end},
     {"正しくない表記への対応",
      fun() ->
              Value = [
                       %% {Expected, Input}
                       {{ok, {hh,       {15, 0, 0}}, <<"e">>},   <<"15e">>},   %% 前方一致で正しい部分があればok
                       {{ok, {hh,       {15, 0, 0}}, <<"1">>},   <<"151">>},
                       {{ok, {hh,       {15, 0, 0}}, <<"e12">>}, <<"15e12">>},
                       {{ok, {hh,       {15, 0, 0}}, <<"..2">>}, <<"15..2">>},
                       {{ok, {hh_mm,    {15,30, 0}}, <<"..2">>}, <<"15:30..2">>},
                       {{ok, {hhmm,     {15,30, 0}}, <<"..2">>}, <<"1530..2">>},
                       {{ok, {hhmmss,   {15,30,30}}, <<"..2">>}, <<"153030..2">>},
                       {{ok, {hh_mm_ss, {15,30,30}}, <<"..2">>}, <<"15:30:30..2">>},
                       {error, <<"e">>}
                      ],
              [?assertEqual(Expected, moyo_clock:parse_iso8601_time(Input)) || {Expected, Input} <- Value]
      end},
     {"binary以外が入力されるとbadargになる",
      fun() ->
              [?assertError(badarg, moyo_clock:parse_iso8601_time(X)) || X <- [1, atom, "string"]]
      end}
    ].

parse_iso8601_timezone_test_() ->
    [
     {"iso8601の各形式を認識できる",
      fun() ->
              Value = [
                       %% {Expected, Input}
                       {{ok, {z,    {1, {0, 0, 0}}}, <<>>}, <<"Z">>},
                       {{ok, {hh,   {1, {1, 0, 0}}}, <<>>}, <<"+01">>},
                       {{ok, {hhmm, {-1,{1,30, 0}}}, <<>>}, <<"-0130">>},
                       {{ok, {hh_mm,{1, {1,30, 0}}}, <<>>}, <<"+01:30">>}
                     ],
              [?assertEqual(Expected, moyo_clock:parse_iso8601_timezone(Input)) || {Expected, Input} <- Value]
      end},
     {"正しくない表記への対応",
      fun() ->
              Value = [
                       %% {Expected, Input}
                       {{ok, {z,    {1, {0, 0,0}}}, <<"eeee">>}, <<"Zeeee">>}, %% 前方一致で正しい部分があればok
                       {{ok, {hhmm, {-1,{2,10,0}}}, <<"z">>},    <<"-0210z">>},
                       {{ok, {hh,   {1, {2, 0,0}}}, <<"1h">>},   <<"+021h">>},
                       {{ok, {hh,   {1, {2, 0,0}}}, <<"hoge">>}, <<"+02hoge">>},
                       {error, <<"+zz09">>},
                       {error, <<"+">>},
                       {error, <<"h">>},
                       {error, <<"=9:00">>}
                      ],
              [?assertEqual(Expected, moyo_clock:parse_iso8601_timezone(Input)) || {Expected, Input} <- Value]
      end},
     {"binary以外が入力されるとbadargになる",
      fun() ->
              [?assertError(badarg, moyo_clock:parse_iso8601_timezone(X)) || X <- [1, atom, "string"]]
      end}
    ].

parse_iso8601_test_() ->
    [
     {"iso8601の各形式を認識できる",
      fun() ->
              Value = [
                       %% {Expected, Input}
                       {{ok,{yyyy,
                             {{2014,1,1}, { 0,0,0}},  {1, {0,0,0}}}}, <<"2014">>},
                       {{ok,{yyyy_mm,
                             {{2014,1,1}, { 0,0,0}},  {1, {0,0,0}}}}, <<"2014-01">>},
                       {{ok,{yyyy_mm_dd,
                             {{2014,1,2}, { 0,0,0}},  {1, {0,0,0}}}}, <<"2014-01-02">>},
                       {{ok,{yyyymmdd,
                             {{2014,1,1}, { 0,0,0}},  {1, {0,0,0}}}}, <<"20140101">>},
                       {{ok,{yyyy_ddd,
                             {{2014,12,31},{0,0,0}},  {1, {0,0,0}}}}, <<"2014-365">>},
                       {{ok,{yyyyddd,
                             {{2014,12,31},{0,0,0}},  {1, {0,0,0}}}}, <<"2014365">>},
                       {{ok,{yyyy_Www_d,
                             {{2004,4,1}, { 0,0,0}},  {1, {0,0,0}}}}, <<"2004-W14-4">>},
                       {{ok,{yyyyWwwd,
                             {{2004,4,1}, { 0,0,0}},  {1, {0,0,0}}}}, <<"2004W144">>},
                       {{ok,{{yyyymmdd, hh_mm, z},
                             {{2014,1,2}, {15,4,0}},  {1, {0,0,0}}}}, <<"20140102T15:04Z">>},
                       {{ok,{{yyyy_mm_dd, hh_mm, hh_mm},
                             {{2014,1,2}, {15,4,0}},  {1, {9,0,0}}}}, <<"2014-01-02T15:04+09:00">>},
                       {{ok,{{yyyy_mm_dd, hh_mm_s, z},
                             {{2014,1,2}, {15,4,30}}, {1, {0,0,0}}}}, <<"2014-01-02T15:04.5Z">>},
                       {{ok,{{yyyy_mm_dd, hh_mm_s, hhmm},
                             {{2014,1,2}, {15,4,52}}, {1, {9,0,0}}}}, <<"2014-01-02T15:04.88+0900">>},
                       {{ok,{{yyyy_mm_dd, hh_mm_ss},
                             {{2014,1,2}, {15,4,5}},  {1, {0,0,0}}}}, <<"2014-01-02T15:04:05">>},
                       {{ok,{{yyyy_mm_dd, hh_mm_ss_s, hh_mm},
                             {{2014,1,2}, {15,4,5}},  {1, {9,0,0}}}}, <<"2014-01-02T15:04:05.2+09:00">>},
                       {{ok,{{yyyy_mm_dd, hh_s, hh_mm},
                             {{2014,1,2}, {15,7,30}}, {-1,{9,0,0}}}}, <<"2014-01-02T15.125-09:00">>},
                       {{ok,{{yyyy_mm_dd, hhmm_s, hhmm},
                             {{2014,1,2}, {15,4,7}},  {-1,{9,0,0}}}}, <<"2014-01-02T1504.125-0900">>},
                       {{ok,{{yyyy_mm_dd, hhmmss_s, hh_mm},
                             {{2014,1,2}, {15,4,5}},  {1, {9,0,0}}}}, <<"2014-01-02T150405.2+09:00">>}
                      ],
              [?assertEqual(Output, moyo_clock:parse_iso8601(Input)) || {Output, Input} <- Value]
      end},
     {"正常ではない入力はエラーになる",
      fun() ->
              Value = [
                       <<"hoge">>, <<"2014-1">>, <<"2014-01-02x15">>, <<"2014-01-02 15Z">>,
                       <<"2014-01-02T15:04:05Z09:00">>, <<"2014-01-0215:04Z">>, <<"2014-1-1">>,
                       <<"2014-W1">>, <<"2014-W1A-1">>
                      ],
              [?assertEqual(error, moyo_clock:parse_iso8601(X)) || X <- Value]
      end},
     {"binary以外が入力されるとbadargになる",
      fun() ->
              [?assertError(badarg, moyo_clock:parse_iso8601(X)) || X <- [1, atom, "string"]]
      end},
     {"正しくない時間でもエラーになる",
      fun() ->
              Value = [<<"2014-13">>, <<"2013-02-29">>, <<"2014-01-32">>, <<"2014-01-01T24:00Z">>],
              [?assertEqual(error, moyo_clock:parse_iso8601(X)) || X <- Value]
     end}
    ].

is_date_test_() ->
    [
     {"unixtimeを入れるとtrue",
      fun() ->
              UnixTime = [0, 1405474781],
              [?assertEqual(true, moyo_clock:is_date(X)) || X <- UnixTime]
      end},
     {"unixtimeとして負の値やfloatの値を入れるとfalse",
      fun() ->
              UnixTime = [-1, 1.0, 0.0],
              [?assertEqual(false, moyo_clock:is_date(X)) || X <- UnixTime]
      end},
     {"calendar:date/0に対応している",
      fun() ->
              ?assertEqual(true, moyo_clock:is_date({2014, 10, 10})),
              ?assertEqual(false, moyo_clock:is_date({2014, 12, 32})),
              ?assertEqual(false, moyo_clock:is_date({2014, 2, 29})),
              ?assertEqual(false, moyo_clock:is_date({2014, 0, 10}))
      end},
     {"calendar:datetime/0に対応している",
      fun() ->
              %% is_valid_datetimeでチェックしているので適当
              ?assertEqual(true, moyo_clock:is_date({{2014, 10, 10}, {10, 10, 10}}))
      end},
     {"iso8601形式の各種正常な入力形式に対応している",
      %% parse_iso8601で細かくやっているので比較的適当
      fun() ->
              ISO = [<<"2014">>, <<"2014-01">>, <<"2014-01-02">>,
                     <<"2014-01-02T15:04Z">>,      <<"2014-01-02T15:04:05Z">>,
                     <<"2014-01-02T15:04+09:00">>, <<"2014-01-02T15:04:05+09:00">>,
                     <<"2014-01-02T15:04-09:00">>, <<"2014-01-02T15:04:05-09:00">>
                    ],
              [?assertEqual({true,X}, {moyo_clock:is_date(X),X}) || X <- ISO],
              %% 正しくない入力
              ?assertEqual(false, moyo_clock:is_date(<<"hoge">>))
      end},
     {"stringではfalse",
      fun() ->
              ?assertEqual(false, moyo_clock:is_date("20140101"))
      end}
    ].

is_date_option_test_() ->
    [
     {"1つのOption一致",
      fun() ->
              ?assertEqual(true,  moyo_clock:is_date(<<"2014-01-02">>, [yyyy_mm_dd])),
              ?assertEqual(false, moyo_clock:is_date(<<"2014-01-02">>, [yyyy]))
      end},
     {"複数のOption",
      fun() ->
              ?assertEqual(true,  moyo_clock:is_date(<<"2014-01-02">>, [yyyy, yyyy_mm_dd])),
              ?assertEqual(false, moyo_clock:is_date(<<"2014-01-02">>, [yyyy, yyyymmdd]))
      end}
    ].

iso8601_to_datetime_test_() ->
    [
     {"iso8601形式がdatetime()になる",
     fun() ->
             ?assertEqual({{2007,2,7},{0,0,0}}, moyo_clock:iso8601_to_datetime(<<"2007-02-07T09:00:00+09:00">>))
     end},
     {"他のbinaryが入力されるとbadarg",
      fun() ->
              ?assertError(badarg, moyo_clock:iso8601_to_datetime(<<"2014-04-02T01a">>))
      end}
    ].

iso8601_to_datetime_tz_test_() ->
    [
     {"iso8601形式がdatetime()になる",
     fun() ->
             ?assertEqual({{2007,2,7},{0,0,0}}, moyo_clock:iso8601_to_datetime_tz(<<"2007-02-07T09:00:00+09:00">>, 0)),
             ?assertEqual({{2007,2,7},{9,0,0}}, moyo_clock:iso8601_to_datetime_tz(<<"2007-02-07T09:00:00+09:00">>, 540))
     end},
     {"他のbinaryが入力されるとbadarg",
      fun() ->
              ?assertError(badarg, moyo_clock:iso8601_to_datetime_tz(<<"2014-04-02T01a">>, 0))
      end}
    ].
