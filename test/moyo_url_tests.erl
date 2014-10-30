%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_assocモジュールのユニットテスト
-module(moyo_url_tests).

-include_lib("eunit/include/eunit.hrl").

urlencode_base64_test_() ->
    [
     {"バイナリをbase64url形式にエンコード",
      fun () ->
              Input    = <<74,246,131,230,69,201,139,137,216,12,185,11,47,244,62,144,246,158,93,40>>,
              Expected = <<"SvaD5kXJi4nYDLkLL_Q-kPaeXSg=">>,
              ?assertEqual(Expected, moyo_url:urlencode_base64(Input))
      end},
     {"バイナリをbase64url形式(パディング文字なし)にエンコード",
      fun () ->
              Input    = <<74,246,131,230,69,201,139,137,216,12,185,11,47,244,62,144,246,158,93,40>>,
              Expected = <<"SvaD5kXJi4nYDLkLL_Q-kPaeXSg">>,
              ?assertEqual(Expected, moyo_url:urlencode_base64(Input, [no_padding]))
      end}
    ].

urldecode_base64_test_() ->
    [
     {"base64url形式でエンコードされたバイナリを復号",
      fun () ->
              Input1    = <<"SvaD5kXJi4nYDLkLL_Q-kPaeXSg=">>,
              Expected1 = <<74,246,131,230,69,201,139,137,216,12,185,11,47,244,62,144,246,158,93,40>>,
              ?assertEqual(Expected1, moyo_url:urldecode_base64(Input1)),

              Input2    = <<"SvaD5kXJi4nYDLkLL_Q-kPaeXSg">>, % パディング文字が省略されていても複合可能
              Expected2 = <<74,246,131,230,69,201,139,137,216,12,185,11,47,244,62,144,246,158,93,40>>,
              ?assertEqual(Expected2, moyo_url:urldecode_base64(Input2))
      end}
    ].

urlencode_rfc3986_test_() ->
    [
     {"テキストを RFC3986 にもとづいてエンコードする",
      fun () ->
              Input    = <<"sales and marketing/Miami">>,
              Expected = <<"sales%20and%20marketing%2FMiami">>,
              ?assertEqual(Expected, moyo_url:urlencode_rfc3986(Input))
      end}
    ].

urldecode_rfc3986_test_() ->
    [
     {"RFC3986形式でエンコードされたバイナリを複合する",
      fun () ->
              Input    = <<"sales%20and%20marketing%2FMiami">>,
              Expected = <<"sales and marketing/Miami">>,
              ?assertEqual(Expected, moyo_url:urldecode_rfc3986(Input))
      end}
    ].

build_qs_test_() ->
    [
     {"連想リストからHTTPのクエリ文字列を生成する",
      fun () ->
              AssocList = [{color, red},
                           {lang, erlang}],
              Expected = <<"color=red&lang=erlang">>,

              ?assertEqual(Expected, moyo_url:build_qs(AssocList))
      end}
    ].

parse_query_test_() ->
    [
     {"クエリストリングがパースできる",
      fun () ->
              Query = <<"a=10&b=erlang">>,
              Expected = [{<<"a">>, <<"10">>},
                          {<<"b">>, <<"erlang">>}],
              ?assertEqual(Expected, moyo_url:parse_query(Query))
      end},
     {"キーのみのパラメータの値は、空バイナリとなる",
      fun () ->
              Query = <<"no_value&b=erlan=">>,
              Expected = [{<<"no_value">>, <<"">>},
                          {<<"b">>, <<"erlan=">>}],
              ?assertEqual(Expected, moyo_url:parse_query(Query))
      end},
     {"キーと値の両方が省略された場合は、パース後の値はどちらも空バイナリとなる",
      fun () ->
              Query = <<"a=b&&&&c=d">>,
              Expected = [{<<"a">>, <<"b">>},
                          {<<"">>, <<"">>},
                          {<<"">>, <<"">>},
                          {<<"">>, <<"">>},
                          {<<"c">>, <<"d">>}],
              ?assertEqual(Expected, moyo_url:parse_query(Query))
      end},

     {"キー及び値に含まれるパーセントエンコーディングされた文字は復号される",
      fun () ->
              Query = <<"a%23b=12%2a">>,
              Expected = [{<<"a#b">>, <<"12*">>}],
              ?assertEqual(Expected, moyo_url:parse_query(Query))
      end},
     {"キー及び値に含まれる'+'は' 'に展開される",
      fun () ->
              Query = <<"a+b=1++">>,
              Expected = [{<<"a b">>, <<"1  ">>}],
              ?assertEqual(Expected, moyo_url:parse_query(Query))
      end}
    ].
