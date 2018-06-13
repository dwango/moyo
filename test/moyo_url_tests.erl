%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc moyo_assocモジュールのユニットテスト
-module(moyo_url_tests).

-include("eunit.hrl").

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

parse_scheme_test_() ->
    [
     {"parseに成功する",
        fun () ->
                [
                    ?assertEqual({ok,Expected},moyo_url:parse_scheme(URL))
                    ||
                    {URL, Expected} <- [
                        {<<"scheme://">>, {<<"scheme">>, <<"">>}},
                        {<<"://hoge">>, {<<"">>, <<"hoge">>}},
                        {<<"://">>, {<<"">>, <<"">>}},
                        {<<"::::////">>, {<<":::">>, <<"//">>}}
                    ]
            ]
        end},
      {"parseに失敗する",
        fun() ->
            [
                ?assertEqual(error,moyo_url:parse_scheme(URL))
                || URL <- [<<":::::::">>, <<"asdfghjkl">> ,<<"/////">>]
            ]
        end}
    ].

parse_url_test_()->
    [
        {"成功する",
            fun() ->
                ?assertMatch(
                    {ok, #{scheme:=<<"http">>, host:= <<"github.o-in.dwango.co.jp">>, path := <<"/Erlang/moyo">>}},
                    moyo_url:parse_url(<<"http://github.o-in.dwango.co.jp/Erlang/moyo">>))
            end},
        {"ipv6を入力にして成功する",
            fun() ->
                ?assertMatch(
                    {ok, #{scheme := <<"http">>,
                        host := <<"FEDC:BA98:7654:3210:FEDC:BA98:7654:3210">>, port := 80, path:= <<"/index.html">>}},
                    moyo_url:parse_url(<<"http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html">>))
            end},
        {"Fragment Query Userinfo が空文字（区切り記号以外）の場合のテスト",
            fun() ->
                ?assertMatch(
                    {ok, #{scheme := <<"s">>, userinfo := <<"">>, host := <<"h">>,
                        path := <<"/">>, query := <<"?">>,fragment := <<"#">>}},
                    moyo_url:parse_url(<<"s://@h?#">>))
            end},
        {"成功する最小の入力",
            fun() ->
                ?assertMatch(
                    {ok, #{scheme:=<<"">>, host:= <<"">>, path := <<"/">>}},
                    moyo_url:parse_url(<<"://">>))
            end},
        {"Fragment Query Path Port Userinfo の有無に関わらずparseに成功する",
            %% 5項目の有無なので2^5=32パターン
            fun() ->
                [
                    ?assertMatch(Output, moyo_url:parse_url(URL))
                    ||
                    {URL, Output} <- [
                        %%userinfo:true ,port:true ,path:true ,query:true ,fragment:true
                        {<<"s://host">>, {ok, #{host => <<"host">>, path => <<"/">>, scheme => <<"s">>}}},
                        %%userinfo:true ,port:true ,path:true ,query:true ,fragment:false
                        {<<"s://host#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:true ,path:true ,query:false,fragment:true
                        {<<"s://host?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:true ,path:true ,query:false,fragment:false
                        {<<"s://host?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:true ,path:false,query:true ,fragment:true
                        {<<"s://host/p@">>, {ok, #{host => <<"host">>, path => <<"/p@">>, scheme => <<"s">>}}},
                        %%userinfo:true ,port:true ,path:false,query:true ,fragment:false
                        {<<"s://host/p@#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:true ,path:false,query:false,fragment:true
                        {<<"s://host/p@?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/p@">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:true ,path:false,query:false,fragment:false
                        {<<"s://host/p@?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:true ,query:true ,fragment:true
                        {<<"s://host:99">>, {ok, #{host => <<"host">>, path => <<"/">>, port => 99, scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:true ,query:true ,fragment:false
                        {<<"s://host:99#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            port => 99,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:true ,query:false,fragment:true
                        {<<"s://host:99?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:true ,query:false,fragment:false
                        {<<"s://host:99?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:false,query:true ,fragment:true
                        {<<"s://host:99/p@">>, {ok, #{host => <<"host">>, path => <<"/p@">>, port => 99, scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:false,query:true ,fragment:false
                        {<<"s://host:99/p@#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            port => 99,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:false,query:false,fragment:true
                        {<<"s://host:99/p@?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/p@">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:true ,port:false,path:false,query:false,fragment:false
                        {<<"s://host:99/p@?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>}}},
                        %%userinfo:false,port:true ,path:true ,query:true ,fragment:true
                        {<<"s://user@host">>, {ok, #{host => <<"host">>,
                            path => <<"/">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:true ,path:true ,query:true ,fragment:false
                        {<<"s://user@host#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:true ,path:true ,query:false,fragment:true
                        {<<"s://user@host?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:true ,path:true ,query:false,fragment:false
                        {<<"s://user@host?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:true ,path:false,query:true ,fragment:true
                        {<<"s://user@host/p@">>, {ok, #{host => <<"host">>,
                            path => <<"/p@">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:true ,path:false,query:true ,fragment:false
                        {<<"s://user@host/p@#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:true ,path:false,query:false,fragment:true
                        {<<"s://user@host/p@?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/p@">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:true ,path:false,query:false,fragment:false
                        {<<"s://user@host/p@?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:true ,query:true ,fragment:true
                        {<<"s://user@host:99">>, {ok, #{host => <<"host">>,
                            path => <<"/">>,
                            port => 99,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:true ,query:true ,fragment:false
                        {<<"s://user@host:99#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            port => 99,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:true ,query:false,fragment:true
                        {<<"s://user@host:99?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:true ,query:false,fragment:false
                        {<<"s://user@host:99?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:false,query:true ,fragment:true
                        {<<"s://user@host:99/p@">>, {ok, #{host => <<"host">>,
                            path => <<"/p@">>,
                            port => 99,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:false,query:true ,fragment:false
                        {<<"s://user@host:99/p@#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            port => 99,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:false,query:false,fragment:true
                        {<<"s://user@host:99/p@?q/=?">>, {ok, #{host => <<"host">>,
                            path => <<"/p@">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}},
                        %%userinfo:false,port:false,path:false,query:false,fragment:false
                        {<<"s://user@host:99/p@?q/=?#f@/?">>, {ok, #{fragment => <<"#f@/?">>,
                            host => <<"host">>,
                            path => <<"/p@">>,
                            port => 99,
                            query => <<"?q/=?">>,
                            scheme => <<"s">>,
                            userinfo => <<"user">>}}}
                    ]
                    %% 上のテストパターンの生成コード
                    %%   パターンの生成ルール理解のために書き残す
                    %% Out=[maps:filter(fun(_,V)->V=/=none end,
                    %%      #{scheme=><<"s">>, userinfo => U,host=><<"host">>,port=>Po,path=>P,query=>Q,fragment=>F})
                    %%      || U<-[none,<<"user">>],     Po<-[none,99],            P<-[<<"/">>,<<"/p@">>],  Q<-[none,<<"?q/=?">>],  F<-[none,<<"#f@/?">>] ],
                    %% In=[<<"s://",U/binary,"host",Po/binary,P/binary,Q/binary,F/binary>>
                    %%      || U<-[<<"">>,<<"user@">>],  Po<-[<<"">>,<<":99">>],   P<-[<<"">>,<<"/p@">>],   Q<-[<<"">>,<<"?q/=?">>],F<-[<<"">>,<<"#f@/?">>] ],
                    %% Comment=[{U,Po,P,Q,F}||U<-["true ","false"],Po<-["true ","false"],P<-["true ","false"],Q<-["true ","false"],F<-["true ","false"]],
                    %% [io:format("%%userinfo:~s,port:~s,path:~s,query:~s,fragment:~s\n{~p,{ok,~p}},~n",[A,S,D,F,G,I,O])|| {I,O,{A,S,D,F,G}}<-lists:zip3(In,Out,Comment)].
                ]
            end},
        {"schemeがない場合",
            fun() ->
                ?assignMatch({error, require_scheme}, moyo_url:parse_url(<<"//moyo.com">>))
            end},
        {"host名が不正な場合",
            fun() ->
                ?assignMatch({error, require_host}, moyo_url:parse_url(<<"scheme://[?q">>))
            end},
        {"portが負の場合",
            fun() ->
                ?assignMatch({error, require_port_is_integer}, moyo_url:parse_url(<<"scheme://foo:-5">>))
            end}
    ].
