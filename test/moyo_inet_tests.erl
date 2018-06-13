%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_inet_tests).

-include("eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

find_free_port_test_() ->
    {foreach,
     fun setup/0,
     fun clean/1,
     [
      {"返されたポートでlistenができる",
       fun() ->
               ?assignMatch({ok, Port},   moyo_inet:find_free_port()),
               ?assignMatch({ok, Socket}, gen_tcp:listen(Port, [])),
               ?assertEqual(ok, gen_tcp:close(Socket))
       end},
      {"find_free_port/1では, `Count'個の相異なるポートが返り, 返されたすべてのポートでlistenができる",
       fun() ->
               Count = 10,
               ?assignMatch({ok, Ports}, moyo_inet:find_free_port(Count)),
               ?assertEqual(Count, length(Ports)), % `Count'個のポート
               ?assertEqual(Ports, moyo_list:uniq(Ports)), % 相異なるポート
               %% gen_tcp:close が呼ばれているか確かめることで, すべてのポートが
               %% 閉じられていることを確かめる (= すべてのポートでlistenができる)
               [?assert(meck:called(gen_tcp, close, [{socket, Port}])) || Port <- Ports]
       end},
      {"find_free_port/1では, 空きポートの数が`Count'より小さい場合は`{error, system_limit}'が返る. 処理途中でlistenしたポートはすべて閉じられている.",
       fun() ->
               Exceeded = 11,
               ?assertEqual({error, system_limit}, moyo_inet:find_free_port(Exceeded)), % 空きポートは10まで
               %% gen_tcp:close が呼ばれているか確かめることで, すべてのポートが閉じられていることを確かめる
               [?assert(meck:called(gen_tcp, close, [{socket, Port}])) || Port <- lists:seq(1, 10)]
       end},
      {"find_free_port/1では, `Count'に-1を指定しても空のリストが返る",
       fun() ->
               Count = -1,
               ?assertEqual({ok, []}, moyo_inet:find_free_port(Count))
       end}
     ]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% gen_tcp:listen, gen_tcp:close, inet:port のスタブを作る
%% 1~10 までのポートが空いていて, それ以上は system_limit
-spec setup() -> ok.
setup() ->
    ok = meck:new([gen_tcp, inet], [unstick]),
    Seq = meck:seq([{ok, {socket, Port}} || Port <- lists:seq(1, 10)] ++ [{error, system_limit}]),
    ok = meck:expect(gen_tcp, listen, 2, Seq),
    ok = meck:expect(gen_tcp, close, 1, ok),
    ok = meck:expect(inet, port, fun({socket, Port}) -> {ok, Port} end).

-spec clean(term()) -> ok.
clean(_) -> _ = meck:unload().
