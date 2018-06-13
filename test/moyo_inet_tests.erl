%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_inet_tests).

-include("eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

find_free_port_test_() ->
    {setup,
     fun() ->
             ok = meck:new([gen_tcp, inet], [unstick]),
             ok = meck:expect(gen_tcp, listen, 2, {ok, dummy}),
             ok = meck:expect(gen_tcp, close, 1, ok),
             ok = meck:expect(inet, port, 1, {ok, 1234})
     end,
     fun(_) ->
             _ = meck:unload()
     end,
     [
      {"返されたポートでlistenができる",
       fun() ->
               ?assignMatch({ok, Port},   moyo_inet:find_free_port()),
               ?assignMatch({ok, Socket}, gen_tcp:listen(Port, [])),
               ?assertEqual(ok, gen_tcp:close(Socket))
       end}
     ]}.
