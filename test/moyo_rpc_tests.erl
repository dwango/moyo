%% @copyright 2017 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_rpc_tests).

-include_lib("moyo/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

-define(FUNCTION_EXPORTED_COMMON(NAME, F),
NAME() ->
    [
     {"確実にロードされているモジュールでのテスト",
      fun () ->
              ?assertEqual({ok, true},
                           F(node(), {moyo_rpc, function_exported, 2})),
              ?assertEqual({ok, true},
                           F(node(), {moyo_rpc, function_exported, 3})),
              ?assertEqual({ok, false},
                           F(node(), {moyo_rpc, function_exported, 0}))
      end},
     {"存在しないノードに対するテスト",
      fun () ->
              ?assertEqual({error, {badrpc, nodedown}},
                           F(unknown_node_name, {moyo_rpc, function_exported, 2}))
      end},
     {"存在しないモジュールに対するテスト",
      fun () ->
              ?assertEqual({ok, false},
                           F(node(), {monyo, monyo, 0}))
      end}
    ]).

-define(ENSURE_LOADED_COMMON(NAME, F),
NAME() ->
    [
     {"確実にロードされているモジュールでのテスト",
      fun () ->
              ?assertEqual({ok, {module, moyo_rpc}},
                           F(node(), moyo_rpc))
      end},
     {"存在しないノードに対するテスト",
      fun () ->
              ?assertEqual({error, {badrpc, nodedown}},
                           F(unknown_node_name, moyo_rpc))
      end},
     {"存在しないモジュールに対するテスト",
      fun () ->
              ?assertEqual({error, nofile},
                           F(node(), monyo))
      end}
    ]).

?FUNCTION_EXPORTED_COMMON(function_exported_2_test_, moyo_rpc:function_exported).
?FUNCTION_EXPORTED_COMMON(function_exported_3_test_, function_exported_3_stub).
?ENSURE_LOADED_COMMON(ensure_loaded_2_test_, moyo_rpc:ensure_loaded).
?ENSURE_LOADED_COMMON(ensure_loaded_3_test_, ensure_loaded_3_stub).
?FUNCTION_EXPORTED_COMMON(is_function_callable_2_test_, moyo_rpc:is_function_callable).
?FUNCTION_EXPORTED_COMMON(is_function_callable_3_test_, is_function_callable_3_stub).

function_exported_3_stub(N, MFA) -> moyo_rpc:function_exported(N, MFA, infinity).
ensure_loaded_3_stub(N, M) -> moyo_rpc:ensure_loaded(N, M, infinity).
is_function_callable_3_stub(N, MFA) -> moyo_rpc:is_function_callable(N, MFA, infinity).
