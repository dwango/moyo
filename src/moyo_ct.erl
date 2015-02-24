%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc Common Test Utility
-module(moyo_ct).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         all/1,
         eunit/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc SUITE moduleのCommon TestのCall back Functions以外を取得する.
%%
%% Arity =:= 1 以外は取得しない為, Eunit対象も外れる.
-spec all(SuiteModule :: module()) -> [Function :: atom()].
all(SuiteModule) ->
    [X || {X, 1} <- SuiteModule:module_info(exports),
          %% common test
          X =/= all,
          X =/= groups,
          X =/= suite,
          X =/= init_per_suite,
          X =/= end_per_suite,
          X =/= init_per_group,
          X =/= end_per_group,
          X =/= init_per_testcase,
          X =/= end_per_testcase,
          %% other
          X =/= module_info,
          X =/= test].

%% @doc EunitをCommon Testに組み込む場合に使用できる.
-spec eunit(Application :: atom()) -> ok.
eunit(Application) ->
    ok = eunit:test({application, Application}).
