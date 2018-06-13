%% @copyright 2017 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc ノード間のやりとりをするユーティリティ関数.
-module(moyo_rpc).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([is_function_callable/2,
         is_function_callable/3,
         function_exported/2,
         function_exported/3,
         ensure_loaded/2,
         ensure_loaded/3
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc 指定したノードにおいて, 指定した関数が呼び出し可能かどうかを確認する.
%%
%% {@link function_exported/3}の場合, まだロードされていないモジュールの関数に対しては`{ok, false}'を返すため,
%% その関数を呼び出せるかどうかの判定としては不十分である.
%% そこで`is_function_callable/3'では, 指定したモジュールのロードを試みた後に,
%% 指定した関数が存在するかどうかを確認する. これにより, 呼び出し可能かどうかを確実に判定することができる.
%%
%% @returns
%%   ノードとの通信に成功した場合, 指定された関数が呼び出し可能ならば`{ok, true}', 不可能ならば`{ok, false}'を返す.
%%
%%   ノードとの通信に失敗した場合, `{error, {badrpc, BadRpcReason}}'を返す.
%%   `BadRpcReason'は`rpc:call/5'と同様のものとなる.
%%   代表的なものは以下の通り:
%%   <dl>
%%     <dt>nodedown</dt><dd>ノードが落ちている</dd>
%%     <dt>timeout</dt><dd>タイムアウトした</dd>
%%   </dl>
-spec is_function_callable(Node :: node(), MFA :: mfa(), Timeout :: integer() | infinity) ->
                                  {ok, boolean()} | {error, term()}.
is_function_callable(Node, {Module, Func, Arity}, Timeout) ->
    case ensure_loaded(Node, Module, Timeout) of
        {error, {badrpc, Reason}} ->
            % RPCエラーはそのままエラーとする.
            {error, {badrpc, Reason}};
        {error, _} ->
            % リモートノードでのensure_loadedに失敗して, モジュールがロードできていない場合には{ok, false}とする.
            {ok, false};
        {ok, {module, Module}} ->
            % モジュールが元々ロードされていた場合や, ロードに成功した場合には, 関数がエクスポートされているか調べる.
            function_exported(Node, {Module, Func, Arity}, Timeout)
    end.

%% @equiv is_function_callable(Node, MFA, infinity)
-spec is_function_callable(Node :: node(), MFA :: mfa()) -> {ok, boolean()} | {error, nodedown | timeout | term()}.
is_function_callable(Node, MFA) -> is_function_callable(Node, MFA, infinity).

%% @doc 指定したノードに指定した関数が存在するかどうかを確認する.
%%
%% `erlang:function_exported/3'のRPC版.
%%
%% @returns
%%   ノードとの通信に成功した場合, 指定された関数が存在すれば`{ok, true}', 存在しなければ`{ok, false}'を返す.
%%
%%   ノードとの通信に失敗した場合, `{error, {badrpc, BadRpcReason}}'を返す.
%%   `BadRpcReason'は`rpc:call/5'と同様のものとなる.
-spec function_exported(Node :: node(), MFA :: mfa(), Timeout :: integer() | infinity) ->
                               {ok, boolean()} | {error, term()}.
function_exported(Node, {Module, Func, Arity}, Timeout) ->
    case rpc:call(Node, erlang, function_exported, [Module, Func, Arity], Timeout) of
        {badrpc, Reason} ->
            {error, {badrpc, Reason}};
        Boolean ->
            {ok, Boolean}
    end.

%% @equiv function_exported(Node, MFA, infinity)
-spec function_exported(Node :: node(), MFA :: mfa()) -> {ok, boolean()} | {error, term()}.
function_exported(Node, MFA) -> function_exported(Node, MFA, infinity).


%% @doc 指定したノードにおいて, 指定したモジュールが確実にロードされるようにする.
%%
%% `code:ensure_loaded/1'のRPC版.
%%
%% @returns
%%   ノードとの通信に成功した場合, 元々モジュールがロードされていた場合や,
%%    モジュールのロードに成功した場合には`{ok, {module Module}}'を,
%%   失敗すれば`{error, Reason}'を返す.
%%   `Reason'は`code:ensure_loaded/1'と同様のものとなる.
%%
%%   ノードとの通信に失敗した場合, `{error, {badrpc, BadRpcReason}}'を返す.
%%   `BadRpcReason'は`rpc:call/5'と同様のものとなる.
-spec ensure_loaded(Node :: node(), Module, Timeout :: integer() | infinity) ->
                           {ok, {module, Module}} | {error, term()} when
        Module :: module().
ensure_loaded(Node, Module, Timeout) ->
    case rpc:call(Node, code, ensure_loaded, [Module], Timeout) of
        {badrpc, Reason} ->
            {error, {badrpc, Reason}};
        {error, E} ->
            {error, E};
        {module, Module} ->
            {ok, {module, Module}}
    end.

%% @equiv ensure_loaded(Node, Module, infinity)
-spec ensure_loaded(Node :: node(), Module :: module()) -> {ok, {module, Module}} | {error, term()} when
        Module :: module().
ensure_loaded(Node, Module) ->
    ensure_loaded(Node, Module, infinity).
