%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc アプリケーション関連の処理を集めたモジュール
-module(moyo_application).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         ensure_loaded/1,
         ensure_all_loaded/1,
         ensure_all_unloaded/1,
         get_key/3,
         get_priv_dir/1
        ]).

-export_type([
              name/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type name() :: atom(). % アプリケーション名

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 指定されたアプリケーションが確実にロードされているようにする
%%
%% {@link application:load/1}とは異なり、既にロード済みのアプリケーションが指定された場合は`ok'が返される
-spec ensure_loaded(name()) -> ok | {error, Reason} when
      Reason :: term().
ensure_loaded(Application) ->
    case application:load(Application) of
        {error, {already_loaded, _}} -> ok;
        Other                        -> Other
    end.

%% @doc 指定されたアプリケーションおよびそれが依存するプリケーション群がロードされているようにする
%%
%% `Loaded'は、関数呼び出し中に新規にロードされたアプリケーション群
-spec ensure_all_loaded(name()) -> {ok, Loaded} | {error, Reason} when
      Loaded :: ordsets:ordset(name()),
      Reason :: term().
ensure_all_loaded(Application) ->
    AlreadyLoaded = gb_sets:from_list([Name || {Name, _, _} <- application:loaded_applications()]),
    case ensure_all_loaded_impl(gb_sets:singleton(Application), gb_sets:empty()) of
        {error, Reason} -> {error, Reason};
        {ok, Loaded}    ->
            %% NOTE: `gb_sets:to_list/1'が返り値の順序保証を仕様に明記していないので、
            %%       念のために`ordsets:from_list/1'を呼び出しておく
            LoadedDiff = ordsets:from_list(gb_sets:to_list(gb_sets:difference(Loaded, AlreadyLoaded))),
            {ok, LoadedDiff}
    end.

%% @doc `Pred(Application)'が`true'を返したアプリケーションを全て停止してアンロードする.
%%
%% {@link application:loaded_applications/0}の結果に基づき、<br />
%% ロードされているアプリケーションについて`Pred(Application)'を呼び出し、<br />
%% `Pred(Application)'が`true'を返したアプリケーションを全て停止してアンロードする.
%%
%% 一つでもアンロードに失敗した場合は、即座に`{error, Reason}'を返す.
%%
%% ※`kernel'と`stdlib'はアンロードしない.
-spec ensure_all_unloaded(fun((Application :: atom()) -> boolean())) -> ok | {error, Reason :: term()}.
ensure_all_unloaded(Pred) ->
    try
        ensure_all_unloaded_or_error(Pred)
    catch
        throw:Reason -> {error, Reason}
    end.

%% @doc {@link applications:get_key/2}にデフォルト値を指定可能にしたもの
-spec get_key(name(), atom(), term()) -> Value::term().
get_key(Application, Key, DefaultValue) ->
    case application:get_key(Application, Key) of
        undefined   -> DefaultValue;
        {ok, Value} -> Value
    end.

%% @doc {@link code:priv_dir/1}の代替となる関数。
%%
%% 標準あるいはERL_LIBS環境変数で指定されたディレクトリ以下に指定したアプリケーションが存在せず
%% code:priv_dirに失敗した場合もprivディレクトリを推測して値を返す
-spec get_priv_dir(name()) -> {ok, file:filename()} | {error, bad_name}.
get_priv_dir(Application) ->
    case code:priv_dir(Application) of
        {error, bad_name} ->
            case application:get_key(Application, modules) of % 代替方法でprivディレクトリのパスを推測する
                undefined    -> {error, bad_name};
                {ok, [M |_]} -> {ok, filename:join([filename:dirname(filename:dirname(code:which(M))), "priv"])}
            end;
        Dir -> {ok, Dir}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec ensure_all_loaded_impl(Queue, Loaded) -> {ok, Loaded} | {error, Reason} when
      Queue  :: gb_sets:set(name()), % 未処理のアプケーションセット (キュー)
      Loaded :: gb_sets:set(name()), % 処理済みのアプリケーションセット
      Reason :: term().
ensure_all_loaded_impl(Queue0, Loaded0) ->
    case gb_sets:is_empty(Queue0) of
        true  -> {ok, Loaded0};
        false ->
            {Application, Queue1} = gb_sets:take_smallest(Queue0),
            case ensure_loaded(Application) of
                {error, Reason} -> {error, Reason};
                ok              ->
                    Loaded1 = gb_sets:add(Application, Loaded0),
                    Dependings = gb_sets:from_list(get_key(Application, applications, [])),
                    UnloadedDependings = gb_sets:difference(Dependings, Loaded1),
                    ensure_all_loaded_impl(gb_sets:union(Queue1, UnloadedDependings), Loaded1)
            end
    end.

-spec ensure_all_unloaded_or_error(fun((Application :: atom()) -> boolean())) -> ok.
ensure_all_unloaded_or_error(Pred) ->
    [
     begin
         ok = case application:stop(Application) of
                  ok -> ok;
                  {error, {not_started, Application}} -> ok
              end,
         ok = case application:unload(Application) of
                  ok -> ok;
                  {error, {not_loaded, Application}} -> ok;
                  {error, {running, Application}} -> throw({running, Application})
              end
     end
     || {Application, _, _} <-
         application:loaded_applications(),
         Application =/= kernel andalso Application =/= stdlib andalso Pred(Application)
    ],
    ok.
