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
         get_key/3
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

%% @doc {@link applications:get_key/2}にデフォルト値を指定可能にしたもの
-spec get_key(name(), atom(), term()) -> Value::term().
get_key(Application, Key, DefaultValue) ->
    case application:get_key(Application, Key) of
        undefined   -> DefaultValue;
        {ok, Value} -> Value
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
