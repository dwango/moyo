%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_application_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
ensure_loaded_test_() ->
    [
     {"未ロードのアプリケーションをロードできる",
      fun () ->
              ok = ensure_unloaded(crypto),
              ?assertEqual(false, lists:keymember(crypto, 1, application:loaded_applications())),
              ?assertEqual(ok, moyo_application:ensure_loaded(crypto))
      end},
     {"ロード済みのアプリケーションが指定された場合は単に無視される",
      fun () ->
              ?assertEqual(true, lists:keymember(stdlib, 1, application:loaded_applications())),
              ?assertEqual({error, {already_loaded, stdlib}}, application:load(stdlib)),
              ?assertEqual(ok, moyo_application:ensure_loaded(stdlib))
      end},
     {"存在しないアプリケーションが指定された場合はエラーとなる",
      fun () ->
              ?assertMatch({error, _}, moyo_application:ensure_loaded('HOGE'))
      end}
    ].


ensure_all_loaded_test_() ->
    [
     {"「依存しており」かつ「未ロード」のアプリケーションを全てロードする",
      fun () ->
              ?assertMatch({ok, _}, moyo_application:ensure_all_loaded(moyo)),

              ok = ensure_unloaded(moyo),
              ok = ensure_unloaded(crypto),
              ?assertEqual({ok, [crypto, moyo]}, moyo_application:ensure_all_loaded(moyo))
      end},
     {"存在しないアプリケーションが指定された場合はエラーとなる",
      fun () ->
              ?assertMatch({error, _}, moyo_application:ensure_all_loaded('HOGE'))
      end}
    ].

ensure_all_unloaded_test_() ->
    {ok, RE} = re:compile("^rebar.*$"),
    [
     {"ロードされているアプリケーションを全てアンロードする.",
      fun () ->
              %% ロードしておく.
              ?assertEqual(ok, moyo_application:ensure_loaded(moyo)),
              ?assertEqual(ok, moyo_application:ensure_loaded(crypto)),
              %% 全てアンロードする(rebar関連以外).
              ?assertEqual(ok, moyo_application:ensure_all_unloaded(fun(Application) -> re:run(atom_to_list(Application), RE, [{capture, none}]) =/= match end)),
              %% 全てアンロードされている.
              Applications = application:loaded_applications(),
              ?assertEqual(false, lists:keymember(moyo, 1, Applications)),
              ?assertEqual(false, lists:keymember(crypto, 1, Applications)),
              %% リロードしておく.
              ?assertEqual(ok, moyo_application:ensure_loaded(moyo))
      end},
     {"ロードされている特定のアプリケーションをアンロードする.",
      fun () ->
              %% ロードしておく.
              ?assertEqual(ok, moyo_application:ensure_loaded(moyo)),
              ?assertEqual(ok, moyo_application:ensure_loaded(crypto)),
              %% `crypto'をアンロードする.
              ?assertEqual(ok, moyo_application:ensure_all_unloaded(fun(Application) -> Application =:= crypto end)),
              %% `crypto'がアンロードされている.
              Applications = application:loaded_applications(),
              ?assertEqual(true, lists:keymember(moyo, 1, Applications)), % `moyo'はアンロードされない.
              ?assertEqual(false, lists:keymember(crypto, 1, Applications))
      end},
     {"`kernel'と`stdlib'はアンロードされない.",
      fun () ->
              %% 全てアンロードする(rebar関連以外).
              ?assertEqual(ok, moyo_application:ensure_all_unloaded(fun(Application) -> re:run(atom_to_list(Application), RE, [{capture, none}]) =/= match end)),
              %% cryptoがアンロードされている.
              Applications = application:loaded_applications(),
              ?assertEqual(true, lists:keymember(kernel, 1, Applications)),
              ?assertEqual(true, lists:keymember(stdlib, 1, Applications)),
              %% リロードしておく.
              ?assertEqual(ok, moyo_application:ensure_loaded(moyo))
      end},
     {"アンロードに失敗した場合.",
      fun () ->
              ok = meck:new(application, [passthrough, unstick]),
              ok = meck:expect(application, unload, 1, {error, {running, moyo}}),
              ?assertEqual({error, {running, moyo}}, moyo_application:ensure_all_unloaded(fun(Application) -> Application =:= moyo end)),
              [application] = meck:unload()
      end}
    ].

get_key_test_() ->
    [
     {"*.appに項目が存在する場合は`application:get_key/2'と同じ挙動となる",
      fun () ->
              Default = 'DEFAULT',
              ?assertMatch({ok, Value} when Value =/= Default, application:get_key(moyo, vsn)), % 存在する
              ?assertEqual({ok, moyo_application:get_key(moyo, vsn, Default)}, application:get_key(moyo, vsn))
      end},
     {"*.appに項目が存在しない場合は、デフォルト値が返される",
      fun () ->
              Default = 'DEFAULT',
              ?assertEqual(undefined, application:get_key(moyo, unexisting_key)), % 存在しない
              ?assertEqual(Default, moyo_application:get_key(moyo, unexisting_key, Default))
      end}
    ].

get_priv_dir_test_() ->
    {setup,
     fun() -> _ = meck:new(application, [unstick, passthrough]) end,
     fun(_) -> _ = meck:unload() end,
     [
      {"privディレクトリが存在する場合は`code:priv_dir/1`と同じ値を返す",
       fun () ->
               ?assertEqual({ok, code:priv_dir(crypto)}, moyo_application:get_priv_dir(crypto))
       end},
      {"アプリケーションが存在しない場合はエラー値が返される",
       fun () ->
               ?assertEqual({error, bad_name}, moyo_application:get_priv_dir(not_existing_app))
       end},
      {"アプリケーションが存在し、privディレクトリが存在しない場合は推測されたパスが返される",
       fun () ->
               ok = meck:expect(application, get_key, 2, {ok, [crypto]}), % 存在するモジュールを返す
               ?assertEqual({ok, code:priv_dir(crypto)}, moyo_application:get_priv_dir(app_without_priv_dir))
       end}
     ]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec ensure_unloaded(moyo_application:name()) -> ok | {error, Reason::term()}.
ensure_unloaded(Application) ->
    case ensure_stopped(Application) of
        {error, Reason} -> {error, Reason};
        ok              ->
            case application:unload(Application) of
                {error, {not_loaded, _}} -> ok;
                Other                    -> Other
            end
    end.

-spec ensure_stopped(moyo_application:name()) -> ok | {error, Reason::term()}.
ensure_stopped(Application) ->
    case application:stop(Application) of
        {error, {not_started, _}} -> ok;
        Other                     -> Other
    end.
