%% coding: latin-1
%%
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
