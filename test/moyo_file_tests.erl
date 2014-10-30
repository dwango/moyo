%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
-module(moyo_file_tests).

-include_lib("eunit/include/eunit.hrl").

delete_directory_test_() ->
    [
     {"空のディレクトリを削除する",
      fun () ->
              Dir = moyo_file:make_temp_filepath(<<"moyo_file_test_">>),
              ok  = file:make_dir(Dir),

              ?assert(filelib:is_dir(Dir)),
              ?assertEqual(ok, moyo_file:delete_directory(Dir, false)),
              ?assert(not filelib:is_dir(Dir))
      end},
     {"ディレクトリを再帰的に削除する",
      fun () ->
              Dir = make_dummy_directory(),

              %% 再帰フラグがfalseの場合は削除に失敗する
              ?assertMatch({error, eexist}, moyo_file:delete_directory(Dir, false)),

              ?assert(filelib:is_dir(Dir)),
              ?assertEqual(ok, moyo_file:delete_directory(Dir, true)),
              ?assert(not filelib:is_dir(Dir))
      end},
     {"ディレクトリ以外に適用した場合は失敗する",
      fun () ->
              Path = moyo_file:make_temp_filepath(<<"moyo_file_test_">>),
              ok = file:write_file(Path, <<"dummy data">>),

              ?assert(filelib:is_file(Path)),
              ?assertMatch({error, {not_directory, _}}, moyo_file:delete_directory(Path, false)),
              ?assertMatch({error, {not_directory, _}}, moyo_file:delete_directory(Path, true)),
              ?assert(filelib:is_file(Path)),

              ok = file:delete(Path)
      end}
    ].

delete_if_exists_test_() ->
    [
     {"存在しないファイルを削除する",
      fun () ->
              %% examle.txt が存在しない状態にする
              case filelib:is_file("example.txt") of
                  true -> ?assertEqual(ok, file:delete("example.txt"));
                  false -> ok
              end,
              %% example.txt が存在しないけど失敗しない
              ?assertEqual(ok, moyo_file:delete_if_exists("example.txt"))
      end},
     {"存在するファイルを削除する",
      fun () ->
              ?assertEqual(ok, file:write_file("example.txt", "foo")),
              ?assertEqual(ok, moyo_file:delete_if_exists("example.txt"))
      end}
    ].

get_disk_usage_test_() ->
    [
     {"指定ディレクトリの容量を取得する",
      fun () ->
              Dir = make_dummy_directory(),
              ?assertMatch({ok, Size} when is_integer(Size), moyo_file:get_disk_usage(Dir)),
              ok = moyo_file:delete_directory(Dir, true)
      end},
     {"指定ファイルのサイズを取得する",
      fun () ->
              Path = moyo_file:make_temp_filepath(),
              Data = <<"dummy data">>,
              ok = file:write_file(Path, Data),

              ?assertEqual({ok, byte_size(Data)}, moyo_file:get_disk_usage(Path)),

              ok = file:delete(Path)
      end},
     {"指定ファイルが存在しない場合は、エラーではなくサイズ0扱いになる",
      fun () ->
              Path = moyo_file:make_temp_filepath(),
              ?assertEqual({ok, 0}, moyo_file:get_disk_usage(Path))
      end}
    ].

get_disk_usage_async_test_() ->
    [
     {"非同期でディスク容量を取得する",
      fun () ->
              Dir = make_dummy_directory(),
              Tag = disk_usage_result,

              %% 最初の呼び出しは常にokを返す
              ?assertEqual(ok, moyo_file:get_disk_usage_async(Dir, self(), Tag)),

              receive
                  {disk_usage_result, Result} ->
                      ?assertMatch({ok, Size} when is_integer(Size), Result)
              end,

              ok = moyo_file:delete_directory(Dir, true)
      end}
    ].

open_test_() ->
    [
     {"成功 -> io_device",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, open, 2, {ok, dummy_io_device}),

              ?assertEqual(dummy_io_device, moyo_file:open("example.txt", [read])),
              ?assert(meck:called(file, open, ["example.txt", [read]])),

              ok = meck:unload(file)
      end},
     {"失敗 -> raise error",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, open, 2, {error, some_reason}),

              ?assertError({failed_to_open_file, _, _}, moyo_file:open("example.txt", [read])),
              ?assert(meck:called(file, open, ["example.txt", [read]])),

              ok = meck:unload(file)
      end}
    ].

write_test_() ->
    [
     {"成功 -> ok",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, write, 2, ok),

              ?assertEqual(ok, moyo_file:write(dummy_io_device, "foo")),
              ?assert(meck:called(file, write, [dummy_io_device, "foo"])),

              ok = meck:unload(file)
      end},
     {"失敗 -> エラー発生",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, write, 2, {error, some_reason}),

              ?assertError({failed_to_write, _}, moyo_file:write(dummy_io_device, "foo")),
              ?assert(meck:called(file, write, [dummy_io_device, "foo"])),

              ok = meck:unload(file)
      end}
    ].

write_or_close_test_() ->
    [
     {"成功 -> ok",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, write, 2, ok),
              ok = meck:expect(file, close, 1, ok),

              ?assertEqual(ok, moyo_file:write_or_close(dummy_io_device, "foo")),
              ?assert(meck:called(file, write, [dummy_io_device, "foo"])),
              %% 成功したので file:close は呼ばれなかった
              ?assertNot(meck:called(file, close, [dummy_io_device])),

              ok = meck:unload(file)
      end},
     {"失敗 -> file:close が呼ばれ write のエラー発生",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, write, 2, {error, write_error_reason}),
              ok = meck:expect(file, close, 1, ok),

              ?assertError({failed_to_write, write_error_reason}, moyo_file:write_or_close(dummy_io_device, "foo")),
              ?assert(meck:called(file, write, [dummy_io_device, "foo"])),
              %% エラーを返す前に file:close もちゃんと呼ばれている
              ?assert(meck:called(file, close, [dummy_io_device])),

              ok = meck:unload(file)
      end},
     {"write 失敗 -> file:close が呼ばれる -> file:closeも失敗した場合 -> write, close に関するエラー発生",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, write, 2, {error, write_error_reason}),
              ok = meck:expect(file, close, 1, {error, close_error_reason}),

              ?assertError([{failed_to_write, write_error_reason}, {failed_to_close, close_error_reason}],
                               moyo_file:write_or_close(dummy_io_device, "foo")),
              ?assert(meck:called(file, write, [dummy_io_device, "foo"])),
              %% エラーを返す前に file:close もちゃんと呼ばれている
              ?assert(meck:called(file, close, [dummy_io_device])),

              ok = meck:unload(file)
      end}
    ].

close_test_() ->
    [
     {"成功 -> ok",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, close, 1, ok),

              ?assertEqual(ok, moyo_file:close(dummy_io_device)),
              ?assert(meck:called(file, close, [dummy_io_device])),

              ok = meck:unload(file)
      end},
     {"失敗 -> エラー発生",
      fun () ->
              ok = meck:new(file, [unstick, passthrough]),
              ok = meck:expect(file, close, 1, {error, some_reason}),

              ?assertError({failed_to_close, some_reason}, moyo_file:close(dummy_io_device)),
              ?assert(meck:called(file, close, [dummy_io_device])),

              ok = meck:unload(file)
      end}
    ].


%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
make_dummy_directory() ->
    Dir = moyo_file:make_temp_filepath(<<"moyo_file_test_">>),
    ok = file:make_dir(Dir),
    ok = file:make_dir(filename:join(Dir, "a")),
    ok = file:make_dir(filename:join(Dir, "b")),
    ok = file:write_file(filename:join([Dir, "a", "c.txt"]), <<"dummy data">>),
    Dir.
