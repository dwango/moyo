%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc ファイル関連の処理を集めたユーティリティモジュール.
-module(moyo_file).

-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         delete_directory/2,
         delete_if_exists/1,
         get_disk_usage/1,
         get_disk_usage_async/3,
         make_temp_filepath/0, make_temp_filepath/1,
         open/2,
         write/2,
         write_or_close/2,
         close/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(MOYO_ERROR(Tag, Infos), {error, {Tag, [{place, self(), ?MODULE, ?LINE} | Infos]}}).
-define(MOYO_ERROR_FUNCALL(Module, Function, Args, Reason), ?MOYO_ERROR(funcall_failed, [{previous, Reason}, {mfargs, Module, Function, Args}])).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 指定ディレクトリを削除する.
%%
%% `Recursive'に`true'が指定された場合は、ディレクトリの子要素を含めて再帰的に削除処理を実行する. <br />
%% なお、指定ディレクトリが存在しない場合は`ok'が返される.
-spec delete_directory(file:name_all(), boolean()) -> ok | {error, Reason::term()}.
delete_directory(DirectoryPath, Recursive) ->
    Result =
        case file:read_file_info(DirectoryPath) of
            {error, Reason}                    -> {error, Reason};
            {ok, #file_info{type = directory}} ->
                case Recursive of
                    false -> file:del_dir(DirectoryPath);
                    true  -> delete_directory_recur(DirectoryPath)
                end;
            {ok, #file_info{type = Type}}      -> {error, {not_directory, Type}}
        end,
    case Result of
        {error, enoent} -> ok;
        _               -> Result
    end.

%% @doc ファイルを削除する. 失敗したらエラーが発生するが, ファイルがない時はエラーは起きず ok を返す.
%%
%% (補足)file:delete はファイルがないと {error, enoent} を返すが, この関数はそのような場合何もしないで ok を返す.
-spec delete_if_exists(file:name_all()) -> ok.
delete_if_exists(FilePath) ->
    case file:delete(FilePath) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> error({failed_to_delete, Reason})
    end.

%% @doc 指定パスのディスク使用量を取得する.
%%
%% 対象パスが存在しない場合は`{ok, 0}'が返る.
-spec get_disk_usage(file:name_all()) -> {ok, UsageBytes::non_neg_integer()} | {error, Reason::term()}.
get_disk_usage(Path) ->
    case file:read_file_info(Path) of
        {error, enoent} -> {ok, 0};
        {error, Reason} -> ?MOYO_ERROR_FUNCALL(file, read_file_info, [Path], Reason);
        {ok, #file_info{size = Size, type = directory}} ->
            case file:list_dir_all(Path) of
                {error, enoent} -> 0;
                {error, Reason} -> ?MOYO_ERROR_FUNCALL(file, list_dir_all, [Path], Reason);
                {ok, Filenames} ->
                    moyo_list:maybe_foldl(
                      fun (Filename, AccSize) ->
                              case get_disk_usage(filename:join(Path, Filename)) of
                                  {error, Reason} -> {error, Reason};
                                  {ok, SubSize}   -> {ok, AccSize + SubSize}
                              end
                      end,
                      Size,
                      Filenames)
            end;
        {ok, #file_info{size = Size}} ->
            {ok, Size}
    end.

%% @doc 指定パスのディス使用量を非同期に取得する.
%%
%% 容量取得処理が終了したタイミングで`Pid'プロセスに`{Tag, get_disk_usage(Path)}'形式のメッセージが送信される.
-spec get_disk_usage_async(file:name_all(), pid(), term()) -> ok.
get_disk_usage_async(Path, Pid, Tag) ->
    _ = spawn(fun () ->
                      Result =
                          try
                              get_disk_usage(Path)
                          catch
                              Class:Reason -> {error, {Class, Reason, erlang:get_stacktrace()}}
                          end,
                      Pid ! {Tag, Result}
              end),
    ok.

%% @equiv make_temp_filepath(<<"">>)
-spec make_temp_filepath() -> Path::binary().
make_temp_filepath() ->
    make_temp_filepath(<<"">>).

%% @doc ユニークな一時ファイルパスを生成して返す.
%%
%% 生成されるパスの形式は`/tmp/Prefix_ユニークな文字列'となる.
%%
%% なお、この関数自体はファイルの作成を行わないため、以下のように一時ファイルパスの生成から、
%% 実際のファイル作成までの間に、他のプロセスとの競合が発生する可能性があるので注意が必要.
%% ```
%% %% 1] 一時ファイルパスを生成 (この時点ではユニーク)
%% > Path = make_temp_filepath().
%% 　
%% %% 2] ここで他のプロセスが偶然同じファイル名を使用して file:write_file/2 を呼び出した
%% 　
%% %% 3] Pathにデータを書き込み
%%       => 他のプロセスが書き込んだ内容を上書きしてしまう！
%% > file:write_file(Path, <<"data">>).
%% '''
-spec make_temp_filepath(Prefix::binary()) -> Path::binary().
make_temp_filepath(<<Prefix/binary>>) ->
    Name = list_to_binary([Prefix, moyo_binary:to_hex(crypto:rand_bytes(8))]),
    Path = filename:join(<<"/tmp/">>, Name),
    case filelib:is_file(Path) of
        true  -> make_temp_filepath(Prefix);
        false -> iolist_to_binary(Path)
    end.

%% @doc {error, _} が返ってきた時にエラーを発生させる版の file:open/2 です.
-spec open(file:name_all(), [file:mode()]) -> file:io_device().
open(File, Modes) ->
    case file:open(File, Modes) of
        {error, Reason} -> error({failed_to_open_file, File, Reason});
        {ok, IoDevice} -> IoDevice
    end.

%% @doc {error, _} が返ってきた時にエラーを発生させる版の file:write/2 です.
-spec write(file:io_device(), iodata()) -> ok.
write(IoDevice, Bytes) ->
    case file:write(IoDevice, Bytes) of
        {error, Reason} ->
            error({failed_to_write, Reason});
        ok -> ok
    end.

%% @doc {error, _} が返ってきた時にファイルを閉じてエラーを発生させる版の file:write/2 です.
-spec write_or_close(file:io_device(), iodata()) -> ok.
write_or_close(IoDevice, Bytes) ->
    try
        write(IoDevice, Bytes)
    catch error:EW ->
            try
                close(IoDevice)
            catch error:EC ->
                    %% write error, close error どちらの内容も報告されます.
                    error([EW, EC])
            end,
            error(EW)
    end.

%% @doc {error, _} が返ってきた時にエラーを発生させる版の file:close/1 です.
-spec close(file:io_device()) -> ok.
close(IoDevice) ->
    case file:close(IoDevice) of
        ok -> ok;
        {error, Reason} -> error({failed_to_close, Reason})
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec delete_directory_recur(file:name_all()) -> ok | {error, Reason::term()}.
delete_directory_recur(DirectoryPath) ->
    case file:list_dir_all(DirectoryPath) of
        {error, enoent} -> ok;
        {error, Reason} -> ?MOYO_ERROR_FUNCALL(file, list_dir_all, [DirectoryPath], Reason);
        {ok, Filenames} ->
            Result =
                moyo_list:maybe_foreach(
                  fun (Filename) ->
                          Path = filename:join(DirectoryPath, Filename),
                          case file:read_file_info(Path) of
                              {error, enoent} -> ok;
                              {error, Reason} ->
                                  ?MOYO_ERROR_FUNCALL(file, read_file_info, [Path], Reason);
                              {ok, #file_info{type = directory}} ->
                                  delete_directory_recur(Path);
                              {ok, _} ->
                                  case file:delete(Path) of
                                      {error, enoent} -> ok;
                                      {error, Reason} -> ?MOYO_ERROR_FUNCALL(file, delete, [Path], Reason);
                                      ok              -> ok
                                  end
                          end
                  end,
                  Filenames),
            case Result of
                {error, Reason} -> {error, Reason};
                ok              ->
                    case file:del_dir(DirectoryPath) of
                        {error, enoent} -> ok;
                        {error, Reason} -> ?MOYO_ERROR_FUNCALL(file, del_dir, [DirectoryPath], Reason);
                        ok              -> ok
                    end
            end
    end.
