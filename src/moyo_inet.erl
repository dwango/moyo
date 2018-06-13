%% @copyright 2013-2015 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc `inet'の拡張ライブラリ
-module(moyo_inet).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

-export([
         find_free_port/0, find_free_port/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc 現在空いているポートを1つ返す
%%
%% この関数を呼び出した際の空きポートを返す為, そのポートが他のアプリケーションによって使用されてしまい, 使えない可能性がある.
-spec find_free_port() -> {ok, inet:port_number()} | {error, Reason :: term()}.
find_free_port() ->
    case find_free_port(1) of
        {ok, [Port]} -> {ok, Port}; % find_free_port(1) は ok を返す場合は必ず1つのポートになる
        {error, Reason} -> {error, Reason}
    end.

%% @doc 現在空いているポートを引数として与えられた個数返す. 順不同.
%% 現在の空きポート以上の数を要求した場合は, `{error, system_limit}' が返る.
%%
%% この関数を呼び出した際の空きポートを返す為, そのポートが他のアプリケーションによって使用されてしまい, 使えない可能性がある.
-spec find_free_port(Count :: non_neg_integer()) -> {ok, [inet:port_number()]} | {error, Reason :: term()}.
find_free_port(Count) ->
    case find_free_port_rec(Count, []) of
        {ok, SocketPortPairs} ->
            Ports = close_sockets_return_ports(SocketPortPairs),
            {ok, Ports};
        {error, Reason, SocketPortPairs} ->
            _ = close_sockets_return_ports(SocketPortPairs),
            {error, Reason}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec find_free_port_rec(Count :: non_neg_integer(), Acc :: [SocketPortPair]) -> Result when
  Result :: {ok, [SocketPortPair]} | {error, Reason :: term(), [SocketPortPair]},
  SocketPortPair :: {gen_tcp:socket(), inet:port_number()}.
find_free_port_rec(Count, Acc) when Count =< 0 -> {ok, Acc};
find_free_port_rec(Count, Acc) ->
    case gen_tcp:listen(0, []) of
        {ok, Socket} ->
            case inet:port(Socket) of
                {ok, Port} -> find_free_port_rec(Count - 1, [{Socket, Port}|Acc]);
                {error, Reason} -> {error, Reason, Acc}
            end;
        {error, Reason} -> {error, Reason, Acc}
    end.

-spec close_sockets_return_ports([{gen_tcp:socket(), inet:port_number()}]) -> [inet:port_number()].
close_sockets_return_ports(SocketPortPairs) ->
    lists:map(fun({Socket, Port}) ->
                      ok = gen_tcp:close(Socket),
                      Port
              end, SocketPortPairs).
