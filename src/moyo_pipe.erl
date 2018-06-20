%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
%% @doc ポート(外部コマンド)に対するパイプ入出力機能を提供するためのモジュール
%%
%% 現在は出力機能にのみ対応済み
-module(moyo_pipe).

-include("moyo_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         output_start/3,
         output_start/4
        ]).

-export_type([
              output_option/0,
              output_data_generate_fun/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------

-type output_option() :: {interval, moyo_clock:non_neg_milliseconds()}.
%% interval: 各出力データ送信後にスリープする時間(ミリ秒).  デフォルト値は 10.

-type output_data_generate_fun() :: fun ((State::term()) -> {ok, OutputData::iodata(), NextState::term()} | stop).

%%----------------------------------------------------------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------------------------------------------------------
-record(output_state,
        {
          port                :: port(),
          data_generate_state :: term(),
          data_generate_fun   :: output_data_generate_fun(),
          interval   = 10     :: moyo_clock:non_neg_milliseconds()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 指定ポートに対して固定データを出力し続けるプロセスを生成する.
%%
%% `output_start(Port, fun (State) -> {ok, Data, State} end, InitialState, Options)'と等価なので、詳細はそちらのドキュメントを参照. <br />
-spec output_start(port(), Data, Options) -> OutputProcessId when
      Data            :: iodata(),
      Options         :: [output_option()],
      OutputProcessId :: pid().
output_start(Port, Data, Options) ->
    output_start(Port, fun (State) -> {ok, Data, State} end, undefined, Options).

%% @doc 指定ポートに対してデータ出力を行い続けるプロセスを生成する.
%%
%% 生成されたプロセスは、ポートの実行終了に伴い、自動で終了する. <br />
%% また`DataGenerateFun'が`stop'を返した場合もプロセスは終了する (この際にポートの停止は行われない). <br />
-spec output_start(port(), DataGenerateFun, InitialState, Options) -> OutputProcessId when
      DataGenerateFun :: output_data_generate_fun(),
      InitialState    :: term(),
      Options         :: [output_option()],
      OutputProcessId :: pid().
output_start(Port, DataGenerateFun, InitialState, Options) ->
    case parse_output_options(Options) of
        {error, Reason} -> error({wrong_options, Reason});
        {ok, Options2}  ->
            OutputLoopState =
                #output_state{
                   port                = Port,
                   data_generate_state = InitialState,
                   data_generate_fun   = DataGenerateFun,
                   interval            = moyo_assoc:fetch(interval, Options2)
                  },
            spawn(fun () -> output_loop_start(OutputLoopState) end)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Inner Function
%%----------------------------------------------------------------------------------------------------------------------
-spec output_loop_start(#output_state{}) -> no_return().
output_loop_start(State) ->
    true = link(State#output_state.port),
    output_loop(State).

-spec output_loop(#output_state{}) -> no_return().
output_loop(State) ->
    receive
        Msg ->
            %% キューが溜まらないように、受信したメッセージは破棄する (そもそもメッセージ受信は意図していない)
            ok = error_logger:error_msg("unknown message: ~p~n", [Msg]),
            output_loop(State)
    after 0 ->
            %% ポートの標準入力にデータを送る処理
            #output_state{port = Port, interval = Interval,
                          data_generate_state = GenState, data_generate_fun = GenFun} = State,
            case GenFun(GenState) of
                stop ->
                    exit(normal);
                {ok, Data, GenState2} ->
                    try
                        true = port_command(Port, Data)
                    catch
                        error:badarg ?CAPTURE_STACKTRACE ->
                            case erlang:port_info(Port) of
                                undefined ->
                                    %% ポートが閉じている(コマンドの実行が終了している)ので、出力プロセスも終了
                                    exit(normal);
                                _ ->
                                    erlang:raise(error, badarg, ?GET_STACKTRACE)
                            end
                    end,
                    ok = timer:sleep(Interval),
                    State2 = State#output_state{data_generate_state = GenState2},
                    output_loop(State2)
            end
    end.

-spec parse_output_options([output_option()]) -> {ok, [output_option()]} | {error, Reason::term()}.
parse_output_options(Options) ->
    Spec = [
            {interval, {integer, [non_negative]}, [{default, 10}]}
           ],
    moyo_assoc:lookup_entries_as(Spec, Options).
