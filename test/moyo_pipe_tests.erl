%% coding: latin-1
%%
%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
%%
-module(moyo_pipe_tests).

-include("eunit.hrl").

output_start_test_() ->
    [
     {"ポートに固定データを出力し続ける",
      fun () ->
              Port = open_port({spawn, "cat"}, [binary]),
              Data = <<"pipe_data\n">>,

              _OutputPid = moyo_pipe:output_start(Port, Data, []),

              receive
                  {Port, {data, RecvData}} ->
                      ?assertMatch(<<"pipe_data\n", _/binary>>, RecvData)
              end
      end},
     {"パイププロセスに送られたメッセージは単に捨てられる",
      fun () ->
              %% moyo_pipeが不正なメッセージを受け取った際に出す警告を抑制しておく
              error_logger:delete_report_handler(error_logger_tty_h),

              Port = open_port({spawn, "cat"}, [binary]),
              Data = <<"pipe_data\n">>,

              OutputPid = moyo_pipe:output_start(Port, Data, []),

              ?assertEqual({message_queue_len, 0}, erlang:process_info(OutputPid, message_queue_len)),

              OutputPid ! hello,
              OutputPid ! world,

              timer:sleep(5), % `OutputPid'がメッセージを処理するまで待機する (sleepで調整するのはあまり良くない処理)
              ?assertEqual({message_queue_len, 0}, erlang:process_info(OutputPid, message_queue_len)), % キューにメッセージは溜まらない

              receive
                  {Port, {data, RecvData}} ->
                      ?assertMatch(<<"pipe_data\n", _/binary>>, RecvData)
              end
      end},
     {"ポートが閉じた場合は、パイププロセスも終了する",
      fun () ->
              Port = open_port({spawn, "cat"}, [binary, stderr_to_stdout]),
              Data = <<"pipe_data\n">>,

              OutputPid = moyo_pipe:output_start(Port, Data, []),
              Ref = monitor(process, OutputPid),
              timer:sleep(10),
              true = port_close(Port),
              ?assertDown(Ref, normal)
      end},
     {"パイププロセスが(異常)終了した場合は、ポートも閉じる",
      fun () ->
              process_flag(trap_exit, true),

              Port = open_port({spawn, "cat"}, [binary, stderr_to_stdout]),
              Data = <<"pipe_data\n">>,

              OutputPid = moyo_pipe:output_start(Port, Data, []),
              timer:sleep(10),

              exit(OutputPid, kill),
              receive
                  {'EXIT', Port, Reason} ->
                      ?assertEqual(killed, Reason)
              end
      end}
    ].
