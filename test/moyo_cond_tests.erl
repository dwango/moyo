%% @copyright 2013-2014 DWANGO Co., Ltd. All Rights Reserved.
-module(moyo_cond_tests).

-include_lib("eunit/include/eunit.hrl").

apply_if_test_() ->
    [
     {"条件節の値がtrueの場合は、THEN節で渡した関数が実行される",
      fun () ->
              ?assertEqual('then', moyo_cond:apply_if(true, fun () -> 'then' end, fun () -> 'else' end))
      end},
     {"条件節の値がfalseの場合は、ELSE節で渡した関数が実行される",
      fun () ->
              ?assertEqual('else', moyo_cond:apply_if(false, fun () -> 'then' end, fun () -> 'else' end))
      end}
    ].

apply_when_test_() ->
    [
     {"条件節の値がtrueの場合は、THEN節で渡した関数が実行される",
      fun () ->
              Msg = make_ref(),
              ?assertEqual(ok, moyo_cond:apply_when(true, fun () -> self() ! Msg end)),
              receive
                  Msg -> ?assert(true)
              after 0 -> ?assert(false)
              end
      end},
     {"条件節の値がfalseの場合は、何も処理は実行されない",
      fun () ->
              Msg = make_ref(),
              ?assertEqual(ok, moyo_cond:apply_when(false, fun () -> self() ! Msg end)),
              receive
                  Msg -> ?assert(false)
              after 0 -> ?assert(true)
              end
      end}
    ].

apply_unless_test_() ->
    [
     {"条件節の値がfalseの場合は、THEN節で渡した関数が実行される",
      fun () ->
              Msg = make_ref(),
              ?assertEqual(ok, moyo_cond:apply_unless(false, fun () -> self() ! Msg end)),
              receive
                  Msg -> ?assert(true)
              after 0 -> ?assert(false)
              end
      end},
     {"条件節の値がtrueの場合は、何も処理は実行されない",
      fun () ->
              Msg = make_ref(),
              ?assertEqual(ok, moyo_cond:apply_unless(true, fun () -> self() ! Msg end)),
              receive
                  Msg -> ?assert(false)
              after 0 -> ?assert(true)
              end
      end}
    ].

conditional_test_() ->
  [
    {"booleanに対応したintegerを返す",
      fun() ->
        A = 1,
        B = 2,
        ?assertEqual(A, moyo_cond:conditional(true, A, B)),
        ?assertEqual(B, moyo_cond:conditional(false, A, B))
      end
    },
    {"booleanに対応したlistを返す",
      fun() ->
        A = [1, 2, 3],
        B = [4, 5, 6],
        ?assertEqual(A, moyo_cond:conditional(true, A, B)),
        ?assertEqual(B, moyo_cond:conditional(false, A, B))
      end
    },
    {"booleanに対応したfunctionを返す",
      fun() ->
        A = fun (X) -> X * 2 end,
        B = fun (Y) -> Y * 2 end,
        ?assertEqual(A, moyo_cond:conditional(true, A, B)),
        ?assertEqual(B, moyo_cond:conditional(false, A, B))
      end
    }
  ].

while_test_() ->
  [
    {"数字の累進",
      fun() ->
        Fun = fun ({Cur, _, Dst}) when Cur > Dst -> {false, Cur};
                  ({Cur, Step, Dst}) -> {true, {Cur + Step, Step, Dst}} end,
        ?assertEqual(6, moyo_cond:while(Fun, {1, 1, 5})),
        ?assertEqual(22, moyo_cond:while(Fun, {10, 2, 20}))
      end
    },
    {"別関数の実行",
      fun() ->
        FunA = fun ({_, Cur, Dst}) when Cur > Dst -> {false, Cur};
                  ({Fun, Cur, Dst}) -> {true, {Fun, Fun(Cur), Dst}} end,
        FunB = fun (Num) -> Num + 1 end,
        ?assertEqual(6, moyo_cond:while(FunA, {FunB, 1, 5}))
      end
    }
  ].
