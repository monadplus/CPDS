-module(control).
-export([go/2, first_worker/2, connect/2]).


random_list(N) -> random_list(N,N, []).
random_list(0, _, L) -> L;
random_list(N, M, L) -> random_list(N-1, M, [rand:uniform(M) | L]).

flush_mailbox() ->
    receive
      _Any -> flush_mailbox()
    after
      0 -> ok
    end.

go(N, M) ->
    flush_mailbox(),
    TargetList = random_list(M),
    io:format("~p~n", [TargetList]),
    FirstWorker = spawn(control, first_worker, [N-1, self()]),
    wait_all_connected(FirstWorker),
    FirstWorker ! {self(), token},
    ResultList = controlgame(TargetList, []),
    io:format("~w~n", [ResultList]),
    FirstWorker ! stop.

wait_all_connected(PID) ->
    receive PID2 when PID == PID2 -> start
    end.

controlgame([], ResultList) -> ResultList;
controlgame(TargetList, ResultList) ->
    receive
        {Pid, eat} -> io:format("~w eats~n", [Pid]),
                      [First|NewTargetList] = TargetList,
                      NewResultList = [{Pid, First}, ResultList],
                      controlgame(NewTargetList, NewResultList)
    end.

first_worker(N, PID) ->
    NEXT = spawn(control, connect, [N-1, self()]),
    receive
        connected -> PID ! self(),
        worker(NEXT)
    end.

connect(0, FIRST) ->
  FIRST ! connected,
  worker(FIRST);
connect(N, FIRST) ->
    NEXT = spawn(control, connect, [N-1, FIRST]),
    worker(NEXT).

worker(NEXT) ->
  receive
    {PARENT, token} ->
          PARENT ! {self(), eat},
          NEXT ! {PARENT, token},
          worker(NEXT);
    stop ->
      io:format("~w stopped~n", [self()]),
      NEXT ! stop,
      done
  end.
