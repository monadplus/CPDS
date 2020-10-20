-module(quicksort).
-compile(export_all).

%% qs :: [a] -> [a]
qs([]) -> [];
qs([H|T]) ->
    LT = [X || X <- T, X < H],
    GE = [X || X <- T, X >= H],
    qs(LT) ++ [H] ++ qs(GE).

%% parallel qs
%% ?MODULE expands to the module name i.e. quicksort
pqs(L) ->
  P = spawn(?MODULE, psq2, [self(), L]),
  rcv(P).

rcv(P) -> receive {P, X} -> X end.

pqs2(P, L) ->
    if  length(L) < 100000 ->
            P ! {self(), qs(L)};
        true ->
            [H | T] = L,
            LT = [X || X <- T, X < H],
            GE = [X || X <- T, X >= H],
            P1 = spawn(?MODULE, pqs2, [self(), LT]),
            P2 = spawn(?MODULE, pqs2, [self(), GE]),
            L1 = rcv(P1),
            L2 = rcv(P2),
            P ! {self(), L1 ++ [H] ++ L2}
    end.
