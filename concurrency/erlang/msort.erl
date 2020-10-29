-module(msort).
-compile(export_all).

sep(L, 0) -> {[], L};
sep([], _) -> {[], []};
sep([H|T], N) ->
    {L1, L2} = sep(T, N-1),
    {[H|L1], L2}.

merge([], YS) -> YS;
merge(XS, []) -> XS;
merge([X|XS],[Y|YS]) ->
    if
       X =< Y -> [X | merge(XS, [Y|YS])];
       true   -> [Y | merge([X|XS], YS)]
    end.

ms([]) -> [];
ms([A]) -> [A];
ms(L) ->
    {L1, L2}  = sep(L, length(L) div 2),
    SL1 = ms(L1),
    SL2 = ms(L2),
    merge(SL1, SL2).

rcvp(PID) ->
  receive {PID, L} ->
    L
  end.

pms(L) ->
    PID = spawn(?MODULE, p_ms, [self(), L]),
    rcvp(PID).

p_ms(PID, L) when length(L) < 100 -> PID ! {self(), ms(L)};
p_ms(PID, L) ->
    {LEFT, RIGHT}  = sep(L, length(L) div 2),
    PID1 = spawn(?MODULE, p_ms, [self(), LEFT]),
    PID2 = spawn(?MODULE, p_ms, [self(), RIGHT]),
    L1 = rcvp(PID1),
    L2 = rcvp(PID2),
    PID ! {self(), merge(L1, L2)}.
