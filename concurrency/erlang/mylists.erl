-module(mylists).
-compile(export_all).
%% -export([add/2]).

id(X) -> X.

concat([X|XS], YS) -> [X | concat(XS, YS)];
concat([], YS) -> YS.

sum ([H|T]) -> H + sum(T);
sum ([]) -> 0.

map (_, []) -> [];
map (F, [H|T]) -> [F(H)| map(F, T)].

member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H,T);
member(_, []) -> false.

reverse(L) -> reverse(L, []).
reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L) -> L.

sep(L, 0) -> {[], L};
sep([], _) -> {[], []};
sep([H|T], N) ->
    {L1, L2} = sep(T, N-1),
    {[H|L1], L2}.

random_list(N) -> random_list(N,N, []).
random_list(0, _, L) -> L;
random_list(N, M, L) -> random_list(N-1, M, [rand:uniform(M) | L]).
