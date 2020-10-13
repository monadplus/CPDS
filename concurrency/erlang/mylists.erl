-module(mylists).
-compile(export_all).
%% -export([add/2]).

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
