-module(perm).
-compile(export_all).

%% perm :: [a] -> [[a]]
perm([]) -> [[]];
perm(L) ->  [[H|T] || H <- L, T <- perm(L--[H])].
