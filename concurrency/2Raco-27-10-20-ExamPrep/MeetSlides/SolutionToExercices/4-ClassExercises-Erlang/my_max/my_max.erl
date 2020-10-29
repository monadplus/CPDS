-module(my_max).
-compile(export_all).

%% max of a list

my_max([H|T]) -> my_max(T, H).

my_max([H|T], Max) when H > Max -> my_max(T, H);
my_max([_|T], Max)              -> my_max(T, Max);
my_max([],    Max)              -> Max.


%% sep(L, N) returns {L1,L2} so that L1++L2 == L and length(L1)=N.

sep(L, 0) -> {[], L};
sep([H|T], N) ->
     {L1, L2} = sep(T, N-1),
     {[H|L1], L2}.



pmax(L)->
	if
         length(L) < 10 -> my_max(L);
         true ->
             {L1, L2} = sep(L, length(L) div 2),
		P1 = spawn(?MODULE, pmax2, [self(), L1]),
		P2 = spawn(?MODULE, pmax2, [self(), L2]),
		Max1= rcv(P1),
		Max2= rcv(P2),
		my_max([Max1,Max2])
	end.


pmax2(P, L)-> P!{self(), my_max(L)}.


% Retorna el missatge enviat pel procés P.




rcv(P) ->
     receive
         {P, X} -> X
     end.


% Retorna el temps que es triga en computar la funció M:F sobre
% els paràmetres P. És a dir, com un apply, però en retorna el temps.
% (aquest impl passa dels mega-segons)

chrono(M, F, P) ->
     {_, Seconds, Micros} = now(),
     T1 = Seconds + (Micros/1000000.0),
     apply(M, F, P),
     {_, Seconds2, Micros2} = now(),
     T2 = Seconds2 + (Micros2/1000000.0),
     T2 - T1.

%% funcions de test

random_list(N) -> random_list(N, N, []).
random_list(0, _, L) -> L;
random_list(N, M, L) -> random_list(N-1, M, [random:uniform(M) | L]).

test_seq(N) ->
     L = random_list(N),
     chrono(?MODULE, my_max, [L]).

test_par(N) ->
     L = random_list(N),
     chrono(?MODULE, pmax, [L]).






