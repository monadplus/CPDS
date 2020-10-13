%% parallel quick sort
%% jpetit@lsi.upc.edu

-module(new_pqs).
-compile([export_all]).


    
    
%% quicksort sequencial
    
qs([]) -> [];
qs([H|T]) -> 
    LT = [X || X <- T, X < H],
    GE = [X || X <- T, X >= H],
    qs(LT) ++ [H] ++ qs(GE).

    
%% quicksort parallel
    
pqs(L) ->
    P = spawn(?MODULE, pqs2, [self(), L]),
    rcv(P).
    

pqs2(P, L) -> 
    if 
        length(L) < 100000 -> 
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
       

% Retorna el missatge enviat pel proces P.        
            
rcv(P) ->
    receive
        {P, X} -> X
    end.



% Retorna el temps que es triga en computar la funcio M:F sobre
% els parametres P. Es a dir, com un apply, pero en retorna el temps.
% (aquest impl passa dels mega-segons)

chrono(M, F, P) -> 
    {_, Seconds, Micros} = erlang:timestamp(),
    T1 = Seconds + (Micros/1000000.0),
    apply(M, F, P),    
    {_, Seconds2, Micros2} = erlang:timestamp(),
    T2 = Seconds2 + (Micros2/1000000.0),
    T2 - T1.

%% funcions de test

random_list(N) -> random_list(N, N, []).
random_list(0, _, L) -> L;
random_list(N, M, L) -> random_list(N-1, M, [random:uniform(M) | L]).

test_seq(N) ->
    L = random_list(N),
    chrono(?MODULE, qs, [L]).
    
test_par(N) ->
    L = random_list(N),
    chrono(?MODULE, pqs, [L]).
    
    
    
