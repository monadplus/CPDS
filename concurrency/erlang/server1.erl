
-module(server1).
-export([start/2, rpc/2]).

%% > server1:start(name_server, name_server).
%% true
%% > name_server:add(joe, "at home").
%% ok
%% name_server:whereis(joe).
%% {ok, "at home"}

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, Response} ->
             Response
    end.

loop(Name, Mod, State) ->
    receive
        {From, Request} ->
            {Response,State1} = Mod:handle(Request, State),
            From ! {Name, Response},
            loop(Name, Mod, State1)
    end.
