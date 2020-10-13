
-module(a).
-compile(export_all).

start(Tag) ->
     spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    % expects: c(b).
    Val = b:x(),
    io:format("Version 1 (~p) b:x() = ~p ~n", [Tag, Val]),
    loop(Tag).

sleep() ->
    receive
      after 5000 -> true
    end.
