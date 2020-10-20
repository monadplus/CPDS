
-module(mydict).
-compile(export_all).

dict1() -> dict:store(2, "B", dict:store(1, "A", dict:new())).

get(K, D) -> dict:find(K, D).
