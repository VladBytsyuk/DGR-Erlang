-module(server).
-export([start/1, stop/1, link/1]).


start(ServerName) -> 
    register(ServerName, spawn(fun() -> loop({[], []}) end)).

stop(ServerName) -> 
    ServerName ! stop.



link(ServerPid) ->
    serverPid ! {link, ServerPid},
    ServerPid ! {link, serverPid}.



loop(Data) -> 
    receive
        Message -> handleMessage(Message, Data)
    end.



handleMessage({ClientPid, stop}, _Data) -> {server_stopped, ClientPid};

handleMessage({ClientPid, set, Key, Value}, {ServersList, DataList}) ->
    NewDataList = lists:append([{Key, Value}], DataList),
    io:format("Set: ~p, ~p~n", [Key, Value]),
    ClientPid ! {set, Key, Value}, 
    loop({ServersList, NewDataList});

handleMessage({ClientPid, get, Key}, Data = {_ServersList, DataList}) ->
    Value = findValue(Key, DataList),
    io:format("Get: ~p, ~p~n", [Key, Value]),
    ClientPid ! {get, Key, Value},
    loop(Data);

handleMessage({link, ServerPid}, {ServersList, DataList}) ->
    NewServersList = lists:append([ServerPid], ServersList),
    loop({NewServersList, DataList}).



findValue(_Key, []) -> value_not_found;

findValue(Key, [{K, V} | _T]) when Key == K ->
    V;

findValue(Key, [{_K, _V} | T]) ->
    findValue(Key, T).

