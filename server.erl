-module(server).
-export([start/0, stop/0, link/1]).


start() -> 
    register(serverPid, spawn(fun() -> loop({[], []}) end)).

stop() -> 
    serverPid ! stop.



link(ServerName) ->
    net_kernel:connect_node(ServerName),
    {serverPid, ServerName} ! {link, node()},
    serverPid ! {link, ServerName}.



loop(Data) -> 
    receive
        Message -> handleMessage(Message, Data)
    end.



handleMessage(stop, _Data) -> server_stopped;

handleMessage({ClientPid, set, Key, Value}, {ServersList, DataList}) ->
    NewDataList = lists:append([{Key, Value}], DataList),
    io:format("~p Set: ~p, ~p~n", [ClientPid, Key, Value]),
    ClientPid ! {set, Key, Value}, 
    loop({ServersList, NewDataList});

handleMessage({ClientPid, get, Key}, Data = {_ServersList, DataList}) ->
    Value = findValue(Key, DataList),
    io:format("~p Get: ~p, ~p~n", [ClientPid, Key, Value]),
    ClientPid ! {get, Key, Value},
    loop(Data);

handleMessage({link, ServerName}, {ServersList, DataList}) ->
    NewServersList = lists:append([ServerName], ServersList),
    loop({NewServersList, DataList}).



findValue(_Key, []) -> value_not_found;

findValue(Key, [{K, V} | _T]) when Key == K ->
    V;

findValue(Key, [{_K, _V} | T]) ->
    findValue(Key, T).

