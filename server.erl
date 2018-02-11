-module(server).
-export([start/0, stop/0, link/1]).


start() -> 
    register(serverPid, spawn(fun() -> loop({oset:new(), []}) end)).

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



handleMessage(stop, _Data) -> 
    server_stopped;

handleMessage({ClientPid, set, Key, Value}, {ServersList, DataList}) ->
    NewDataList = lists:append([{Key, Value}], DataList),
    ClientPid ! {set, Key, Value}, 
    loop({ServersList, NewDataList});

handleMessage({ClientPid, get, Key}, Data = {ServersList, DataList}) ->
    Value = findValue(Key, DataList),
    if 
        Value == value_not_found -> 
            io:format("Value not found~n", []),
            NewValue = findValueOnOtherServers(Key, oset:to_list(ServersList), oset:new()),
            io:format("Value from remote server: ~p~n", [NewValue]),
            ClientPid ! {get, Key, NewValue};
        true -> ClientPid ! {get, Key, Value}
    end,
    loop(Data);

handleMessage({find, Key, ServerName, UsedServersList}, Data = {ServersList, DataList}) ->
        Value = findValue(Key, DataList),
        if 
            Value == value_not_found ->
                NewValue = findValueOnOtherServers(Key, oset:to_list(ServersList), UsedServersList),
                if 
                    NewValue == value_not_found ->
                        {serverPid, ServerName} ! {value_not_found, UsedServersList};
                    true ->
                        {serverPid, ServerName} ! {found, Key, NewValue}
                end;
            true -> 
                {serverPid, ServerName} ! {found, Key, Value}
        end,
        loop(Data);

handleMessage({link, ServerName}, {ServersList, DataList}) ->
    NewServersList = oset:add_element(ServerName, ServersList),
    loop({NewServersList, DataList}).



findValue(_Key, []) -> 
    value_not_found;

findValue(Key, [{K, V} | _T]) when Key == K ->
    V;

findValue(Key, [{_K, _V} | T]) ->
    findValue(Key, T).

findValueOnOtherServers(_Key, [], _UsedServersList) ->
    value_not_found;
findValueOnOtherServers(Key, _ServersList = [H | T], UsedServersList) ->
    NewUsedServersList = oset:add_element(node(), UsedServersList),
    case oset:is_element(H, NewUsedServersList) of
        true -> 
            findValueOnOtherServers(Key, T, NewUsedServersList);
        false -> 
            {serverPid, H} ! {find, Key, node(), NewUsedServersList},
            receive
                {value_not_found, UpdatedUsedServersList} -> 
                    findValueOnOtherServers(Key, T, UpdatedUsedServersList);
                {found, _Key, Value} -> 
                    Value
            end
    end.
