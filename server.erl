-module(server).
-export([start/0, stop/0, link/1]).


start() -> 
    register(serverPid, spawn(fun() -> 
        loop({
                _LinkedServers = oset:new(), 
                _Data = [], 
            
                % Число серверов - не нужно -> оно вычисляется как LinkedServers.size()
                % Число элементов - не нужно -> оно вычисляется как Data.size()
                %
                _Config = {
                    % Вектор частоты использования каждого объекта на этом сервере
                    _Ri = []
                }
        }) 
    end)).

stop() -> 
    serverPid ! stop.



link(ServerName) ->
    net_kernel:connect_node(ServerName),
    {serverPid, ServerName} ! {link, node()},
    serverPid ! {link, ServerName}.



loop(Data) -> 
    receive
        Message -> handleClientMessage(Message, Data)
    end.

%% ================================================================================================
%%
%%  Handle client messages
%%
%% ================================================================================================

% Сообщение об остановке сервера
handleClientMessage(stop, _Data) -> 
    server_stopped;

% Сообщение о сохранении пары ключ-значение на сервере
handleClientMessage({ClientPid, set, Key, Value}, {ServersList, DataList, Config}) ->
    Number = findNewObjectNumber(0, ServersList, oset:new()),
    NewDataList = lists:append([{Key, Value, Number}], DataList),
    ClientPid ! {set, Key, Value}, 
    loop({ServersList, NewDataList, Config});

% Сообщение о запросе значения по ключу от клиента
handleClientMessage({ClientPid, get, Key}, Data = {ServersList, DataList, _Config}) ->
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

% Сообщение о привязке текущего сервера к другому
handleClientMessage({link, ServerName}, {ServersList, DataList, Config}) ->
    NewServersList = oset:add_element(ServerName, ServersList),
    loop({NewServersList, DataList, Config});


%% ================================================================================================
%%
%%  Handle distributed messages
%%
%% ================================================================================================

% Сообщение о поиске элемента другим сервером на этом и его соседях
handleClientMessage({find, Key, ServerName, UsedServersList}, Data = {ServersList, DataList, _Config}) ->
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

% Сообщение о поиске максимального номера объекта
handleClientMessage({object_max, ServerName, UsedServers}, Data = {ServersList, DataList, _Config}) ->
    LocalMax = findMaxObjectNumberLocal(DataList),
    Max = findNewObjectNumber(LocalMax, ServersList, UsedServers),
    {serverPid, ServerName} ! {object_max, Max},
    loop(Data).

%% ================================================================================================
%%
%%  Utils functions
%%
%% ================================================================================================

% Ищет значение по ключу на текущем сервере
findValue(_Key, []) -> 
    value_not_found;

findValue(Key, [{K, V} | _T]) when Key == K ->
    V;

findValue(Key, [{_K, _V} | T]) ->
    findValue(Key, T).

% Находит максимальный номер объекта на текущем сервере
findMaxObjectNumberLocal(_DataList = []) -> 0;
findMaxObjectNumberLocal(_DataList = [H | T]) ->
    {_Key, _Value, Number} = H,
    RecursiveMax = findMaxObjectNumberLocal(T),
    case Number > RecursiveMax of
        true -> Number;
        false -> RecursiveMax
    end.

% Находит значение по ключу на всех серверах рекурсивно
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

% Находит максимальный номер объекта среди всех объектов на соседних серверах рекурсивно 
findNewObjectNumber(Max, _ServersList = [], _UsedServers) -> Max;
findNewObjectNumber(Max, _ServersList = [H | T], UsedServers) ->
    NewUsedServers = oset:add_element(node(), UsedServers),
    case oset:is_element(H, NewUsedServers) of
        true -> findNewObjectNumber(Max, T, NewUsedServers);
        false -> 
            {serverPid, H} ! {object_max, node(), NewUsedServers},
            receive
                {object_max, NewMax} -> 
                    case Max < NewMax of
                        true  -> findNewObjectNumber(NewMax, T, NewUsedServers);
                        false -> findNewObjectNumber(Max, T, NewUsedServers)
                    end
            end
    end.



% Находим максимальную InsertionGain среди всех серверов и его номер
findMaxInsertionGain(Data, [], _UsedServersList) -> Data;

findMaxInsertionGain(Data = {InsertionGain, _I, _J, _EvictJ}, _ServersList = [H | T], UsedServersList) ->
        NewUsedServersList = oset:add_element(node(), UsedServersList),
        case oset:is_element(H, NewUsedServersList) of
            true -> findMaxInsertionGain(Data, T, NewUsedServersList);
            false -> 
                {serverPid, H} ! {ig_max, node(), InsertionGain, NewUsedServersList},
                receive
                    {ig_max, NewData = {NewInsertionGain, _NewI, _NewJ, _NewEvictJ}} -> 
                        case NewInsertionGain > InsertionGain of
                            true ->  findMaxInsertionGain(NewData, T, NewUsedServersList);
                            false -> findMaxInsertionGain(Data, T, NewUsedServersList)
                        end
                end
        end.