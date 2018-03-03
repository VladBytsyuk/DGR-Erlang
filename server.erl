-module(server).
-export([start/1, stop/0, slink/1]).

% Macro
-define(IF(Condition, True, False), (case (Condition) of true -> (True); false -> (False) end)).
-define(MAX(A, B), (case (A > B) of true -> A; false -> B end)).

% Exported functions
start(Number) -> 
    register(serverPid, spawn(fun() -> 
        loop({_LinkedServers = oset:new(), 
                _Data = oset:new(), 
                _Config = {
                    Number,
                    % Вектор частоты использования каждого объекта на этом сервере
                    _Ri = []
                }
        }) 
    end)).

stop() -> 
    serverPid ! stop.

slink(ServerName) ->
    net_kernel:connect_node(ServerName),
    {serverPid, ServerName} ! {link, node()},
    serverPid ! {link, ServerName}.


% Main loop
loop(State) -> 
    printState(State),
    receive
        Message -> handleMessage(Message, State)
    end.

printState(State) ->
    {Servers, Data, {I, Ri}} = State,
    io:format("~n", []),
    io:format("State:~n", []),
    io:format(" Servers: ~p~n", [oset:to_list(Servers)]),
    io:format(" Data: ~p~n", [oset:to_list(Data)]),
    io:format(" Config:~n", []),
    io:format("     Server number: ~p~n", [I]),
    io:format("     Ri: ~p~n", [Ri]),
    io:format("~n", []).

%% ================================================================================================
%%
%%  Handle messages
%%
%% ================================================================================================

% Current server was stopped
handleMessage(stop, _State = {Servers, _Data, _Config}) -> 
    io:format("Stop~n",[]),
    notifyStop(oset:to_list(Servers), oset:new(), node());

% Remote server was stopped
handleMessage({s_stopped, Pid, StoppedName, UsedServers}, _State = {Servers, Data, Config}) ->
    NewServers = oset:del_element(StoppedName, Servers),
    {ok, notifyStop, StoppedName, UpdatedUsedServers} = notifyStop(oset:to_list(NewServers), UsedServers, StoppedName),
    Pid ! {s_stopped, UpdatedUsedServers},
    loop({NewServers, Data, Config});

% Set data from client
handleMessage({c_set, Pid, Key, Value}, State = {Servers, _Data, _Config}) ->
    io:format("Client -> Set {~p, ~p}~n", [Key, Value]),
    Object = findObject(Key, State),
    io:format("Object in system - ~p~n", [Object]),
    {NewObject, NewData, NewConfig} = tryToAddObject(Object, State, Key, Value),
    Pid ! {c_set, NewObject}, 
    loop({Servers, NewData, NewConfig});

% Get data to client
handleMessage({c_get, Pid, Key}, State = {Servers, Data, Config}) ->
    io:format("Client -> Get {~p}~n", [Key]),
    Object = findObject(Key, State),
    case Object of 
        value_not_found ->
            Pid ! {c_get, value_not_found},
            loop(State);
        {_K, _V, N} ->
            { I, Ri } = Config,    
            OldRiN = getListItem(N, Ri),
            NewRi = replaceListItem(N, OldRiN + 1, Ri),
            NewConfig = { I, NewRi },
            Pid ! {c_get, Object},
            loop({Servers, Data, NewConfig})
    end;

handleMessage({s_get, ServerPid, Key, UsedServers}, State = {Servers, Data, _Config}) ->
    LocalObject = findObjectLocal(Key, oset:to_list(Data)),
    case LocalObject == value_not_found of
        true  -> 
            NewUsedServers = oset:add_element(node(), UsedServers),
            {RemoteObject, UpdatedUsedServers} = findObjectRemote(Key, oset:to_list(Servers), NewUsedServers),
            ServerPid ! {s_get, RemoteObject, UpdatedUsedServers};
        false -> ServerPid ! {s_get, LocalObject, UsedServers}
    end,
    loop(State);

% Update server config on adding object
handleMessage({s_obj_add, Pid,  UsedServers}, _State = {Servers, Data, _Config = { I, Ri }}) ->
    NewConfig = { I, lists:append(Ri, [0]) },
    {ok, notifyObjectAdded, NewUsedServers} = notifyObjectAdded(oset:to_list(Servers), UsedServers),
    Pid ! {s_obj_add, NewUsedServers},
    loop({Servers, Data, NewConfig});

% Find maximum of objects naumber
handleMessage({s_max_num, ServerPid, Max, UsedServers}, State = {Servers, Data, _Config}) ->
    LocalMax  = findMaxNumberLocal(Max, oset:to_list(Data)),
    {RemoteMax, UpdatedUsedServers} = findMaxNumberRemote(Max, oset:to_list(Servers), UsedServers),
    RealMax = ?MAX(LocalMax, RemoteMax),
    ServerPid ! {s_max_num, RealMax, UpdatedUsedServers},
    loop(State);

% Link servers with each other
handleMessage({link, ServerName}, _State = {ServersList, DataList, Config}) ->
    NewServersList = oset:add_element(ServerName, ServersList),
    loop({NewServersList, DataList, Config}).


%% ================================================================================================
%%
%%  Utils functions
%%
%% ================================================================================================

% Notify all neighbor-servers that current server was stopped
notifyStop(_ServersList = [], UsedServers, StoppedName) -> {ok, notifyStop, StoppedName, UsedServers};
notifyStop(_ServersList = [H | T], UsedServers, StoppedName) ->
    case oset:is_element(H, UsedServers) of
        true  -> notifyStop(T, UsedServers, StoppedName);
        false -> 
            NewUsedServers = oset:add_element(H, UsedServers),
            {serverPid, H} ! {s_stopped, {serverPid, node()}, StoppedName, NewUsedServers},
            receive 
                {s_stopped, UpdatedUsedServers} -> notifyStop(T, UpdatedUsedServers, StoppedName)
            end
    end.


% Notify all servers that new object was added
notifyObjectAdded(_State = {Servers, _Data, _Config}) ->
    EmptyOset = oset:new(),
    UsedServers = oset:add_element(node(), EmptyOset),
    notifyObjectAdded(oset:to_list(Servers), UsedServers).
notifyObjectAdded(_ServersList = [], UsedServers) -> {ok, notifyObjectAdded, UsedServers};
notifyObjectAdded(_ServersList = [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of 
        true  -> notifyObjectAdded(T, UsedServers);
        false ->
            NewUsedServers = oset:add_element(H, UsedServers),
            {serverPid, H} ! {s_obj_add, {serverPid, node()}, NewUsedServers},
            receive
                {s_obj_add, UpdatedUsedServers} -> notifyObjectAdded(T, UpdatedUsedServers)
            end
    end.


% Find object on local (or remote) server
findObject(Key, {Servers, Data, _Config}) ->
    io:format("Trying to find value for key = \"~p\"~n", [Key]),
    LocalObject = findObjectLocal(Key, oset:to_list(Data)),
    io:format("Result of local search: ~p~n", [LocalObject]),
    case LocalObject == value_not_found of
        true  -> 
            io:format("Need to find on other servers: ~p~n", [oset:to_list(Servers)]),
            {RemoteObject, _UsedServers} = findObjectRemote(Key, Servers),
            io:format("Result of remote search: ~p~n", [RemoteObject]),
            RemoteObject;
        false -> LocalObject
    end.

findObjectLocal(_Key, []) -> value_not_found;
findObjectLocal(Key, [{Key, V, N} | _T]) -> {Key, V, N};
findObjectLocal(Key, [{_K, _V, _N} | T]) -> findObjectLocal(Key, T).

findObjectRemote(Key, Servers) -> findObjectRemote(Key, oset:to_list(Servers), oset:new()).
findObjectRemote(_Key, [], UsedServers) -> {value_not_found, UsedServers};
findObjectRemote(Key, _ServersList = [H | T], UsedServers) ->
    NewUsedServers = oset:add_element(node(), UsedServers),
    case oset:is_element(H, NewUsedServers) of
        true  -> findObjectRemote(Key, T, UsedServers);
        false ->
            UpdatedUsedServers = oset:add_element(H, NewUsedServers),
            {serverPid, H} ! {s_get, {serverPid, node()}, Key, UpdatedUsedServers},
            receive
                {s_get, value_not_found, NewUpdatedUsedServers} -> findObjectRemote(Key, T, NewUpdatedUsedServers);
                {s_get, Object, NewUpdatedUsedServers} -> {Object, NewUpdatedUsedServers}
            end
    end.


% Find max number of object (for creating new number when object was added)
findMaxNumber(Max, ServersList, UsedServers, Data) ->
    LocalMax  = findMaxNumberLocal(Max, oset:to_list(Data)),
    {RemoteMax, _UsedServers} = findMaxNumberRemote(Max, ServersList, UsedServers),
    ?MAX(LocalMax, RemoteMax).

findMaxNumberLocal(Max, _DataList = []) -> Max;
findMaxNumberLocal(Max, _DataList = [{_K, _V, N} | T]) when N > Max -> findMaxNumberLocal(N, T);
findMaxNumberLocal(Max, _DataList = [_H | T]) -> findMaxNumberLocal(Max, T).

findMaxNumberRemote(Max, _ServersList = [], UsedServers) -> {Max, UsedServers};
findMaxNumberRemote(Max, _ServersList = [H | T], UsedServers) ->
    NewUsedServers = oset:add_element(node(), UsedServers),
    case oset:is_element(H, NewUsedServers) of
        true  -> findMaxNumberRemote(Max, T, NewUsedServers);
        false ->
            UpdatedUsedServers = oset:add_element(H, NewUsedServers),
            {serverPid, H} ! {s_max_num, {serverPid, node()}, Max, UpdatedUsedServers},
            receive
                {s_max_num, NewMax, NewUpdatedUsedServers} -> 
                    findMaxNumberRemote(?MAX(Max, NewMax), T, NewUpdatedUsedServers)    
            end
    end.


% Replaced list item by Index, on Value in List
replaceListItem(0, Value, _List = [_H | T]) -> [Value] ++ T;
replaceListItem(Index, Value, _List = [H | T]) -> [H] ++ replaceListItem(Index - 1, Value, T).


% Forms new (Updates old) object and put it into Data
tryToAddObject(value_not_found, State = {Servers, Data, _Config = { I, Ri }}, Key, Value) ->
    Number = findMaxNumber(-1, oset:to_list(Servers), oset:new(), Data) + 1,
    Object = {Key, Value, Number},
    NewData = oset:add_element(Object, Data),
    notifyObjectAdded(State),
    NewConfig = { I, lists:append(Ri, [0]) },
    {Object, NewData, NewConfig};
tryToAddObject(Object, _State = {_Servers, Data, Config}, _Key, Value) ->
    NewData = oset:add_element(Object, Data),
    {K, _V, N} = Object,
    NewObject = {K, Value, N},
    {NewObject, NewData, Config}.

% Get list item by index
getListItem(_Index, _List = []) -> {error, index_greater_than_list_size};
getListItem(0, _List = [H | _T]) -> H;
getListItem(Index, _List = [_H | T]) -> getListItem(Index - 1, T).
