-module(server).
-export([start/2, stop/0, slink/1]).

% Macro
-define(IF(Condition, True, False), (case (Condition) of true -> (True); false -> (False) end)).
-define(MAX(A, B), (case (A > B) of true -> A; false -> B end)).

% Exported functions
start(Number, Capacity) -> 
    register(serverPid, spawn(fun() -> 
        loop({_LinkedServers = oset:new(), 
                _Data = oset:new(), 
                _Config = {
                    % Номер сервера в сети
                    Number,
                    % Ёмкость сервера (количество объектов, которые сервер может сохранить)
                    Capacity,
                    % Оставшееся место на сервере
                    Capacity,
                    % Вектор частоты использования каждого объекта на этом сервере
                    _Ri = [],
                    % Вектор реплик объектов
                    _Xi = []
                }
        }) 
    end)).

stop() -> serverPid ! stop.

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
    {Servers, Data, _Config = {I, C, E, Ri, Xi}} = State,
    io:format("~n", []),
    io:format("State:~n", []),
    io:format(" Servers: ~p~n", [oset:to_list(Servers)]),
    io:format(" Data: ~p~n", [oset:to_list(Data)]),
    io:format(" Config:~n", []),
    io:format("     Server number: ~p~n", [I]),
    io:format("     Server capacity: ~p~n", [C]),
    io:format("     Server space left: ~p~n", [E]),
    io:format("     Ri: ~p~n", [Ri]),
    io:format("     Xi: ~p~n", [Xi]),
    io:format("~n", []).

%% ================================================================================================
%%
%%  Handle messages
%%
%% ================================================================================================

% Current server was stopped
handleMessage(stop, _State = {Servers, _Data, _Config}) -> 
    io:format("Stop~n",[]),
    s_utils:notifyStop(oset:to_list(Servers), oset:new(), node());

% Remote server was stopped
handleMessage({s_stopped, Pid, StoppedName, UsedServers}, _State = {Servers, Data, Config}) ->
    NewServers = oset:del_element(StoppedName, Servers),
    {ok, notifyStop, StoppedName, UpdatedUsedServers} = s_utils:notifyStop(oset:to_list(NewServers), UsedServers, StoppedName),
    Pid ! {s_stopped, UpdatedUsedServers},
    loop({NewServers, Data, Config});

% Set data from client
handleMessage({c_set, Pid, Key, Value}, State = {Servers, _Data, _Config}) ->
    io:format("Client -> Set {~p, ~p}~n", [Key, Value]),
    Object = s_utils:findObject(Key, State),
    io:format("Object in system - ~p~n", [Object]),
    {NewObject, NewData, NewConfig} = s_utils:tryToAddObject(Object, State, Key, Value),
    Pid ! {c_set, NewObject}, 
    NewState = {Servers, NewData, NewConfig},%dgr:dgr({Servers, NewData, NewConfig}),
    loop(NewState);

% Get data to client
handleMessage({c_get, Pid, Key}, State = {Servers, Data, Config}) ->
    io:format("Client -> Get {~p}~n", [Key]),
    Object = s_utils:findObject(Key, State),
    case Object of 
        value_not_found ->
            Pid ! {c_get, value_not_found},
            loop(State);
        {_K, _V, N} ->
            { I, C, E, Ri, Xi } = Config,    
            OldRiN = s_utils:getListItem(N, Ri),
            NewRi = s_utils:replaceListItem(N, OldRiN + 1, Ri),
            NewConfig = { I, C, E, NewRi, Xi },
            Pid ! {c_get, Object},
            NewState = {Servers, Data, NewConfig},%dgr:dgr({Servers, Data, NewConfig}),
            loop(NewState)
    end;

handleMessage({s_get, ServerPid, Key, UsedServers}, State = {Servers, Data, _Config}) ->
    LocalObject = s_utils:findObjectLocal(Key, oset:to_list(Data)),
    case LocalObject == value_not_found of
        true  -> 
            NewUsedServers = oset:add_element(node(), UsedServers),
            {RemoteObject, UpdatedUsedServers} = s_utils:findObjectRemote(Key, oset:to_list(Servers), NewUsedServers),
            ServerPid ! {s_get, RemoteObject, UpdatedUsedServers};
        false -> ServerPid ! {s_get, LocalObject, UsedServers}
    end,
    loop(State);

% Update server config on adding object
handleMessage({s_obj_add, Pid,  UsedServers}, _State = {Servers, Data, _Config = {I, C, E, Ri, Xi}}) ->
    NewConfig = { I, C, E, Ri ++ [0], Xi ++ [0]},
    {ok, notifyObjectAdded, NewUsedServers} = s_utils:notifyObjectAdded(oset:to_list(Servers), UsedServers),
    Pid ! {s_obj_add, NewUsedServers},
    loop({Servers, Data, NewConfig});

% Find maximum of objects naumber
handleMessage({s_max_num, ServerPid, Max, UsedServers}, State = {Servers, Data, _Config}) ->
    LocalMax  = s_utils:findMaxNumberLocal(Max, oset:to_list(Data)),
    {RemoteMax, UpdatedUsedServers} = s_utils:findMaxNumberRemote(Max, oset:to_list(Servers), UsedServers),
    RealMax = ?MAX(LocalMax, RemoteMax),
    ServerPid ! {s_max_num, RealMax, UpdatedUsedServers},
    loop(State);

handleMessage({d_popul, Pid, List, UsedServers}, State = {Servers, _Data, _Config = { _I, _C, _E, Ri, _Xi }}) ->
    {NewList, NewUsedServers} = dgr:getPopularity(s_utils:sum(List, Ri), oset:to_list(Servers), oset:add_element(node(), UsedServers)),
    Pid ! {d_popul, NewList, NewUsedServers},
    loop(State);

handleMessage({d_all_m, Pid, UsedServers}, State = {Servers, _Data, _Config = { I, _C, _E, Ri, _Xi }}) ->
    EmptyOset = oset:new(),
    SelfUsed = oset:add_element(node(), EmptyOset),
    ServersList = oset:to_list(Servers),
    {Popularity, _UsedServers} = dgr:getPopularity(Ri, ServersList, SelfUsed),
    Ig = s_utils:initIg(Ri, Popularity),
    {IgMax, J} = s_utils:findIgMax(Ig),
    SendMsg = {IgMax, I, J, 0},
    {MessagesList, UpdatedUsedServers} = dgr:getAllMessages(oset:to_list(Servers), UsedServers),
    Pid ! {d_all_m, [SendMsg] ++ MessagesList, UpdatedUsedServers},
    loop(State);

% Link servers with each other
handleMessage({link, ServerName}, _State = {ServersList, DataList, Config}) ->
    NewServersList = oset:add_element(ServerName, ServersList),
    loop({NewServersList, DataList, Config}).