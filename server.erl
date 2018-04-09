-module(server).
-export([start/3, stop/0, slink/1]).

% Macro
-define(IF(Condition, True, False), (case (Condition) of true -> (True); false -> (False) end)).
-define(MAX(A, B), (case (A > B) of true -> A; false -> B end)).

% Exported functions
start(Number, Capacity, BarrierPid) -> 
    {barrier, BarrierNode} = BarrierPid,
    net_kernel:connect_node(BarrierNode),
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
                }, 
                BarrierPid
        }) 
    end)).

stop() -> serverPid ! stop.

slink(ServerName) ->
    net_kernel:connect_node(ServerName),
    serverPid ! {link_first, ServerName}.


% Main loop
loop(State) -> 
    printState(State),
    receive
        Message -> handleMessage(Message, State)
    end.

printState(State) ->
    {Servers, Data, _Config = {I, C, E, Ri, Xi}, BarrierPid} = State,
    io:format("~n", []),
    io:format("~p State:~n", [node()]),
    io:format(" Servers: ~p~n", [oset:to_list(Servers)]),
    io:format(" Data: ~p~n", [oset:to_list(Data)]),
    io:format(" Config:~n", []),
    io:format("     Server number: ~p~n", [I]),
    io:format("     Server capacity: ~p~n", [C]),
    io:format("     Server space left: ~p~n", [E]),
    io:format("     Ri: ~p~n", [Ri]),
    io:format("     Xi: ~p~n", [Xi]),
    io:format(" Barrier: ~p~n", [BarrierPid]),
    io:format("~n", []).

%% ================================================================================================
%%
%%  Handle messages
%%
%% ================================================================================================

% Current server was stopped
handleMessage(stop, _State = {Servers, _Data, _Config, BarrierPid}) -> 
    io:format("Stop~n",[]),
    s_utils:notifyStop(oset:to_list(Servers), oset:new(), node(), BarrierPid);

% Remote server was stopped
handleMessage({s_stopped, Pid, StoppedName, UsedServers}, _State = {Servers, Data, Config, BarrierPid}) ->
    NewServers = oset:del_element(StoppedName, Servers),
    {ok, notifyStop, StoppedName, UpdatedUsedServers} = s_utils:notifyStop(oset:to_list(NewServers), UsedServers, StoppedName),
    Pid ! {s_stopped, UpdatedUsedServers},
    loop({NewServers, Data, Config, BarrierPid});

% Set data from client
handleMessage({c_set, Pid, Key, Value}, State = {Servers, _Data, _Config, BarrierPid}) ->
    Object = s_utils:findObject(Key, State),
    {NewObject, NewData, NewConfig, _BarrierPid} = s_utils:tryToAddObject(Object, State, Key, Value),
    Pid ! {c_set, NewObject}, 
    loop({Servers, NewData, NewConfig, BarrierPid});

% Get data to client
handleMessage({c_get, Pid, Key}, State = {Servers, Data, Config, BarrierPid}) ->
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
            loop({Servers, Data, NewConfig, BarrierPid})
    end;

handleMessage({s_get, ServerPid, Key, UsedServers}, State = {Servers, Data, _Config, _BarrierPid}) ->
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
handleMessage({s_obj_add, Pid,  UsedServers}, _State = {Servers, Data, _Config = {I, C, E, Ri, Xi}, BarrierPid}) ->
    NewConfig = { I, C, E, Ri ++ [0], Xi ++ [0]},
    {ok, notifyObjectAdded, NewUsedServers} = s_utils:notifyObjectAdded(oset:to_list(Servers), UsedServers),
    Pid ! {s_obj_add, NewUsedServers},
    loop({Servers, Data, NewConfig, BarrierPid});

% Find maximum of objects naumber
handleMessage({s_max_num, ServerPid, Max, UsedServers}, State = {Servers, Data, _Config, _BarrierPid}) ->
    LocalMax  = s_utils:findMaxNumberLocal(Max, oset:to_list(Data)),
    {RemoteMax, UpdatedUsedServers} = s_utils:findMaxNumberRemote(Max, oset:to_list(Servers), UsedServers),
    RealMax = ?MAX(LocalMax, RemoteMax),
    ServerPid ! {s_max_num, RealMax, UpdatedUsedServers},
    loop(State);

handleMessage({d_popul, Pid, List, UsedServers}, State = {Servers, _Data, _Config = { _I, _C, _E, Ri, _Xi }, _BarrierPid}) ->
    {NewList, NewUsedServers} = dgr:getPopularity(s_utils:sum(List, Ri), oset:to_list(Servers), oset:add_element(node(), UsedServers)),
    Pid ! {d_popul, NewList, NewUsedServers},
    loop(State);


handleMessage({d_data, Pid, UsedServers}, State = {Servers, Data, _Config, _BarrierPid}) ->
    {Objects, NewUsedServers} = s_utils:getAllObjects(oset:to_list(Servers), Data, UsedServers),
    Pid ! {d_data, Objects, NewUsedServers},
    loop(State);

handleMessage(d_dgr, State = {Servers, Data, _Config = {_I, _C, _E, Ri, _Xi}, BarrierPid}) ->
    BarrierPid ! start,
    
    UsedServers = oset:add_element(node(), oset:new()),

    {Popularity, _UsedServers} = dgr:getPopularity(Ri, oset:to_list(Servers), UsedServers),
    io:format(" Popularity: ~p~n", [Popularity]),
   
    {Objects, _NewUsedServers} = s_utils:getAllObjects(oset:to_list(Servers), Data, UsedServers),
    ObjectsList = oset:to_list(Objects),
    io:format(" Objects: ~p~n", [ObjectsList]),

    s_utils:dgrNotify(Popularity, ObjectsList, oset:to_list(Servers), UsedServers),
    loop(dgr:dgr(State, Popularity, ObjectsList, initiator));

handleMessage({d_dgr, Pid, Popularity, ObjectsList, UsedServers}, State = {Servers, _Data, _Config, _BarrierPid}) ->
    UpdatedUsedServers = s_utils:dgrNotify(Popularity, ObjectsList, oset:to_list(Servers), oset:add_element(node(), UsedServers)),
    Pid ! {d_dgr, UpdatedUsedServers},
    loop(dgr:dgr(State, Popularity, ObjectsList, no_initiator));

handleMessage({in_one_conponent, Pid, Node, UsedServers}, State = {Servers, _Data, _Config, _BarrierPid}) ->
    Pid ! s_utils:inOneComponent(Node, oset:to_list(Servers), UsedServers),
    loop(State);

handleMessage({fix_numbers, Pid, MaxNumber, UsedServers}, _State = {Servers, Data, _Config = {I, C, E, Ri, Xi}, BarrierPid}) ->
    NewData = s_utils:fixNumbers(MaxNumber, Data),
    NewConfig = { I, C, E, s_utils:fillList(MaxNumber, 0) ++ Ri, s_utils:fillList(MaxNumber, 0) ++ Xi},
    {fix_numbers, UpdatedUsedServers} = s_utils:fixNumbers(MaxNumber, oset:to_list(Servers), UsedServers),
    {serverPid, Pid} ! {fix_numbers, UpdatedUsedServers},
    loop({Servers, NewData, NewConfig, BarrierPid});

handleMessage({max_number, Pid}, State = {Servers, Data, _Config, _BarrierPid}) ->
    MaxNumber = s_utils:findMaxNumber(0, oset:to_list(Servers), oset:new(), Data),
    {serverPid, Pid} ! MaxNumber,
    loop(State);

% Link servers with each other
handleMessage({link_first, ServerName}, State = {Servers, Data, _Config = {I, C, E, Ri, Xi}, BarrierPid}) ->
    OtherComponentMaxNumber = s_utils:tryToFixNumbers(ServerName, State),
    NewServers = oset:add_element(ServerName, Servers),
    NewConfig = {I, C, E, Ri ++ s_utils:fillList(OtherComponentMaxNumber, 0), Xi ++ s_utils:fillList(OtherComponentMaxNumber, 0)},
    {serverPid, ServerName} ! {link_second, node()},   
    loop({NewServers, Data, NewConfig, BarrierPid});


handleMessage({link_second, ServerName}, _State = {Servers, Data, Config, BarrierPid}) ->
    NewServers = oset:add_element(ServerName, Servers),
    loop({NewServers, Data, Config, BarrierPid}).