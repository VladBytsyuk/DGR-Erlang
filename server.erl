-module(server).
-export([start/3, stop/0, slink/1, sunlink/1]).

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

stop() -> 
    serverPid ! {stop, self()},
    s_utils:forceMessage().

slink(ServerName) ->
    net_kernel:connect_node(ServerName),
    serverPid ! {link, self(), ServerName},
    s_utils:forceMessage().

sunlink(ServerName) ->
    serverPid ! {unlink, self(), ServerName},
    s_utils:forceMessage().

% Main loop
loop(State) -> 
    printState(State),
    receive
        Message -> handleMessage(Message, State)
    end.

printState(State) ->
    {Servers, Data, _Config = {I, _C, _E, Ri, Xi}, BarrierPid} = State,
    io:format("~n", []),
    io:format("~p state:~n", [node()]),
    io:format(" Servers: ~p~n", [oset:to_list(Servers)]),
    io:format(" Data: ~p~n", [oset:to_list(Data)]),
    io:format(" Config:~n", []),
    io:format("     Server id: ~p~n", [I]),
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
handleMessage({stop, Pid}, State = {Servers, Data, _Config, _BarrierPid}) -> 
    case s_utils:isBridgeNode(node(), oset:to_list(Servers)) of
        true  -> 
            Pid ! {error, stopping_this_node_breaks_the_system_into_two_components},
            loop(State);
        {false, _UsedServers} -> 
            io:format("Stop~n"),
            [H | _T] = oset:to_list(Servers),
            {serverPid, H} ! {add_all, Data},
            s_utils:notifyStop(oset:to_list(Servers), oset:new(), node()),    
            Pid ! {ok, stopped}
    end;

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

handleMessage({in_one_component, Pid, Node, UsedServers}, State = {Servers, _Data, _Config, _BarrierPid}) ->
    io:format("Is servers ~p and ~p in one component?~n", [node(), Node]),
    InOneComponent = s_utils:inOneComponent(Node, oset:to_list(Servers), UsedServers),
    io:format("~p~n", [InOneComponent]),
    {serverPid, Pid} ! InOneComponent,
    loop(State);

handleMessage({fix_numbers, Pid, MaxNumber, UsedServers}, _State = {Servers, Data, _Config = {I, C, E, Ri, Xi}, BarrierPid}) ->
    io:format("Trying to increase object numbers on ~p [Already fixed on ~p]~n", [MaxNumber, oset:to_list(UsedServers)]),
    NewData = s_utils:fixNumbers(MaxNumber, Data),
    io:format("NewData = ~p~n", [oset:to_list(NewData)]),

    NewConfig = { I, C, E, s_utils:fillList(MaxNumber, 0) ++ Ri, s_utils:fillList(MaxNumber, 0) ++ Xi}, 
    io:format("Trying to fix on numbers on neighbours...~n"),
    {fix_numbers, UpdatedUsedServers} = s_utils:fixNumbers(MaxNumber, oset:to_list(Servers), UsedServers),
    io:format("Fixed! Response to ~p~n", [Pid]),
    {serverPid, Pid} ! {fix_numbers, UpdatedUsedServers},
    loop({Servers, NewData, NewConfig, BarrierPid});

handleMessage({max_number, Pid}, State = {Servers, Data, _Config, _BarrierPid}) ->
    MaxNumber = s_utils:findMaxNumber(0, oset:to_list(Servers), oset:new(), Data),
    {serverPid, Pid} ! MaxNumber,
    loop(State);

handleMessage({increase_ri_xi, Pid, Number, UsedServers}, _State = {Servers, Data, _Config = {I, C, E, Ri, Xi}, BarrierPid}) ->
    NewConfig = {I, C, E, Ri ++ s_utils:fillList(Number, 0), Xi ++ s_utils:fillList(Number, 0)},
    NewUsedServers = s_utils:increaseRiXi(Number, oset:to_list(Servers), UsedServers),
    {serverPid, Pid} ! {ok, NewUsedServers},
    loop({Servers, Data, NewConfig, BarrierPid});

handleMessage({is_bridge, Pid, ServerB, UsedServers}, State = {Servers, _Data, _Config, _BarrierPid}) ->
        io:format("Is ~p-~p brdge? ", [node(), ServerB]),
        IsBridge = s_utils:linkIsNotBridge(ServerB, oset:to_list(Servers), UsedServers),
        io:format("Send reponse to ~p~n", [Pid]),
        {serverPid, Pid} ! IsBridge,
        loop(State);
handleMessage({add_all, NewData}, _State = {Servers, Data, Config, BarrierPid}) ->
    UpdatedData = oset:union(Data, NewData),
    loop({Servers, UpdatedData, Config, BarrierPid});

% Link servers with each other
handleMessage({link, Pid, ServerName}, State = {Servers, Data, _Config = {I, C, E, Ri, Xi}, BarrierPid}) ->
    OtherComponentMaxNumber = s_utils:tryToFixNumbers(ServerName, State),
    NewServers = oset:add_element(ServerName, Servers),
    NewConfig = {I, C, E, Ri ++ s_utils:fillList(OtherComponentMaxNumber, 0), Xi ++ s_utils:fillList(OtherComponentMaxNumber, 0)},
    s_utils:increaseRiXi(OtherComponentMaxNumber, oset:to_list(Servers), oset:from_list([node()])),
    {serverPid, ServerName} ! {link, node()},   
    Pid ! {ok, linked},
    loop({NewServers, Data, NewConfig, BarrierPid});


handleMessage({link, ServerName}, _State = {Servers, Data, Config, BarrierPid}) ->
    NewServers = oset:add_element(ServerName, Servers),
    loop({NewServers, Data, Config, BarrierPid});
    
handleMessage({unlink, ServerName}, _State = {Servers, Data, Config, BarrierPid}) ->
    loop({oset:del_element(ServerName, Servers), Data, Config, BarrierPid});

handleMessage({unlink, Pid, ServerName}, State = {Servers, Data, Config, BarrierPid}) ->
    case oset:is_element(ServerName, Servers) of
        false -> 
            Pid ! {error, servers_are_not_linked},
            loop(State);
        true  ->
            ServerList = oset:to_list(oset:del_element(ServerName, Servers)),
            UsedServers = oset:from_list([node()]),
            case s_utils:linkIsNotBridge(ServerName, ServerList, UsedServers) of
                {false, _UpdatedUsedServers} -> 
                    Pid ! {error, this_link_is_bridge_between_components},
                    loop(State);
                true ->
                    Pid ! {ok, unlinked},
                    {serverPid, ServerName} ! {unlink, node()},
                    loop({oset:del_element(ServerName, Servers), Data, Config, BarrierPid})
            end
    end.