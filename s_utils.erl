-module(s_utils).
-compile(export_all).


-define(IF(Condition, True, False), (case (Condition) of true -> (True); false -> (False) end)).
-define(MAX(A, B), (case (A > B) of true -> A; false -> B end)).
-define(TS, 250).
-define(TR, 40).
-define(TL, 2).

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


% Notify all servers that new object was addeda
notifyObjectAdded(_State = {Servers, _Data, _Config, _BarrierPid}) ->
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
findObject(Key, {Servers, Data, _Config, _BarrierPid}) ->
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
replaceListItem(Index, _Value, _List) when Index =< 0 -> {error, index_less_than_one};
replaceListItem(Index, Value, List) -> 
    case Index > listLength(List) of
        true  -> {error, index_greater_than_list_size};
        false -> replaceListItemCorrect(Index, Value, List)
    end.
replaceListItemCorrect(1, Value, _List = []) -> [Value];
replaceListItemCorrect(1, Value, _List = [_H | T]) -> [Value] ++ T;
replaceListItemCorrect(Index, Value, _List = [H | T]) -> [H] ++ replaceListItemCorrect(Index - 1, Value, T).

% Forms new (Updates old) object and put it into Data
tryToAddObject(value_not_found, State = {Servers, Data, _Config = { I, C, _E, Ri, Xi }, BarrierPid}, Key, Value) ->
    Number = findMaxNumber(0, oset:to_list(Servers), oset:new(), Data) + 1,
    Object = {Key, Value, Number},
    NewData = oset:add_element(Object, Data),
    notifyObjectAdded(State),
    NewConfig = { I, C, C - oset:size(NewData), lists:append(Ri, [0]), lists:append(Xi, [1]) },
    {Object, NewData, NewConfig, BarrierPid};
tryToAddObject(Object, _State = {_Servers, Data, Config, BarrierPid}, _Key, Value) ->
    NewData = oset:add_element(Object, Data),
    {K, _V, N} = Object,
    NewObject = {K, Value, N},
    {NewObject, NewData, Config, BarrierPid}.

% Get list item by index
getListItem(Index, _List) when Index =< 0 -> {error, index_less_than_one};
getListItem(_Index, _List = []) -> {error, index_greater_than_list_size};
getListItem(1, _List = [H | _T]) -> H;
getListItem(Index, _List = [_H | T]) -> getListItem(Index - 1, T).

listLength(_List = []) -> 0;
listLength(_List = [_H | T]) -> 1 + listLength(T);
listLength(_Other) -> {error, not_list}.

sum(_ListA = [], _ListB = []) -> [];
sum(_listA = [], _ListB) -> {error, lists_have_different_lengths};
sum(_listA, _ListB = []) -> {error, lists_have_different_lengths};
sum(ListA = [HA | TA], ListB = [HB | TB]) ->
    case listLength(ListA) =/= listLength(ListB) of
        true  -> {error, lists_have_different_lengths};
        false -> [HA + HB] ++ sum(TA, TB)
    end;
sum(_OtherA, _OtherB) -> {error, arguments_contain_not_list}.

fillList(Length, _Value) when Length < 0 -> {error, length_less_than_zero};
fillList(Length, _Value) when Length == 0 -> [];
fillList(Length, Value) ->  [Value] ++ fillList(Length - 1, Value).

initIg(_Ri = [], _TP = []) -> [];
initIg(_Ri = [HRi | TRi], _P = [HP | TP]) ->
    Value = HRi * (?TR - ?TL) + HP * (?TS -?TR),
    [Value] ++ initIg(TRi, TP).

findIgMax(Ig) -> findIgMax(-1, 1, 1, Ig).
findIgMax(Max, J, _Index, _Ig = []) -> {Max, J};
findIgMax(Max, J, Index, _Ig = [H | T]) when Max > H -> findIgMax(Max, J, Index + 1, T);
findIgMax(_Max, _J, Index, _Ig = [H | T]) -> findIgMax(H, Index, Index + 1, T).

findEcMin(Ec) -> findEcMin(9999999, -1, -1, Ec).
findEcMin(Min, J, _Index, _Ec = []) -> {Min, J};
findEcMin(Min, J, Index, _Ec = [H | T]) when Min < H -> findEcMin(Min, J, Index + 1, T);
findEcMin(_Min, _J, Index, _Ec = [H | T]) -> findEcMin(H, Index, Index + 1, T).


findObjectWithNumber(_DataList = [], _Number) -> {error, no_such_object};
findObjectWithNumber(_DataList = [Object = {_K, _V, N} | _T], Number) when N == Number -> Object;
findObjectWithNumber(_DataList = [_H | T], Number) -> findObjectWithNumber(T, Number);
findObjectWithNumber(Data, Number) -> findObjectWithNumber(oset:to_list(Data), Number).

removeObject(_Data, Object) when Object =:= {error, no_such_object} -> {error, no_object_with_current_number};
removeObject(Data, Object) -> oset:del_element(Object, Data).

removeObjectByNumber(Data, Number) -> 
    Object = findObjectWithNumber(Data, Number),
    removeObject(Data, Object).

getAllObjects(_ServersList = [], Data, UsedServers) -> {Data, UsedServers};
getAllObjects(_ServersList = [H | T], Data, UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> getAllObjects(T, Data, UsedServers);
        false -> 
            NewUsedServers = oset:add_element(H, UsedServers),
            {serverPid, H} ! {d_data, {serverPid, node()}, NewUsedServers},
            receive
                {d_data, NewData, UpdatedUsedServers} ->
                    UpdatedData = oset:union(Data, NewData),
                    getAllObjects(T, UpdatedData, UpdatedUsedServers)
            end
    end. 

dgrNotify(_Popularity, _ObjectsList, _ServersList = [], UsedServers) -> UsedServers;
dgrNotify(Popularity, ObjectsList, _ServersList = [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> dgrNotify(Popularity, ObjectsList, T, UsedServers);
        false -> 
            NewUsedServers = oset:add_element(H, UsedServers),
            {serverPid, H} ! {d_dgr, {serverPid, node()}, Popularity, ObjectsList, NewUsedServers},
            receive
                {d_dgr, UpdatedUsedServers} ->
                    dgrNotify(Popularity, ObjectsList, T, UpdatedUsedServers)
            end
    end. 






tryToFixNumbers(ServerName, _State) when ServerName =:= node() -> 0;
tryToFixNumbers(ServerName, _State = {Servers, Data, _Config, _BarrierPid}) ->
    io:format("Is servers ~p and ~p in one component?~n", [node(), ServerName]),
    case inOneComponent(ServerName, oset:to_list(Servers), oset:from_list([node()])) of
        true -> io:format("In one component => 0~n"),0;
        {false, _UsedServers} -> 
            io:format("In different components. Trying to find MaxNumber on my component...~n"),
            MaxNumber = findMaxNumber(0, oset:to_list(Servers), oset:new(), Data),
            io:format("Max Number of my component = ~p. Trying to find MaxNumber of neighbour component...~n", [MaxNumber]),
            OtherComponentMaxNumber = findMaxNumberOnOtherComponent(ServerName),
            io:format("Max Number of neighbour component = ~p. Trying to fix numbers of neighbour component...~n", [OtherComponentMaxNumber]),
            {serverPid, ServerName} ! {fix_numbers, node(), MaxNumber, oset:from_list([node(), ServerName])},
            receive
                {fix_numbers, _UpdatedUsedServers} -> io:format("Number in neighbours component are fixed => ~p~n", [OtherComponentMaxNumber]),OtherComponentMaxNumber
            end
    end.

inOneComponent(_Node, [], UsedServers) -> io:format("Servers are in different components.~n"),{false, UsedServers};
inOneComponent(Node, [H | _T], _UsedServers) when Node =:= H -> true;
inOneComponent(Node, [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> inOneComponent(Node, T, UsedServers);
        false -> 
            NewUsedServers = oset:add_element(H, UsedServers),
            io:format(" Ask on ~p: ", [H]),
            {serverPid, H} ! {in_one_component, node(), Node, NewUsedServers},
            receive
                true -> io:format("True~n"),true;
                {false, UpdatedUsedServers} -> io:format("False~n"),inOneComponent(Node, T, UpdatedUsedServers)
            end
    end.

fixNumbers(_MaxNumber, {nil, black, nil, nil}) -> {nil, black, nil, nil};
fixNumbers(MaxNumber, _Data = {{K, V, N}, Color, Left, Right}) -> 
    {{K, V, N + MaxNumber}, Color, fixNumbers(MaxNumber, Left), fixNumbers(MaxNumber, Right)}.

fixNumbers(_MaxNumber, [], UsedServers) -> {fix_numbers, UsedServers};
fixNumbers(MaxNumber, [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> fixNumbers(MaxNumber, T, UsedServers);
        false ->
            io:format(" Fix numbers on ~p~n", [H]),
            NewUsedServers = oset:add_element(H, UsedServers),
            io:format(" Try to fix~n"),
            {serverPid, H} ! {fix_numbers, node(), MaxNumber, NewUsedServers},
            receive
                {fix_numbers, UpdatedUsedServers} -> io:format("Fixed on ~p~n", [H]),fixNumbers(MaxNumber, T, UpdatedUsedServers)
            end
    end.

findMaxNumberOnOtherComponent(ServerName) ->
    {serverPid, ServerName} ! {max_number, node()},
    receive
        MaxNumber -> MaxNumber
    end.

increaseRiXi(_Number, _ServersList = [], UsedServers) -> UsedServers;
increaseRiXi(Number, _ServersList = [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> increaseRiXi(Number, T, UsedServers);
        false ->
            NewUsedServers = oset:add_element(H, UsedServers),
            {serverPid, H} ! {increase_ri_xi, node(), Number, NewUsedServers},
            receive
                {ok, UpdatedUsedServers} -> increaseRiXi(Number, T, UpdatedUsedServers)
            end
    end.

forceMessage() ->
    receive
        Message -> Message
    end.

% Is ServerB in another component from node() with neighbors ServersList
linkIsNotBridge(_ServerB, _ServerList = [], UsedServers) -> {false, UsedServers};
linkIsNotBridge(ServerB, _ServerList = [H | _T], _UsedServers) when H == ServerB -> true;
linkIsNotBridge(ServerB, _ServerList = [H | T], UsedServers) ->
    %io:format(" UsedServers = ~p~n", [oset:to_list(UsedServers)]),
    %io:format(" ServersList = ~p~n", [_ServerList]),
    case oset:is_element(H, UsedServers) of
        true  -> linkIsNotBridge(ServerB, T, UsedServers);
        false ->
            NewUsedServers = oset:add_element(H, UsedServers),
            %io:format(" Is ~p-~p bridge? ", [node(), ServerB]),
            {serverPid, H} ! {is_bridge, node(), ServerB, NewUsedServers},
            receive
                true -> 
                    %io:format("True~n"),
                    true;
                {false, UpdatedUsedServers} -> 
                    %io:format("False~n"),
                    linkIsNotBridge(ServerB, T, UpdatedUsedServers)
            end
    end.

isBridgeNode(Node, ServersList) -> 
    io:format("Is ~p bridge-node? (Neighbors: ~p)~n", [Node, ServersList]),
    UsedServers = oset:from_list([Node]),
    isBridgeNode(Node, ServersList, ServersList, UsedServers).

isBridgeNode(_Node, [], _ServerList, UsedServers) -> {false, UsedServers}; 
isBridgeNode(Node, [H | T], ServerList, UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> 
            io:format(" Link ~p-~p is already checked~n", [node(), H]),
            isBridgeNode(Node, T, ServerList, UsedServers);
        false ->
            ServerListWithoutH = oset:to_list(oset:del_element(H, oset:from_list(ServerList))),
            NewUsedServers = oset:add_element(H, UsedServers),
            io:format("Check ~p~n", [H]),
            case linkIsNotBridge(H, ServerListWithoutH, oset:from_list([Node])) of
                true -> isBridgeNode(Node, T, ServerList, NewUsedServers);
                {false, _UpdatedUsedServers} -> true 
            end
    end.