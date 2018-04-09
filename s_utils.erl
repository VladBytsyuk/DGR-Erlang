-module(s_utils).
-compile(export_all).


-define(IF(Condition, True, False), (case (Condition) of true -> (True); false -> (False) end)).
-define(MAX(A, B), (case (A > B) of true -> A; false -> B end)).
-define(TS, 18).
-define(TR, 5).
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
    case inOneComponent(ServerName, oset:to_list(Servers), node()) of
        true -> 0;
        {false, _UsedServers} -> 
            MaxNumber = findMaxNumber(0, oset:to_list(Servers), oset:new(), Data),
            OtherComponentMaxNumber = findMaxNumberOnOtherComponent(ServerName),
            {serverPid, ServerName} ! {fix_numbers, node(), MaxNumber, oset:from_list([node()])},
            receive
                {fix_numbers, _UpdatedUsedServers} -> OtherComponentMaxNumber
            end
    end.

inOneComponent(_Node, [], UsedServers) -> {false, UsedServers};
inOneComponent(Node, [H | _T], _UsedServers) when Node =:= H -> true;
inOneComponent(Node, [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> inOneComponent(Node, T, UsedServers);
        false -> 
            NewUsedServers = oset:add_element(H, UsedServers),
            {serverPid, H} ! {in_one_conponent, node(), Node, NewUsedServers},
            receive
                true -> true;
                {false, UpdatedUsedServers} -> inOneComponent(Node, T, UpdatedUsedServers)
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
            NewUsedServers = oset:add_element(H, UsedServers),
            {serverPid, H} ! {fix_numbers, node(), MaxNumber, NewUsedServers},
            receive
                {fix_numbers, UpdatedUsedServers} -> fixNumbers(MaxNumber, T, UpdatedUsedServers)
            end
    end.

findMaxNumberOnOtherComponent(ServerName) ->
    {serverPid, ServerName} ! {max_number, node()},
    receive
        MaxNumber -> MaxNumber
    end.