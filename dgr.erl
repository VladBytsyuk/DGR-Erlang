-module(dgr).
-compile(export_all).

-define(IF(Condition, True, False), (case (Condition) of true -> (True); false -> (False) end)).
-define(MAX(A, B), (case (A > B) of true -> A; false -> B end)).
-define(TS, 2).
-define(TR, 2).
-define(TL, 2).


dgr(_State = {Servers, Data, _Config = {I, C, _E, Ri, _Xi}}) ->
    io:format("DGR: start~n~n"),
    EmptyOset = oset:new(),
    SelfUsed = oset:add_element(node(), EmptyOset),
    ServersList = oset:to_list(Servers),
    {Popularity, _UsedServers} = getPopularity(Ri, ServersList, SelfUsed),
    MaxNumber = s_utils:findMaxNumber(0, ServersList, EmptyOset, Data),
    NewXi = s_utils:fillList(MaxNumber, 0),
    NewE = C,
    Ig = s_utils:initIg(Ri, Popularity),
    Ec = s_utils:fillList(MaxNumber, 0),
    Rc = s_utils:fillList(MaxNumber, 0),
    {IgMax, J} = s_utils:findIgMax(Ig),
    SendMsg = {IgMax, I, J, 0},
    io:format("DGR: P = ~p~n", [Popularity]),
    io:format("DGR: Xi = ~p~n", [NewXi]),
    io:format("DGR: E = ~p~n", [NewE]),
    io:format("DGR: Ig = ~p~n", [Ig]),
    io:format("DGR: Ec = ~p~n", [Ec]),
    io:format("DGR: Rc = ~p~n~n", [Rc]),
    RecvMsg = allReduceMax(SendMsg, Servers, Popularity),
    io:format("DGR: allReduceMax(~p, ~p) = ~p~n~n", [SendMsg, oset:to_list(Servers), RecvMsg]),
    NewConfig = whileLoop(RecvMsg, {I, C, NewE, Ri, NewXi}, {Popularity, MaxNumber, Ig, Ec, Rc}, Servers),
    {Servers, Data, NewConfig}.

getPopularity(List, _ServersList = [], UsedServers) -> {List, UsedServers};
getPopularity(List, _ServersList = [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> getPopularity(List, T, UsedServers);
        false -> 
            io:format("Popularity: Send message: ~p ! ~p~n", [H, {d_popul, {serverPid, node()}, List, UsedServers}]),
            {serverPid, H} ! {d_popul, {serverPid, node()}, List, UsedServers},
            receive
                {d_popul, NewList, NewUsedServers} -> 
                    io:format("Popularity: Received message: ~p~n", [{d_popul, NewList, NewUsedServers}]),
                    getPopularity(NewList, T, NewUsedServers)
            end
    end.


allReduceMax(SendMsg, Servers, Popularity) -> 
    UsedServers = oset:add_element(node(), oset:new()),
    allReduceMax(SendMsg, Servers, UsedServers, Popularity).
    
allReduceMax(SendMsg, Servers, UsedServers, Popularity) ->
    {MessagesList, _UsedServers} = getAllMesagges(oset:to_list(Servers), UsedServers, Popularity),
    io:format("DGR: allReduceMax:MessagesList = ~p~n", [MessagesList]),
    findMaxMessage(MessagesList, SendMsg).

getAllMesagges(_ServersList = [], UsedServers, _Popularity) -> {[], UsedServers};
getAllMesagges(_ServersList = [H | T], UsedServers, Popularity) -> 
    case oset:is_element(H, UsedServers) of
        true  -> getAllMesagges(T, UsedServers, Popularity);
        false -> 
            NewUsedServers = oset:add_element(H, UsedServers),
            io:format("DGR: Send message: ~p ! ~p~n", [H, {d_all_m, {serverPid, node()}, oset:to_list(NewUsedServers), Popularity}]),
            {serverPid, H} ! {d_all_m, {serverPid, node()}, NewUsedServers, Popularity},
            receive
                {d_all_m, MessagesList, UpdatedUsedServers} -> 
                    io:format("DGR: Received message: ~p~n", [{d_all_m, MessagesList, oset:to_list(UpdatedUsedServers)}]),
                    {Messages, NewUpdatedUsedServers} = getAllMesagges(T, UpdatedUsedServers, Popularity),
                    {MessagesList ++ Messages, oset:union(UpdatedUsedServers, NewUpdatedUsedServers)}
            end      
    end.

findMaxMessage(_MessageList = [], Msg) -> Msg;
findMaxMessage(_MessageList = [H = {HIgMax, _HI, _HJ, _HJ_} | T], _Msg = {IgMax, _I, _J, _J_}) when HIgMax > IgMax -> findMaxMessage(T, H);
findMaxMessage(_MessageList = [_H = {_HIgMax, _HI, _HJ, _HJ_} | T], Msg) -> findMaxMessage(T, Msg).


whileLoop(RecvMsg = {IgMax, _I_, _J, _J_}, 
            Config = {I, C, E, Ri, _Xi},
            Buf = {P, MaxNumber, Ig, Ec, _Rc},
            Servers) when IgMax > 0 ->
    io:format("While :~n"),
    io:format(" Config : ~p~n", [Config]),
    io:format(" Buf : ~p~n", [Buf]),
    io:format(" Servers : ~p~n~n", [oset:to_list(Servers)]),
    {ResXi, ResIg, ResEc, ResE, ResRc} = igIf(RecvMsg, Config, Buf),
    {NewIgMax, NewJ} = s_utils:findIgMax(Ig),
    {NewEcMin, NewJ_} = s_utils:findEcMin(Ec),
    {ResIgMax, ResJ_} = terminateIf(E, C, MaxNumber + 1, NewIgMax, NewJ_, NewEcMin),
    SendMsg = {ResIgMax, I, NewJ, ResJ_},
    NewRecvMsg = allReduceMax(SendMsg, Servers, P),
    whileLoop(NewRecvMsg, {I, C, ResE, Ri, ResXi}, {P, MaxNumber, ResIg, ResEc, ResRc}, Servers);
whileLoop(_RecvMsg, Config, _Buf, _Servers) -> Config.

igIf(_RecvMsg = {_IgMax, I_, J, J_}, 
        _Config = {I, _C, E, _Ri, _Xi},
        _Buf = {_P, _MaxNumber, NewXi, E, Ig, Ec, Rc}) when I_ == I ->
    UpdatedXi = s_utils:replaceListItem(J, 1, NewXi), 
    NewEc = s_utils:replaceListItem(J, s_utils:getListItem(J, Ig), Ec),
    NewIg = s_utils:replaceListItem(J, 0, Ig),
    NewE = E - 1,
    NewRc = s_utils:replaceListItem(J, s_utils:getListItem(J, Rc) + 1, Rc),
    evictedIfIgSelf(UpdatedXi, NewIg, NewEc, NewE, NewRc, J_);

igIf(_RecvMsg = {_IgMax, _I_, J, J_}, 
        _Config = {_I, _C, E, Ri, _Xi},
        _Buf = {P, _MaxNumber, NewXi, E, Ig, Ec, Rc}) ->
    NewRc = s_utils:replaceListItem(J, s_utils:getListItem(J, Rc) + 1, Rc),
    {NewIg, NewEc} = s_utils:replicaIf(s_utils:getListItem(J, NewXi), s_utils:getListItem(J, Ri), J, Ig, Ec),
    {ResultRc, ResultEc} = evictedIfIgOther(J_, NewXi, Ri, P, NewRc, NewEc),
    {NewXi, NewIg, ResultEc, E, ResultRc}.

evictedIfIgSelf(Xi, Ig, Ec, E, Rc, J) when J =/= 0 ->
    NewXi = s_utils:replaceListItem(J, 0, Xi),
    NewIg = s_utils:replaceListItem(J, s_utils:getListItem(J, Ec), Ig),
    NewEc = s_utils:replaceListItem(J, 0, Ec),
    NewE = E + 1,
    NewRc = s_utils:replaceListItem(J, s_utils:getListItem(J, Rc) - 1, Rc),
    {NewXi, NewIg, NewEc, NewE, NewRc};
evictedIfIgSelf(Xi, Ig, Ec, E, Rc, _J) -> {Xi, Ig, Ec, E, Rc}.

replicaIf(Xij, Rij, J, Ig, Ec) when Xij == 0 -> 
    NewIg = s_utils:replaceListItem(J, Rij * (?TR - ?TL), Ig),
    {NewIg, Ec};
replicaIf(_Xij, Rij, J, Ig, Ec) ->
    NewEc = s_utils:replaceListItem(J, Rij * (?TR - ?TL), Ec),
    {Ig, NewEc}.

evictedIfIgOther(J, Xi, Ri, P, Rc, Ec) when J =/= 0 ->
    NewRc = s_utils:replaceListItem(J, s_utils:getListItem(J, Rc) - 1, Rc),
    case s_utils:getListItem(J, Xi) == 1 andalso s_utils:getListItem(J, NewRc) == 1 of
        true  -> 
            Rij = s_utils:getListItem(J, Ri),
            Pj = s_utils:getListItem(J, P),
            Value = Rij * (?TR -?TL) + Pj * (?TS - ?TR),
            NewEc = s_utils:replaceListItem(J, Value, Ec),
            {NewRc, NewEc};
        false -> {NewRc, Ec}
    end;
evictedIfIgOther(_J, _Xi, _Ri, _P, Rc, Ec) -> {Rc, Ec}.

terminateIf(E, C, N, IgMax, J, EcMin) ->
    case E == 0 orelse C - E >= N of
        true  -> 
            case IgMax =< EcMin of
                true  -> {0, 0};
                false -> {IgMax, J}
            end;
        false -> {IgMax, 0}
    end.