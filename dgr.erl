-module(dgr).
-compile(export_all).

-define(IF(Condition, True, False), (case (Condition) of true -> (True); false -> (False) end)).
-define(MAX(A, B), (case (A > B) of true -> A; false -> B end)).
-define(BARRIER_TIME, 50).
-define(TS, 9).
-define(TR, 5).
-define(TL, 2).


start_barrier() -> 
    register(barrier, spawn(fun() -> loop() end)).

loop() ->
    io:format("========================= End barrier ==============================~n"),
    receive
        start -> 
            io:format("======================== Start barrier =============================~n"),    
            mainLoop([], [])
    end.

mainLoop(Pids, Messages) ->
    receive
        {Pid, Msg} -> 
            io:format("~p: ~p~n", [Pid, Msg]),
            mainLoop(Pids ++ [Pid], Messages ++ [Msg])
        after ?BARRIER_TIME -> 
            sendResponses(Pids, Messages)           
    end.

sendResponses(_Pids, _MessageList = []) -> loop();
sendResponses(Pids, Messages = [H | _T]) ->
    MaxMessaage = findMaxMessage(Messages, H),
    send(Pids, MaxMessaage).

send([], _MaxMessage) -> loop();
send(_Pids = [H | T], MaxMessaage) ->
    H ! {d_barrier, MaxMessaage},
    send(T, MaxMessaage).



dgr(_State = {Servers, _Data, _Config = {I, C, _E, Ri, _Xi}, BarrierPid}, Popularity, Objects, Initiator) ->
    MaxNumber = s_utils:findMaxNumberLocal(0, Objects),
    NewXi = s_utils:fillList(MaxNumber, 0),
    NewE = C,
    Ig = s_utils:initIg(Ri, Popularity),
    Ec = s_utils:fillList(MaxNumber, 0),
    Rc = s_utils:fillList(MaxNumber, 0),
    {IgMax, J} = s_utils:findIgMax(Ig),
    SendMsg = {IgMax, I, J, 0},
    RecvMsg = allReduceMax(BarrierPid, SendMsg),
    {NewData, NewConfig} = whileLoop(RecvMsg, BarrierPid, Objects, oset:new(), {I, C, NewE, Ri, NewXi}, {Popularity, MaxNumber, Ig, Ec, Rc}, Servers, Initiator),
    {Servers, NewData, NewConfig, BarrierPid}.

getPopularity(List, _ServersList = [], UsedServers) -> {List, UsedServers};
getPopularity(List, _ServersList = [H | T], UsedServers) ->
    case oset:is_element(H, UsedServers) of
        true  -> getPopularity(List, T, UsedServers);
        false -> 
            {serverPid, H} ! {d_popul, {serverPid, node()}, List, UsedServers},
            receive
                {d_popul, NewList, NewUsedServers} -> getPopularity(NewList, T, NewUsedServers)
            end
    end.


allReduceMax(BarrierPid, SendMsg) ->
    BarrierPid ! {{serverPid, node()}, SendMsg},
    receive
        {d_barrier, MaxMessaage} -> MaxMessaage
        after 2 * ?BARRIER_TIME -> {0, 0, 0, 0}
    end.
   
findMaxMessage(_MessageList = [], Msg) -> Msg;
findMaxMessage(_MessageList = [H = {HIgMax, _HI, _HJ, _HJ_} | T], _Msg = {IgMax, _I, _J, _J_}) when HIgMax > IgMax -> findMaxMessage(T, H);
findMaxMessage(_MessageList = [_H = {_HIgMax, _HI, _HJ, _HJ_} | T], Msg) -> findMaxMessage(T, Msg).


whileLoop(RecvMsg = {IgMax, _I_, _J, _J_}, 
            BarrierPid, 
            Objects,
            Data,
            Config = {I, C, E, Ri, Xi},
            _Buf = {P, MaxNumber, Ig, Ec, Rc},
            Servers,
            Initiator) when IgMax > 0 ->
    {NewData, ResXi, ResIg, ResEc, ResE, ResRc} = igIf(RecvMsg, Objects, Servers, Data, Config, {P, MaxNumber, Xi, E, Ig, Ec, Rc}),
    {NewIgMax, NewJ} = s_utils:findIgMax(Ig),
    {NewEcMin, NewJ_} = s_utils:findEcMin(Ec),
    {ResIgMax, ResJ_} = terminateIf(E, C, MaxNumber, NewIgMax, NewJ_, NewEcMin),
    SendMsg = {ResIgMax, I, NewJ, ResJ_},
    initiateBarrier(Initiator, BarrierPid),
    NewRecvMsg = allReduceMax(BarrierPid, SendMsg),
    whileLoop(NewRecvMsg, BarrierPid, Objects, NewData, {I, C, ResE, Ri, ResXi}, {P, MaxNumber, ResIg, ResEc, ResRc}, Servers, Initiator);
whileLoop(_RecvMsg, _BarrierPid, _Objects, Data, Config, _Buf, _Servers, _Initiator) -> {Data, Config}.

igIf(_RecvMsg = {_IgMax, I_, J, J_}, 
        Objects,
        _Servers,
        Data,
        _Config = {I, _C, E, _Ri, _Xi},
        _Buf = {_P, _MaxNumber, NewXi, E, Ig, Ec, Rc}) when I_ == I ->
    UpdatedXi = s_utils:replaceListItem(J, 1, NewXi),
    Object = s_utils:findObjectWithNumber(Objects, J),
    NewData = oset:add_element(Object, Data),    
    NewEc = s_utils:replaceListItem(J, s_utils:getListItem(J, Ig), Ec),
    NewIg = s_utils:replaceListItem(J, 0, Ig),
    NewE = E - 1,
    NewRc = s_utils:replaceListItem(J, s_utils:getListItem(J, Rc) + 1, Rc),
    evictedIfIgSelf(NewData, UpdatedXi, NewIg, NewEc, NewE, NewRc, J_);

igIf(_RecvMsg = {_IgMax, _I_, J, J_}, 
        _Objects,
        _Servers,
        Data,
        _Config = {_I, _C, E, Ri, _Xi},
        _Buf = {P, _MaxNumber, NewXi, E, Ig, Ec, Rc}) ->
    NewRc = s_utils:replaceListItem(J, s_utils:getListItem(J, Rc) + 1, Rc),
    {NewIg, NewEc} = replicaIf(s_utils:getListItem(J, NewXi), s_utils:getListItem(J, Ri), J, Ig, Ec),
    {ResultRc, ResultEc} = evictedIfIgOther(J_, NewXi, Ri, P, NewRc, NewEc),
    {Data, NewXi, NewIg, ResultEc, E, ResultRc}.

evictedIfIgSelf(Data, Xi, Ig, Ec, E, Rc, J) when J =/= 0 ->
    NewXi = s_utils:replaceListItem(J, 0, Xi),
    NewData = s_utils:removeObjectByNumber(Data, J),
    NewIg = s_utils:replaceListItem(J, s_utils:getListItem(J, Ec), Ig),
    NewEc = s_utils:replaceListItem(J, 0, Ec),
    NewE = E + 1,
    NewRc = s_utils:replaceListItem(J, s_utils:getListItem(J, Rc) - 1, Rc),
    {?IF(NewData =:= {error, no_object_with_current_number}, Data, NewData), NewXi, NewIg, NewEc, NewE, NewRc};
evictedIfIgSelf(Data, Xi, Ig, Ec, E, Rc, _J) -> {Data, Xi, Ig, Ec, E, Rc}.

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

initiateBarrier(initiator, BarrierPid) -> BarrierPid ! start;
initiateBarrier(_, _BarrierPid) -> ok.