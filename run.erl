-module(run).

-import(server, [start/3, slink/1]).
-import(client, [cstart/1, set/2]).

-export([server/1, client/1, cfill/2, measure/1, measure/2, getObjectNTimes/2, http/1, reg/0]).

client(Args) -> 
    [ServerName, LeftString, RightString | _T] = Args,
    {Left, _} = string:to_integer(LeftString),
    {Right, _} = string:to_integer(RightString),
    cstart(list_to_atom(ServerName)),
    cfill(Left, Right).

server(Args) -> 
    [NumberString, CapacityString, BarrierNode | NeighborNames] = Args,
    Number = list_to_atom(NumberString),
    {Capacity, _} = string:to_integer(CapacityString),
    start(Number, Capacity, {barrier, list_to_atom(BarrierNode)}),
    case NeighborNames == ["empty"] of
        false -> linkNeighbors(NeighborNames);
        true  -> ok
    end.

cfill(A, B) when A > B -> cfill(B, A);
cfill(B, B) -> 
    client:set(B, B),
    {ok, fill};
cfill(A, B) -> 
    client:set(A, A),
    cfill(A + 1, B).
    
linkNeighbors(_NeighborNames = []) -> ok;
linkNeighbors(_NeighborNames = [H | T]) ->
    slink(list_to_atom(H)),
    linkNeighbors(T).

getObjectNTimes(1, Key) -> client:get(Key);
getObjectNTimes(N, Key) ->
    client:get(Key),
    getObjectNTimes(N - 1, Key).

measure(Key) ->
    measure(1000, Key).

measure(N, Key) ->
    {Time, Object} = timer:tc(run, getObjectNTimes, [N, Key]),
    {{time, Time / N / 1000000}, {object, Object}}.
    

http(Port) -> http_server:start(Port).

reg() -> http_server:start("192.168.0.10", 3000, 8001).
