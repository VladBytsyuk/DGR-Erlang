-module(run).

-import(server, [start/3, slink/1]).
-import(client, [cstart/1, set/2]).

-export([server/1, client/1, cfill/0, http/1]).

client(ServerNameList) -> 
    [ServerName | _T] = ServerNameList,
    cstart(list_to_atom(ServerName)).

server(Args) -> 
    [NumberString, CapacityString, BarrierNode | NeighborNames] = Args,
    %{Number, _} = string:to_integer(NumberString),
    Number = list_to_atom(NumberString),
    {Capacity, _} = string:to_integer(CapacityString),
    start(Number, Capacity, {barrier, list_to_atom(BarrierNode)}),
    case NeighborNames == ["empty"] of
        false -> linkNeighbors(NeighborNames);
        true  -> ok
    end.

cfill() ->
    set(alice, amsterdam),
    set(bob, berlin),
    set(carol, coppenhagen),
    set(derek, dubai).

linkNeighbors(_NeighborNames = []) -> ok;
linkNeighbors(_NeighborNames = [H | T]) ->
    slink(list_to_atom(H)),
    linkNeighbors(T).

http(Port) -> http_server:start(Port).