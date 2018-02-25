-module(run).

-import(server, [start/1, slink/1]).
-import(client, [cstart/1]).

-export([server/1, client/1]).

client(ServerNameList) -> 
    [ServerName | _T] = ServerNameList,
    cstart(ServerName).

server(Args) -> 
    [NumberString | NeighborNames] = Args,
    {Number, _Rest} = string:to_integer(NumberString),
    start(Number),
    case NeighborNames == ["empty"] of
        false -> linkNeighbors(NeighborNames);
        true  -> ok
    end.

linkNeighbors(_NeighborNames = []) -> ok;
linkNeighbors(_NeighborNames = [H | T]) ->
    slink(list_to_atom(H)),
    linkNeighbors(T).