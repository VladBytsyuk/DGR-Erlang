-module(client).
-compile(export_all).

start(ClientName, ServerPid, ServerName) -> 
    register(ClientName, spawn(fun() -> loop({ServerPid, ServerName}) end)),
    net_kernel:connect_node(ServerName),
    ClientName.

stop(ClientName) -> 
    ClientName ! self_stop.


set(ClientName, Key, Value) -> 
    ClientName ! {self_set, Key, Value}.

get(ClientName, Key) -> 
    ClientName ! {self_get, Key}.



loop(Data) -> 
    receive
        self_stop -> 
                handleMessage(self_stop, Data);
        Message -> 
                handleMessage(Message, Data), 
                loop(Data)
    end.



handleMessage(self_stop, {ServerPid, ServerName}) ->
    {ServerPid, ServerName} ! {self(), stop};

handleMessage({self_set, Key, Value}, {ServerPid, ServerName}) ->
    {ServerPid, ServerName} ! {self(), set, Key, Value};

handleMessage({self_get, Key}, {ServerPid, ServerName}) ->
    {ServerPid, ServerName} ! {self(), get, Key};

handleMessage({set, Key, Value}, _Data) ->
    io:format("Set: ~p, ~p~n", [Key, Value]),
    {ok, set, Key, Value};

handleMessage({get, Key, Value}, _Data) ->
    io:format("Get: ~p, ~p~n", [Key, Value]),
    {ok, get, Key, Value}.
        