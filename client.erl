-module(client).
-compile(export_all).

start(ServerName) -> 
    register(clientPid, spawn(fun() -> loop(ServerName) end)),
    net_kernel:connect_node(ServerName).

stop() -> 
    clientPid ! self_stop.


set(Key, Value) -> 
    clientPid ! {self_set, Key, Value},
    {ok, send_request_to_server}.

get(Key) -> 
    clientPid ! {self_get, Key},
    {ok, wait_server_response}.



loop(ServerName) -> 
    receive
        self_stop ->
                handleMessage(self_stop, ServerName);
        Message -> 
                handleMessage(Message, ServerName), 
                loop(ServerName)
    end.



handleMessage(self_stop, ServerName) ->
    {ok, client_stopped};

handleMessage({self_set, Key, Value}, ServerName) ->
    {serverPid, ServerName} ! {self(), set, Key, Value};

handleMessage({self_get, Key}, ServerName) ->
    {serverPid, ServerName} ! {self(), get, Key};

handleMessage({set, Key, Value}, _ServerName) ->
    io:format("Set: ~p, ~p~n", [Key, Value]),
    {ok, set, Key, Value};

handleMessage({get, Key, Value}, _ServerName) ->
    io:format("Get: ~p, ~p~n", [Key, Value]),
    {ok, get, Key, Value}.
        