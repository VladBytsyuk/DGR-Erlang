-module(client).
-export([cstart/1, stop/0, set/2, get/1, dgr/0]).

cstart(ServerName) ->
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

dgr() -> 
    clientPid ! dgr,
    {dgr, run}.



loop(ServerName) -> 
    receive
        self_stop ->
            handleMessage(self_stop, ServerName);
        Message -> 
            handleMessage(Message, ServerName), 
            loop(ServerName)
    end.



handleMessage(self_stop, _ServerName) ->
    {ok, client_stopped};

handleMessage({self_set, Key, Value}, ServerName) ->
    {serverPid, ServerName} ! {c_set, self(), Key, Value};

handleMessage({self_get, Key}, ServerName) ->
    {serverPid, ServerName} ! {c_get, self(), Key};

handleMessage(dgr, ServerName) ->
    {serverPid, ServerName} ! d_dgr;

handleMessage({c_set, Object}, _ServerName) ->
    io:format("Set: ~p~n", [Object]),
    {ok, set, Object};

handleMessage({c_get, Object}, _ServerName) ->
    io:format("Get: ~p~n", [Object]),
    {ok, get, Object}.
        