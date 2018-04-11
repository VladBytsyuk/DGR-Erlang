-module(client).
-export([cstart/1, stop/0, set/2, get/1, dgr/0]).

cstart(ServerName) ->
    register(clientPid, spawn(fun() -> loop(ServerName) end)),
    net_kernel:connect_node(ServerName).

stop() -> 
    clientPid ! self_stop.


set(Key, Value) -> 
    clientPid ! {self_set, self(), Key, Value},
    receive
        Message -> Message    
    end.

get(Key) -> 
    clientPid ! {self_get, self(), Key},
    receive
        {get, Object} -> Object;
        Other -> Other
    end.

dgr() -> 
    clientPid ! dgr,
    {dgr, runned}.



loop(ServerName) -> 
    receive
        self_stop ->
            handleMessage(self_stop, ServerName);
        Message -> 
            handleMessage(Message, ServerName), 
            loop(ServerName)
    end.



handleMessage(self_stop, _ServerName) -> {ok, client_stopped};

handleMessage({self_set, Pid, Key, Value}, ServerName) ->
    {serverPid, ServerName} ! {c_set, self(), Key, Value},
    receive
        {c_set, {Key, Value, _Number}} -> Pid ! {ok, set};
        _Other -> Pid ! {error, server}
    end;

handleMessage({self_get, Pid, Key}, ServerName) ->
    {serverPid, ServerName} ! {c_get, self(), Key},
    receive
        {c_get, {Key, Value, _Number}} -> Pid ! {get, {Key, Value}};
        _Other -> Pid ! {error, server}
    end;

handleMessage(dgr, ServerName) -> {serverPid, ServerName} ! d_dgr.