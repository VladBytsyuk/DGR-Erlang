-module(http_client).
-compile(export_all).
-export([request/5, register/3]).

-import(string, [concat/2]).

parseResponse({ok, {{_, Status, _}, _, Content}}) -> {Status, Content}.

request(Method, Adress, Port, Command, Body) ->
    _InetsStatus = inets:start(),
    AdressString = concat("http://", Adress),
    PortString = concat(":", concat(Port, "/")),
    Host = concat(AdressString, concat(PortString, Command)),
    HostWithBody = concat(Host, concat("?body=", Body)),
    io:format("~p~n", [HostWithBody]),
    Response = httpc:request(Method, {HostWithBody, [], "text", ""},  [],  []),
    parseResponse(Response).

register(Adress, Port, MyPort) -> request(put, 
        Adress, 
        Port, 
        "register", 
        concat(
            concat(atom_to_list(node()), ":"), MyPort
        )
    ).