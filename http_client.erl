-module(http_client).
-compile(export_all).
-export([request/5, register/2]).

-import(string, [concat/2]).

parseResponse({ok, {{_, Status, _}, _, Content}}) -> {Status, Content}.

request(Method, Adress, Port, Command, Body) ->
    _InetsStatus = inets:start(),
    AdressString = concat("http://", Adress),
    PortString = concat(":", concat(Port, "/")),
    Host = concat(AdressString, concat(PortString, Command)),
    Response = httpc:request(Method, {Host, [], "text", Body},  [],  []),
    parseResponse(Response).

register(Adress, Port) -> request("PUT", Adress, Port, "register", node()).