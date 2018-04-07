-module(http_server).
-export([start/1]).

-define(SEP, " ").
-define(CRLF, "\r\n").


handle_request("PUT", "/create", Body) ->
    [NumberString, CapacityString, BarrierNameString] = string:tokens(Body, ?SEP),
    {Number, _} = string:to_integer(NumberString),
    {Capacity, _} = string:to_integer(CapacityString),
    BarrierName = list_to_atom(BarrierNameString),
    server:start(Number, Capacity, {barrier, BarrierName}),
    {ok, create};

handle_request("PUT", "/link", Body) ->
    server:slink(list_to_atom(Body)),
    {ok, link};

handle_request("PUT", "/unlink", _Body) ->
    {ok, unlink};

handle_request("PUT", "/stop", _Body) ->
    server:stop(),
    {ok, stop};

handle_request(_Method, _Command, _Body) -> {error, unexpected_request}.


start(Port) ->
    io:format("server started.~n"),
    {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {packet, 0},
        {reuseaddr, true}, {active, true}]),
    server_loop(ServerSocket).

server_loop(ServerSocket) ->
    {ok, Socket} = gen_tcp:accept(ServerSocket),

    Pid = spawn(fun() -> handle_client(Socket) end),
    inet:setopts(Socket, [{packet, 0}, binary,
        {nodelay, true}, {active, true}]),
    gen_tcp:controlling_process(Socket, Pid),

    server_loop(ServerSocket).

handle_client(Socket) ->
    receive
        {tcp, Socket, Request} ->
                io:format("received: ~s~n", [Request]),

                [Header, Body] = parse_response(binary_to_list(Request), []),
                [Method, Command | _Rest] = string:tokens(Header, ?SEP),
                Result = handle_request(Method, Command, Body),

                gen_tcp:send(Socket, header() ++ content(Result)),
                gen_tcp:close(Socket),

                io:format("closed...~n")
    end.


header() ->
    "HTTP/1.1 200 OK" ++ ?CRLF ++
    "Content-Type: text" ++ ?CRLF ++
    "Connection: Close" ++ ?CRLF ++ ?CRLF.


content({_Status, Text}) -> atom_to_list(Text).

parse_response(?CRLF ++ ?CRLF ++ Rest, Acc) -> [lists:reverse(Acc) ++ ?CRLF, Rest];
parse_response([H | T], Acc) -> parse_response(T, [H | Acc]);
parse_response([], _Acc) -> ["", ""].