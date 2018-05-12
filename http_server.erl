-module(http_server).
-export([start/3]).

-define(SEP, " ").
-define(CRLF, "\r\n").

handle_request("PUT", "/barrier", _Body) ->
    dgr:start_barrier(),
    {ok, barrier};

handle_request("PUT", "/start", Body) ->
    [NumberString, CapacityString, BarrierNameString] = string:tokens(Body, ?SEP),
    Number = list_to_atom(NumberString),
    {Capacity, _} = string:to_integer(CapacityString),
    BarrierName = list_to_atom(BarrierNameString),
    server:start(Number, Capacity, {barrier, BarrierName}),
    {ok, create};

handle_request("PUT", "/link", Body) ->
    _Response = server:slink(list_to_atom(Body)),
    {ok, link};

handle_request("PUT", "/unlink", Body) ->
    _Response = server:sunlink(list_to_atom(Body)),
    {ok, unlink};

handle_request("PUT", "/stop", _Body) ->
    server:stop(),
    {ok, stop};

handle_request(_Method, _Command, _Body) -> {error, unexpected_request}.


start(Adress, Port, MyPort) ->
    io:format("server started.~n"),
    {ok, ServerSocket} = gen_tcp:listen(MyPort, [binary, {packet, 0},
        {reuseaddr, true}, {active, true}]),
    http_client:register(Adress, integer_to_list(Port), integer_to_list(MyPort)),
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