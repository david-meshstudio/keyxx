-module(skt).
-export([start_nano_server/0,nano_client/1,udp_server/1,udp_client/1]).

start_nano_server() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	gen_tcp:close(Listen),
	loop(Socket).

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n", [Bin]),
			Str = binary_to_term(Bin),
			io:format("Server (unpacked) ~p~n", [Str]),
			Reply = string2value(Str),
			io:format("Server replying = ~p~n", [Reply]),
			gen_tcp:send(Socket, term_to_binary(Reply)),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Server socket closed~n")
	end.

nano_client(Str) ->
	{ok, Socket} = gen_tcp:connect("localhost", 2345, [binary,{packet,4}]),
	ok = gen_tcp:send(Socket, term_to_binary(Str)),
	receive
		{tcp, Socket, Bin} ->
			io:format("Client received binary = ~p~n",[Bin]),
			Val = binary_to_term(Bin),
			io:format("Client result = ~p~n",[Val]),
			gen_tcp:close(Socket)
	end.

string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.

udp_server(Port) ->
	{ok, Socket} = gen_udp:open(Port, [binary]),
	udp_loop(Socket).

udp_loop(Socket) ->
	receive
		{udp, Socket, Host, Port, Bin} ->
			io:format("Server received binary = ~p~n", [Bin]),
			Str = binary_to_term(Bin),
			io:format("Server (unpacked) ~p~n", [Str]),
			Reply = string2value(Str),
			io:format("Server replying = ~p~n", [Reply]),
			gen_udp:send(Socket, Host, Port, term_to_binary(Reply)),
			udp_loop(Socket)
	end.

udp_client(Request) ->
	{ok, Socket} = gen_udp:open(0, [binary]),
	ok = gen_udp:send(Socket, "localhost", 4000, term_to_binary(Request)),
	Value = receive
				{udp, Socket, _, _, Bin} ->
					{ok, Bin}
			after 2000 ->
				error
			end,
	gen_udp:close(Socket),
	Value.