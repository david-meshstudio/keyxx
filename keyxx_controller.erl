-module(keyxx_controller).
-import(rfc4627,[encode/1,decode/1]).
-export([do/3]).

do(SessionID, _Env, Input) ->
	Data = decode(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8\r\n\r\n"],
	{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,
	case binary_to_list(Command) of
		"getG" ->
			{ok, [X, Y, I, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [X]),
			io:format("~p~n", [Y]),
			io:format("~p~n", [I]),
			io:format("~p~n", [UID]),
			Content = encode(keyxx_tool:getGValue(binary_to_list(UID), X, Y, I));
		"echo" ->
			{ok, [C, _], _} = decode(binary_to_list(Params)),
			L = keyxx_operation:cipher_multiply_constant(1, C),
			Content = encode(keyxx_operation:bv_recover_pow_result(L));
		"add" ->
			{ok, [P1, P2, C1, C2, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID]),
			L = keyxx_operation:cipher_add(P1, P2, C1, C2, binary_to_list(UID)),
			Content = encode(keyxx_operation:bv_recover_pow_result(L));
		"add16" ->
			{ok, [P1, P2, C1, C2, FC0, FC1, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [FC0]),
			io:format("~p~n", [FC1]),
			io:format("~p~n", [UID]),
			Content = encode(keyxx_byte_operation:cipher_add(P1, P2, C1, C2, FC0, FC1, binary_to_list(UID)));
		"multiply" ->
			{ok, [C1, C2, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID]),
			L = keyxx_operation:cipher_multiply(C1, C2, binary_to_list(UID)),
			Content = encode(keyxx_operation:bv_recover_pow_result(L));
		"multiply16" ->
			{ok, [C1, C2, FC0, FC1, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [FC0]),
			io:format("~p~n", [FC1]),
			io:format("~p~n", [UID]),
			L = keyxx_byte_operation:cipher_multiply(C1, C2, FC0, FC1, binary_to_list(UID)),
			Content = encode(keyxx_operation:bv_recover_pow_result(L));
		"compare" ->
			{ok, [C, X0, X1, Y0, Y1, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C]),
			io:format("~p~n", [X0]),
			io:format("~p~n", [X1]),
			io:format("~p~n", [Y0]),
			io:format("~p~n", [Y1]),
			io:format("~p~n", [UID]),
			Content = encode(keyxx_compare:is_positive(C, [X0, X1, Y0, Y1], binary_to_list(UID)));
		"exactdivid" ->
			{ok, [C, P, X0, X1, Y0, Y1, C1, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C]),
			io:format("~p~n", [P]),
			io:format("~p~n", [X0]),
			io:format("~p~n", [X1]),
			io:format("~p~n", [Y0]),
			io:format("~p~n", [Y1]),
			io:format("~p~n", [C1]),
			io:format("~p~n", [UID]),
			Content = encode(keyxx_operation:exact_divid(C, P, [X0, X1, Y0, Y1], C1, binary_to_list(UID)));
		Other ->
			Content = {"No such query", Other}
	end,
	io:format("~p~n", [Content]),
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).