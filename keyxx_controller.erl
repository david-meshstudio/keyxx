-module(keyxx_controller).
-import(rfc4627,[encode/1,decode/1]).
-export([do/3]).

do(SessionID, _Env, Input) ->
	io:format("~p~n", [Input]),
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
			[[L1, B1], [L2, B2]] = keyxx_operation:cipherF_multiply_constant(1, C),
			Content = encode([[keyxx_operation:bv_recover_pow_result(L1), B1], [keyxx_operation:bv_recover_pow_result(L2), B2]]);
		"add" ->
			{ok, [P1, P2, C1, C2, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID]),
			[[L1, B1], [L2, B2]] = keyxx_operation:cipherF_add(P1, P2, C1, C2, binary_to_list(UID)),
			Content = encode([[keyxx_operation:bv_recover_pow_result(L1), B1], [keyxx_operation:bv_recover_pow_result(L2), B2]]);
		"multiply" ->
			{ok, [C1, C2, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID]),
			[[L1, B1], [L2, B2]] = keyxx_operation:cipherF_multiply(C1, C2, binary_to_list(UID)),
			Content = encode([[keyxx_operation:bv_recover_pow_result(L1), B1], [keyxx_operation:bv_recover_pow_result(L2), B2]]);
		"divid" ->
			{ok, [C1, C2, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID]),
			[[L1, B1], [L2, B2]] = keyxx_operation:cipherF_divid(C1, C2, binary_to_list(UID)),
			Content = encode([[keyxx_operation:bv_recover_pow_result(L1), B1], [keyxx_operation:bv_recover_pow_result(L2), B2]]);
		Other ->
			Content = {"No such query", Other}
	end,
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).