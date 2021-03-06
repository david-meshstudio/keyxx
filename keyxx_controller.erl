-module(keyxx_controller).
-import(rfc4627,[encode/1,decode/1]).
-export([base/3,sdb/3,demo/3]).

base(SessionID, _Env, Input) ->
	Data = decode(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8\r\n\r\n"],
	{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,
	case binary_to_list(Command) of
		"add" ->
			{ok, [P1, P2, C1, C2, FC0, FC1, UID], _} = decode(binary_to_list(Params)),
			{ok, C1V, _} = decode(base64:decode_to_string(binary_to_list(C1))),
			{ok, C2V, _} = decode(base64:decode_to_string(binary_to_list(C2))),
			{ok, FC0V, _} = decode(base64:decode_to_string(binary_to_list(FC0))),
			{ok, FC1V, _} = decode(base64:decode_to_string(binary_to_list(FC1))),
			L = keyxx_byte_operation:cipher_add(P1, P2, keyxx_byte_operation:remove_power(C1V), keyxx_byte_operation:remove_power(C2V), keyxx_operation:standardizeList(FC0V), keyxx_operation:standardizeList(FC1V), binary_to_list(UID)),
			io:format("~p~n", [L]),
			Content = base64:encode_to_string(encode(L));
		"multiply" ->
			{ok, [P, C1, C2, FC0, FC1, UID], _} = decode(binary_to_list(Params)),
			{ok, C1V, _} = decode(base64:decode_to_string(binary_to_list(C1))),
			{ok, C2V, _} = decode(base64:decode_to_string(binary_to_list(C2))),
			{ok, FC0V, _} = decode(base64:decode_to_string(binary_to_list(FC0))),
			{ok, FC1V, _} = decode(base64:decode_to_string(binary_to_list(FC1))),
			L = keyxx_byte_operation:cipher_multiply(P, keyxx_byte_operation:remove_power(C1V), keyxx_byte_operation:remove_power(C2V), keyxx_operation:standardizeList(FC0V), keyxx_operation:standardizeList(FC1V), binary_to_list(UID)),
			% Content = base64:encode_to_string(encode(keyxx_byte_operation:bv_recover_pow_result(L)));
			io:format("~p~n", [L]),
			Content = base64:encode_to_string(encode(L));
		"power" ->
			{ok, [C, N, FC0, FC1, UID], _} = decode(binary_to_list(Params)),
			{ok, CV, _} = decode(base64:decode_to_string(binary_to_list(C))),
			{ok, FC0V, _} = decode(base64:decode_to_string(binary_to_list(FC0))),
			{ok, FC1V, _} = decode(base64:decode_to_string(binary_to_list(FC1))),
			L = keyxx_byte_operation:cipher_power(keyxx_byte_operation:remove_power(CV), N, keyxx_operation:standardizeList(FC0V), keyxx_operation:standardizeList(FC1V), binary_to_list(UID)),
			Content = base64:encode_to_string(encode(keyxx_byte_operation:bv_recover_pow_result(L)))
	end,
	io:format("~p~n", [Content]),
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).

sdb(SessionID, _Env, Input) ->
	Data = decode(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8\r\n\r\n"],
	{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,
	case binary_to_list(Command) of
		"put" ->
			Content = encode(Params);
		"get" ->
			Content = encode(Params);
		"del" ->
			Content = encode(Params);
		"list" ->
			Content = encode(Params);
		"add" ->
			Content = encode(Params);
		"multiply" ->
			Content = encode(Params)
	end,
	io:format("~p~n", [Content]),
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).


demo(SessionID, _Env, Input) ->
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
		"add2" ->
			{ok, [C1, C2, UID1, UID2], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID1]),
			io:format("~p~n", [UID2]),
			L = keyxx_operation:cipher_add2(1, 1, C1, C2, binary_to_list(UID1), binary_to_list(UID2)),
			Content = encode(keyxx_operation:bv_recover_pow_result(L));
		"substract2" ->
			{ok, [C1, C2, UID1, UID2], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID1]),
			io:format("~p~n", [UID2]),
			L = keyxx_operation:cipher_add2(1, -1, C1, C2, binary_to_list(UID1), binary_to_list(UID2)),
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
		"multiply2" ->
			{ok, [C1, C2, UID1, UID2], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [UID1]),
			io:format("~p~n", [UID2]),
			L = keyxx_operation:cipher_multiply2(C1, C2, binary_to_list(UID1), binary_to_list(UID2)),
			Content = encode(keyxx_operation:bv_recover_pow_result(L));
		"multiply16" ->
			{ok, [C1, C2, FC0, FC1, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C1]),
			io:format("~p~n", [C2]),
			io:format("~p~n", [FC0]),
			io:format("~p~n", [FC1]),
			io:format("~p~n", [UID]),
			L = keyxx_byte_operation:cipher_multiply(1, C1, C2, FC0, FC1, binary_to_list(UID)),
			Content = encode(keyxx_byte_operation:bv_recover_pow_result(L));
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
		"binary" ->
			{ok, [C, X0, X1, Y0, Y1, C1, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C]),
			io:format("~p~n", [X0]),
			io:format("~p~n", [X1]),
			io:format("~p~n", [Y0]),
			io:format("~p~n", [Y1]),
			io:format("~p~n", [C1]),
			io:format("~p~n", [UID]),
			Content = encode(keyxx_operation:to_binary(C, [X0, X1, Y0, Y1], C1, binary_to_list(UID)));
		"simplify" ->
			% {ok, [C, KPL, UID], _} = decode(binary_to_list(Params)),
			{ok, [C, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C]),
			% io:format("~p~n", [KPL]),
			io:format("~p~n", [UID]),
			% Content = encode(keyxx_operation:cipher_simplify(keyxx_operation:bv_recover_pow_result(C), KPL, binary_to_list(UID)));
			Content = encode(keyxx_operation:cipher_simplify(C, binary_to_list(UID)));
		"register" ->
			{ok, [A1, X1, Y1, A2, X2, Y2, XL, XH, YL, YH, UID], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [A1]),
			io:format("~p~n", [X1]),
			io:format("~p~n", [Y1]),
			io:format("~p~n", [A2]),
			io:format("~p~n", [X2]),
			io:format("~p~n", [Y2]),
			io:format("~p,~p,~p,~p~n", [XL, XH, YL, YH]),
			io:format("~p~n", [UID]),
			{ok, Ref} = dets:open_file(uidreg),
			dets:insert(Ref, {UID, [A1, X1, Y1, A2, X2, Y2, XL, XH, YL, YH]}),
			Content = encode(dets:close(Ref));
		"transform" ->
			{ok, [C, UID1, UID2], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [C]),
			io:format("~p~n", [UID1]),
			io:format("~p~n", [UID2]),
			L = keyxx_operation:cipher_transform(C, binary_to_list(UID1), binary_to_list(UID2)),
			Content = encode(keyxx_operation:bv_recover_pow_result(L));
		"from_to" ->
			{ok, [L11, Tx, L21, UID1, UID2], _} = decode(binary_to_list(Params)),
			io:format("~p~n", [L11]),
			io:format("~p~n", [Tx]),
			io:format("~p~n", [L21]),
			io:format("~p~n", [UID1]),
			io:format("~p~n", [UID2]),
			[Stat, L12, L22] = myth_api:from_to(L11, Tx, L21, binary_to_list(UID1), binary_to_list(UID2)),
			Content = encode([Stat, L12, L22]);
		Other ->
			Content = {"No such query", Other}
	end,
	io:format("~p~n", [Content]),
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).