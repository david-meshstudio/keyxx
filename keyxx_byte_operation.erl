-module(keyxx_byte_operation).
-compile(export_all).

% Add
cipher_add(P1, P2, C1, C2, FC0, FC1, UID) ->
	Len = max(length(C1), length(C2)),
	CS1 = standardize(C1, Len, FC0),
	CS2 = standardize(C2, Len, FC0),
	C3 = cipher_part_add(P1, P2, CS1, CS2, UID),
	byte_simplify(C3, 0, FC1, UID).
	% bv_recover_pow_result(C3).

cipher_part_add(_, _, [], [], _) ->
	[];
cipher_part_add(P1, P2, [Part1|CSL1], [Part2|CSL2], UID) ->
	[keyxx_operation:cipher_add(P1, P2, Part1, Part2, UID)|cipher_part_add(P1, P2, CSL1, CSL2, UID)].

% Multiply
cipher_multiply(P, C1, C2, FC0, FC1, UID) ->
	% io:format("~p~n", [P]),
	C3 = cipher_part_multiply(C1, C2, 0, FC1, UID, C1, C2, 0),
	% io:format("~p~n", [C3]),
	CL = cipher_part_multiply_split(C3, 0, [], [], FC0),
	% io:format("~p~n", [CL]),
	C4 = cipher_part_multiply_merge(CL, FC0, FC1, UID),
	% io:format("~p~n", [C4]),
	[C51, C52, C53, C54|_] = cipher_simplify(C4, UID),
	C6 = cipher_multiply_constant(P, [C51, C52, C53, C54]),
	io:format("~p~n", [C6]),
	byte_simplify(C6, 0, FC1, UID).

cipher_multiply_constant(_, []) ->
	[];
cipher_multiply_constant(P, [C|L]) ->
	[keyxx_operation:cipher_multiply_constant(P, C)|cipher_multiply_constant(P, L)].

cipher_part_multiply([], _, _, _, _, _, _, _) ->
	[];
cipher_part_multiply(C1, [], _, FC1, UID, C1b, C2b, Index) ->
	% io:format("~p~n", [Index]),
	[_|CSL1] = C1,
	cipher_part_multiply(CSL1, C2b, Index + 1, FC1, UID, C1b, C2b, Index);
cipher_part_multiply(C1, C2, CP, FC1, UID, C1b, C2b, Index) ->
	if
		is_integer(CP) ->
			[Part1|_] = C1,
			cipher_part_multiply(C1, C2, Part1, FC1, UID, C1b, C2b, CP);
		true ->
			[Part2|CSL2] = C2,
			[{keyxx_operation:cipher_multiply(CP, Part2, UID), Index}|cipher_part_multiply(C1, CSL2, CP, FC1, UID, C1b, C2b, Index)]
	end.

cipher_part_multiply_split([], Index, CP, Res, FC0) ->
	lists:reverse([insert_zero_head(lists:reverse(CP), FC0, Index)|Res]);
cipher_part_multiply_split([{Part, Index}|CSL], CI, CP, Res, FC0) ->
	if
		Index =:= CI ->
			cipher_part_multiply_split(CSL, Index, [Part|CP], Res, FC0);
		true ->
			cipher_part_multiply_split(CSL, Index, [Part], [insert_zero_head(lists:reverse(CP), FC0, Index - 1)|Res], FC0)
	end.

insert_zero_head(L, FC0, Len) ->
	if
		Len > 0 ->
			insert_zero_head([FC0|L], FC0, Len - 1);
		true ->
			L
	end.

cipher_part_multiply_merge([], FC0, _, _) ->
	[FC0];
cipher_part_multiply_merge([C1|L], FC0, FC1, UID) ->
	remove_power(cipher_add(1, 1, C1, cipher_part_multiply_merge(L, FC0, FC1, UID), FC0, FC1, UID)).

% Simplify
cipher_simplify([], _) ->
	[];
cipher_simplify([Part|CSL], UID) ->
	[keyxx_operation:cipher_simplify2(Part, UID)|cipher_simplify(CSL, UID)].

% Power
cipher_power(C, N, FC0, FC1, UID) ->
	if
		N > 1 ->
			cipher_multiply(1, C, cipher_power(C, N - 1, FC0, FC1, UID), FC0, FC1, UID);
		true ->
			C
	end.

% Common
bv_recover_pow_result([]) ->
	[];
bv_recover_pow_result([Part|CSL]) ->
	[keyxx_operation:bv_recover_pow_result(Part)|bv_recover_pow_result(CSL)].

standardize(C, Len, FC0) ->
	case length(C) of
		Len ->
			C;
		_ ->
			standardize(lists:reverse([FC0|lists:reverse(C)]), Len, FC0)
	end.

remove_power([]) ->
	[];
remove_power([P|L]) ->
	[keyxx_operation:standardizeList(P)|remove_power(L)].

byte_simplify([], Q, FC1, UID) ->
	Range = [92, 93, 89, 90],
	[_, M] = keyxx_operation:exact_divid(keyxx_operation:cipher_multiply_constant(Q, FC1), 256, Range, FC1, UID),
	[M];
byte_simplify([P|CSL], Q, FC1, UID) ->
	io:format("~p~n", [Q]),
	Range = [92, 93, 89, 90],
	[Q1, M] = keyxx_operation:exact_divid(keyxx_operation:cipher_add(1, 1, P, keyxx_operation:cipher_multiply_constant(Q, FC1), UID), 256, Range, FC1, UID),
	[M|byte_simplify(CSL, Q1, FC1, UID)].
