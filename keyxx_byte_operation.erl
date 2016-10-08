-module(keyxx_byte_operation).
-compile(export_all).

cipher_add(P1, P2, C1, C2, FC0, FC1, UID) ->
	Len = max(length(C1), length(C2)),
	CS1 = standardize(C1, Len, FC0),
	CS2 = standardize(C2, Len, FC0),
	C3 = cipher_part_add(P1, P2, CS1, CS2, UID),
	simplify(C3, 0, FC1, UID).

cipher_part_add(_, _, [], [], _) ->
	[];
cipher_part_add(P1, P2, [Part1|CSL1], [Part2|CSL2], UID) ->
	[keyxx_operation:cipher_add(P1, P2, Part1, Part2, UID)|cipher_part_add(P1, P2, CSL1, CSL2, UID)].

cipher_multiply(C1, C2, FC0, FC1, UID) ->
	C3 = cipher_part_multiply(C1, C2, 0, FC1, UID, C1, C2, 0),
	CL = cipher_part_multiply_split(C3, 0, [], [], FC0),
	C4 = cipher_part_multiply_merge(CL, FC0, FC1, UID),
	C4.

cipher_part_multiply([], _, _, _, _, _, _, _) ->
	[];
cipher_part_multiply(C1, [], _, FC1, UID, C1b, C2b, Index) ->
	cipher_part_multiply(C1, C2b, Index + 1, FC1, UID, C1b, C2b, Index);
cipher_part_multiply(C1, C2, CP, FC1, UID, C1b, C2b, Index) ->
	if
		is_integer(CP) ->
			[Part1|CSL1] = C1,
			cipher_part_multiply(CSL1, C2, Part1, FC1, UID, C1b, C2b, CP);
		true ->
			[Part2|CSL2] = C2,
			[{keyxx_operation:cipher_multiply(CP, Part2, UID), Index}|cipher_part_multiply(C1, CSL2, CP, FC1, UID, C1b, C2b, Index)]
	end.

cipher_part_multiply_split([], _, CP, Res, _) ->
	lists:reverse([lists:reverse(CP)|Res]);
cipher_part_multiply_split([{Part, Index}|CSL], CI, CP, Res, FC0) ->
	if
		Index =:= CI ->
			cipher_part_multiply_split(CSL, Index, [Part|CP], Res, FC0);
		true ->
			cipher_part_multiply_split(CSL, Index, [], [insert_zero_head(lists:reverse(CP), FC0, Index)|Res], FC0)
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
	cipher_add(1, 1, C1, cipher_part_multiply_merge(L, FC0, FC1, UID), FC0, FC1, UID).

bv_recover_pow_result([]) ->
	[];
bv_recover_pow_result([Part|CSL]) ->
	[keyxx_operation:bv_recover_pow_result(Part)|bv_recover_pow_result(CSL)].

standardize(C, Len, FC0) ->
	case length(C) of
		Len ->
			C;
		true ->
			standardize([C|[FC0]], Len, FC0)
	end.

simplify([], _, _, _) ->
	[];
simplify([P|CSL], Q, FC1, UID) ->
	Range = [92, 93, 89, 90],
	[Q, M] = keyxx_operation:exact_divid(keyxx_operation:cipher_add(1, 1, P, keyxx_operation:cipher_multiply_constant(Q, FC1), UID), 256, Range, FC1, UID),
	[M|simplify(CSL, Q, FC1, UID)].
