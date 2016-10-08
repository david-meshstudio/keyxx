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
