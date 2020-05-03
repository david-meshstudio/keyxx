-module(keyxx_compare).
-compile(export_all).

substitute_Z([], _, _) ->
	0;
substitute_Z([{[H, FX, FY], [Xp, Yp]}|L], [X, Y], UID) ->
	keyxx_tool:base_add(1, 1, [H * math:pow(X, Xp) * math:pow(Y, Yp), FX, FY], substitute_Z(L, [X, Y], UID), UID).

is_positive(C, [X0, X1, Y0, Y1], UID) ->
	MP = keyxx_operation:get_maxpower(C),
	CP = keyxx_operation:change(C, 2, MP),
	if
		MP > 1 ->
			unknown;
		true ->
			[V00|_] = substitute_Z(CP, [X0, Y0], UID),
			[V01|_] = substitute_Z(CP, [X0, Y1], UID),
			[V10|_] = substitute_Z(CP, [X1, Y0], UID),
			[V11|_] = substitute_Z(CP, [X1, Y1], UID),
			% io:format("~p~n", [V00]),
			% io:format("~p~n", [V01]),
			% io:format("~p~n", [V10]),
			% io:format("~p~n", [V11]),
			if
				V00 > 0 , V01 > 0 , V10 > 0 , V11 > 0 ->
					true;
				V00 < 0 , V01 < 0 , V10 < 0 , V11 < 0 ->
					false;
				true ->
					unknown
			end			
	end.

is_larger(C1, C2, Range, UID) ->
	[X0, X1, Y0, Y1] = Range,
	% io:format("C1:~p~n", [C1]),
	C = keyxx_operation:cipher_add(1, -1, C1, C2, UID),
	% io:format("C:~p~n", [C]),
	keyxx_compare:is_positive(C, [X0, X1, Y0, Y1], UID).

is_same([], []) -> true;
is_same([H1|T1], [H2|T2]) ->
	if
		H1 == H2 ->
			is_same(T1, T2);
		true ->
			false
	end.
