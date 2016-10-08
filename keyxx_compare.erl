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