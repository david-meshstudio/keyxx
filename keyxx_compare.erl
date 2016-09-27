-module(keyxx_compare).
-compile(export_all).

substitute_Z([], _) ->
	0;
substitute_Z([{[H|_], [Xp, Yp]}|L], [X, Y]) ->
	H * math:pow(X, Xp) * math:pow(Y, Yp) + substitute_Z(L, [X, Y]).

is_positive(C, [X0, X1, Y0, Y1]) ->
	MP = keyxx_operation:get_maxpower(C),
	CP = keyxx_operation:change(C, 2, MP),
	if
		MP > 1 ->
			unknown;
		true ->
			V00 = substitute_Z(CP, [X0, Y0]),
			V01 = substitute_Z(CP, [X0, Y1]),
			V10 = substitute_Z(CP, [X1, Y0]),
			V11 = substitute_Z(CP, [X1, Y1]),
			if
				V00 > 0 , V01 > 0 , V10 > 0 , V11 > 0 ->
					true;
				V00 < 0 , V01 < 0 , V10 < 0 , V11 < 0 ->
					false;
				true ->
					unknown
			end			
	end.