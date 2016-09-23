-module(keyxx_derivative).
-compile(export_all).

% To be rewirtten

get_x_partial_derivative([]) ->
	[];
get_x_partial_derivative([{H, {Xp, Yp}}|L]) ->
	if
		Xp > 0 ->
			[{H * Xp, {Xp - 1, Yp}}|get_x_partial_derivative(L)];
		true ->
			[{0, {Xp, Yp}}|get_x_partial_derivative(L)]
	end.
get_y_partial_derivative([]) ->
	[];
get_y_partial_derivative([{H, {Xp, Yp}}|L]) ->
	if
		Yp > 0 ->
			[{H * Yp, {Xp, Yp - 1}}|get_y_partial_derivative(L)];
		true ->
			[{0, {Xp, Yp}}|get_y_partial_derivative(L)]
	end.
	
get_c_partial_derivative([[A, X, Y], _]) ->
	[[[A, get_x_partial_derivative(X), Y], 0], [[A, X, get_y_partial_derivative(Y)], 0]].
get_cf_partial_derivative([[[A1, X1, Y1], B1], [[A2, X2, Y2], B2]]) ->
	[get_c_partial_derivative([[A1, X1, Y1], B1])|get_c_partial_derivative([[A2, X2, Y2], B2])].