-module(myth_api).
-compile(export_all).

from_to(L11, Tx, L21, UID1, UID2) ->
	% io:format("~p~n", [L11]),
	if
		L11 == 0 ->
			[false, 0, 0];
		true ->
			Cond1 = keyxx_compare:is_same(L11, Tx),
			% io:format("Cond1: ~p~n", [Cond1]),
			if
				Cond1 ->
					L12 = 0,
					if
						L21 == 0 ->
							L22 = keyxx_operation:cipher_transform(Tx, UID1, UID2);
						true ->
							L22 = keyxx_operation:cipher_add2(1, 1, L11, L21, UID1, UID2)
					end,
					% [true, L12, keyxx_operation:bv_recover_pow_result(L22)];
					[true, L12, L22];
				true ->
					[X0, X1, Y0, Y1] = keyxx_tool:getXRange(UID1),
					Cond2 = keyxx_compare:is_larger(L11, Tx, [X0, X1, Y0, Y1], UID1),
					% io:format("Cond2: ~p~n", [Cond2]),
					if
						Cond2 ->
							L12 = keyxx_operation:cipher_add(1, -1, L11, Tx, UID1),
							% io:format("~p~n", [L12]),
							if
								L21 == 0 ->
									L22 = keyxx_operation:cipher_transform(Tx, UID1, UID2);
								true ->
									L22 = keyxx_operation:cipher_add2(1, 1, Tx, L21, UID1, UID2)
							end,
							% io:format("~p~n", [L22]),
							% [true, keyxx_operation:bv_recover_pow_result(L12), keyxx_operation:bv_recover_pow_result(L22)];
							[true, L12, L22];
						true ->
							[false, 0, 0]
					end
			end
	end.