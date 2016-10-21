-module(test_tylor).
-compile(export_all).

jc(N) ->
	if
		N > 0 ->
			N * jc(N - 1);
		true ->
			1
	end.

exp_remain_p0(X) ->
	0.0000006 * math:pow(X, 4) - 0.0008 * math:pow(X, 3) + 0.3658 * math:pow(X, 2) - 13.99 * X + 105.15.

exp_remain_p1(X) ->
	0.00000006 * math:pow(X, 5) - 0.00005 * math:pow(X, 4) + 0.0059 * math:pow(X, 3) - 0.1829 * math:pow(X, 2) + 3.0153 * X - 20.049.

exp_remain_p2(X) ->
	0.0000004 * math:pow(X, 4) - 0.0006 * math:pow(X, 3) + 0.2594 * math:pow(X, 2) + 4.6374 * X - 969.06.

test_exp_tylor(X) ->
	N = 20,
	R = math:exp(X) - exp_tylor(X),
	P = math:exp(math:log(R / math:pow(X, N + 1) * jc(N + 1)) / X * (N + 1)),
	[R, P].
	% R.

exp_tylor(X) ->
	exp_tylor(X, 0).

exp_tylor(X,N) ->
	if
		N < 20 ->
			math:pow(X, N) / jc(N) + exp_tylor(X, N + 1);
		X < 9 ->
			0;
		% X >= 10, X < 100 ->
		% 	math:pow(X, N + 1) / jc(N + 1) * math:exp(X / (N + 1) * math:log(exp_remain_p1(X)));
		% X > 100 ->
		% 	math:pow(X, N + 1) / jc(N + 1) * math:exp(X / (N + 1) * math:log(exp_remain_p2(X)))
		true ->
			0
	end.

ln_tylor(X) ->
	ln_tylor(X - 1, 1).

ln_tylor(X, N) ->
	if
		N < 100 ->
			math:pow(-1, N + 1) * math:pow(X, N) / N + ln_tylor(X, N + 1);
		true ->
			0
	end.

ds_tylor(X) ->
	ds_tylor(X - 1, 0).

ds_tylor(X, N) ->
	if
		N < 500 ->
			math:pow(-1, N) * math:pow(X, N) + ds_tylor(X, N + 1);
		true ->
			0
	end.

c(A, N) ->
	if
		N =:= 0 ->
			1;
		N =:= 1 ->
			A;
		true ->
			(A - N + 1) * c(A, N - 1)
	end.

mi_tylor(X, A) ->
	mi_tylor(X - 1, A, 0).

mi_tylor(X, A, N) ->
	if
		N < 100 ->
			c(A, N) / jc(N) * math:pow(X, N) + mi_tylor(X, A, N + 1);
		true ->
			0
	end.
