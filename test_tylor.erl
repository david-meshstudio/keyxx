-module(test_tylor).
-compile(export_all).

jc(N) ->
	if
		N > 0 ->
			N * jc(N - 1);
		true ->
			1
	end.

exp_tylor(X) ->
	exp_tylor(X, 0).

exp_tylor(X,N) ->
	if
		N < 150 ->
			math:pow(X, N) / jc(N) + exp_tylor(X, N + 1);
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
