-module(keyxx_tool).
-compile(export_all).
-import(rfc4627, [encode/1,decode/1]).
-define(PERIOD, 1000).
-define(STEP, 1).
-define(DIR, "E:\\GFDF\\").

% Base Operation
base_add(P1, P2, C1, C2, UID) ->
	[A1, X1, Y1|L] = C1,
	case C2 of
		[A2, X2, Y2|L] ->
			U1 = getH(X1, Y2, 3),
			U2 = getH(X2, Y1, 3),
			U3 = getH(Y1, Y2, 5),
			U4 = getH(U1, U2, 1),
			U5 = getH(U2, U4, 2),
			U6 = getH(U3, U5, 5),
			A3 = A1 * P1 * getGValue(UID, X1, Y2, 3),
			A4 = A2 * P2 * getGValue(UID, X2, Y1, 3),
			A5 = getGValue(UID, Y1, Y2, 5),
			A6 = A3 * getGValue(UID, U1, U2, 0),
			A7 = A6 * getGValue(UID, U2, U4, 2) + (A4 - A3) * getGValue(UID, U2, U4, 1),
			A8 = A5 * getGValue(UID, U3, U5, 5),
			[A7 / A8, U5, U6];
		0 ->
			[A1, X1, Y1];
		P when is_integer(P); is_float(P) ->
			[A1, X1, Y1]
	end.	

base_multiply(C1, C2, UID) ->
	[A1, X1, Y1|L] = C1,
	case C2 of
		[A2, X2, Y2|L] ->
			U1 = getH(X1, X2, 4),
			U2 = getH(Y1, Y2, 5),
			A3 = A1 * A2 * getGValue(UID, X1, X2, 4) / getGValue(UID, Y1, Y2, 5),
			[A3, U1, U2];
		P when is_integer(P) ->
			base_multiply_cp(C1, P)
	end.	

base_multiply(P1, C1, C2, UID) ->
	[A1, X1, Y1|L] = C1,
	case C2 of
		[A2, X2, Y2|L] ->
			U1 = getH(X1, X2, 4),
			U2 = getH(Y1, Y2, 5),
			A3 = P1 * A1 * A2 * getGValue(UID, X1, X2, 4) / getGValue(UID, Y1, Y2, 5),
			[A3, U1, U2];
		P when is_integer(P) ->
			base_multiply_cp(C1, P * P1)
	end.	

base_multiply_cp([A, X, Y|_], P) ->
	[A * P, X, Y].

% G Operation
getGValue(UID, X, Y, I) ->
	Xp = getValueInPeriod(X),
	Yp = getValueInPeriod(Y),
	X0 = getIndex(Xp),
	Y0 = getIndex(Yp),
	X1 = getValueInPeriod(X0 + ?STEP),
	Y1 = getValueInPeriod(Y0 + ?STEP),
	Z00 = getGFileValue(UID, X0, Y0, I),
	Z01 = getGFileValue(UID, X0, Y1, I),
	Z10 = getGFileValue(UID, X1, Y0, I),
	Z11 = getGFileValue(UID, X1, Y1, I),
	Z0 = (Xp - X0) / ?STEP * (Z10 - Z00) + Z00,
	Z1 = (Xp - X0) / ?STEP * (Z11 - Z01) + Z01,
	(Yp - Y0) / ?STEP * (Z1 - Z0) + Z0.

getGFileValue(UID, Xi, Yi, I) ->
	Filename = ?DIR ++ UID ++ "_" ++ integer_to_list(I) ++ ".gmd",
	{ok, File} = file:open(Filename, [raw, read]),
	{ok, FileContent} = file:pread(File, (Xi * ?PERIOD + Yi) * 4 * 8, 8),
	file:close(File),
	binary_to_term(list_to_binary([131,70|lists:reverse(FileContent)])).

getValueInPeriod(X) ->
	if
		X < ?PERIOD ->
			X;
		X >= ?PERIOD ->
			getValueInPeriod(X - ?PERIOD)
	end.

getIndex(X) ->
	trunc(X / ?STEP).

% H Function
getH(X, Y, I) ->
	if
		X + Y + I >= ?PERIOD ->
			X + Y + I - ?PERIOD;
		true ->
			X + Y + I
	end.

% Tylor

jc(N) ->
	if
		N > 0 ->
			N * jc(N - 1);
		true ->
			1
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