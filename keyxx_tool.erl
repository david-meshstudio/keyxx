-module(keyxx_tool).
-compile(export_all).
-import(rfc4627, [encode/1,decode/1]).
-define(PERIOD, 1000).
-define(STEP, 1).
-define(DIR, "D:\\GFDF\\").

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
			A3 = A1 * A2 / getGValue(UID, Y1, Y2, 5) * getGValue(UID, X1, X2, 4),
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

% Transform split F
base_split(C1, C2, UID) ->
	[A1, X1, Y1|L] = C1,
	[A2, X2, Y2|L] = C2,
	U1 = getHR(X2, X1, 4),
	U2 = getHR(Y2, Y1, 5),
	A3 = A1 / A2 * getGValue(UID, U2, Y2, 5) / getGValue(UID, U1, X2, 4),
	[A3, U1, U2].

% Refine Accuracy
% base_get_tormap([A, X, Y|_], UID, I) ->
% 	Tor = getGTFileValue(UID, X, Y, I),
% 	[A * Tor, A, Tor].

% Transform Z mapping
getZMapping(UID) ->
	% Filename = ?DIR ++ UID ++ "_" ++ "Z.gmd",
	% {ok, File} = file:open(Filename, [raw, read]),
	% {ok, A1} = file:pread(File, 0 * 8, 8),
	% {ok, X1} = file:pread(File, 1 * 8, 8),
	% {ok, Y1} = file:pread(File, 2 * 8, 8),
	% {ok, A2} = file:pread(File, 3 * 8, 8),
	% {ok, X2} = file:pread(File, 4 * 8, 8),
	% {ok, Y2} = file:pread(File, 5 * 8, 8),
	% file:close(File),
	{ok, Ref} = dets:open_file(uidreg),
	[{_, [A1, X1, Y1, A2, X2, Y2, _, _, _, _]}|_] = lists:reverse(dets:lookup(Ref, list_to_binary(UID))),
	dets:close(Ref),
	% [binary_to_term(list_to_binary([131,70|lists:reverse(A1)])), binary_to_term(list_to_binary([131,70|lists:reverse(X1)])), binary_to_term(list_to_binary([131,70|lists:reverse(Y1)])), binary_to_term(list_to_binary([131,70|lists:reverse(A2)])), binary_to_term(list_to_binary([131,70|lists:reverse(X2)])), binary_to_term(list_to_binary([131,70|lists:reverse(Y2)]))].
	[A1, X1, Y1, A2, X2, Y2].

getXRange(UID) ->
	{ok, Ref} = dets:open_file(uidreg),
	[{_, [_, _, _, _, _, _, XL, XH, YL, YH]}|_] = lists:reverse(dets:lookup(Ref, list_to_binary(UID))),
	dets:close(Ref),
	[XL, XH, YL, YH].

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

getGFileValue(_, Xi, Yi, I) ->
	% Filename = ?DIR ++ UID ++ "_" ++ integer_to_list(I) ++ ".gmd",
	Filename = ?DIR ++ "DF635683432243530954_" ++ integer_to_list(I) ++ ".gmd",
	% io:format("~p~n", [Filename]),
	{ok, File} = file:open(Filename, [raw, read]),
	{ok, FileContent} = file:pread(File, (Xi * ?PERIOD + Yi) * 4 * 8, 8),
	file:close(File),
	binary_to_term(list_to_binary([131,70|lists:reverse(FileContent)])).

% GT Operation	
getGTFileValue(_, X, Y) ->
	% Filename = ?DIR ++ UID ++ "_gtor.gmd",
	Filename = ?DIR ++ "DF635683432243530954_gtor.gmd",
	{ok, File} = file:open(Filename, [raw, read]),
	{ok, FileContent1} = file:pread(File, (X * ?PERIOD + Y) * 2 * 8, 8),
	{ok, FileContent2} = file:pread(File, (X * ?PERIOD + Y) * 2 * 8 + 8, 8),
	% {ok, FileContent3} = file:pread(File, (X * ?PERIOD + Y) * 3 * 8 + 16, 8),
	file:close(File),
	% [binary_to_term(list_to_binary([131,70|lists:reverse(FileContent1)])),binary_to_term(list_to_binary([131,70|lists:reverse(FileContent2)])),binary_to_term(list_to_binary([131,70|lists:reverse(FileContent3)]))].
	[binary_to_term(list_to_binary([131,70|lists:reverse(FileContent1)])),binary_to_term(list_to_binary([131,70|lists:reverse(FileContent2)]))].

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

getHR(X, Y, I) ->
	if
		Y - X - I < 0 ->
			Y - X - I + ?PERIOD;
		true ->
			Y - X - I
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

test_h() ->
	print_test_h(0, 0).

print_test_h(A, X) ->
	H = math:atan(A * math:tan(X)),
	D = math:tan(H) - A * math:tan(X),
	io:format("~p~n", [[A, X, H, D]]),
	if
		A < 10 ->
			print_test_h(A + 1, X);
		X < 10 ->
			print_test_h(0, X + 1);
		true ->
			io:format("over, ")
	end.