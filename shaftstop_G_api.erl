-module(shaftstop_G_api).
-compile(export_all).
-import(rfc4627, [encode/1,decode/1]).
-define(PERIOD, 1000).
-define(STEP, 1).
-define(DIR, "E:\\GFDF\\").

getGValue(UID, X, Y, I) ->
	Xp = getValueInPeriod(X),
	Yp = getValueInPeriod(Y),
	X0 = getIndex(Xp),
	Y0 = getIndex(Yp),
	X1 = X0 + ?STEP,
	Y1 = Y0 + ?STEP,
	Z00 = getGFileValue(UID, X0, Y0, I),
	Z01 = getGFileValue(UID, X0, Y1, I),
	Z10 = getGFileValue(UID, X1, Y0, I),
	Z11 = getGFileValue(UID, X1, Y1, I),
	Z0 = (Xp - X0) / ?STEP * (Z10 - Z00) + Z00,
	Z1 = (Xp - X0) / ?STEP * (Z11 - Z01) + Z01,
	(Yp - Y0) / ?STEP * (Z1 - Z0) + Z0.

getGFileValue(UID, Xi, Yi, I) ->
	Filename = ?DIR ++ UID ++ "_" ++ I ++ ".gmd",
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