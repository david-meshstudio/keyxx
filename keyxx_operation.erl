-module(keyxx_operation).
-compile(export_all).

add([], [], _) -> [];
add([], [H|T], UID) -> [H|add([], T, UID)];
add([H|T], [], UID) -> [H|add(T, [], UID)];
add([H1|T1], [H2|T2], UID) -> [keyxx_tool:base_add(1, 1, H1, H2, UID)|add(T1, T2, UID)].

subtract(L1, L2, UID) -> add(L1, multi2(-1, L2, UID), UID).

multi(L1, L2, UID) -> multi1(L1, L2, UID).

multi1([], _, _) -> [];
multi1([H|T], L, UID) -> add(multi2(H, L, UID), multi1(T, [0|L], UID), UID).

multi2(_, [], _) -> [];
multi2(H1, [H2|T2], UID) -> [keyxx_tool:base_multiply(H1, H2, UID)|multi2(H1, T2, UID)].

fullpowlist(N, M) -> fullpowlisti(N, 0, M).

fullpowlisti(N, M, M) -> powlist(N, M);
fullpowlisti(N, I, M) -> powlist(N, I) ++ fullpowlisti(N, I+1, M).

powlist(N, M) ->
	L0 = initpow(N, M),
	[L0|addpowlist(M, L0)].

addpowlist(M, L) ->
	Ln = nextpow(M, L),
	case Ln == L of
		false -> [Ln|addpowlist(M, Ln)];
		true -> []
	end.
	
nextpow(M, [H|T]) when H == M -> [H|T];
nextpow(_, [H|T]) when length(T) == 1, hd(T) > 0 -> [H + 1|[hd(T) - 1]];
nextpow(M, [H|T]) when length(T) > 1, hd(T) == M - H -> [H + 1|reinpow(T, M - H - 1)];
nextpow(M, [H|T]) -> [H|nextpow(M - H, T)].

initpow(N, M) when N == 1 -> [M];
initpow(N, M) -> [0|initpow(N - 1, M)].

reinpow([_|T], M) when T == [] -> [M];
reinpow([_|T], M) -> [0|reinpow(T, M)].

% Multi Variable
blanklist(N) ->
	if
		N >= 0 ->
			[0|blanklist(N - 1)];
		true ->
			[]
	end.

change(L, N, M) -> appendpow(L, fullpowlist(N, M), N).

appendpow([H|T], [HP|TP], N) -> [{H,HP}|appendpow(T, TP, N)];
appendpow(_, [], _) -> [];
appendpow([], [HP|TP], N) -> [{blanklist(N),HP}|appendpow([], TP, N)].

add_withpower([], [], _) -> [];
add_withpower([{H1, HP1}|T1], [{H2, _}|T2], UID) -> [{keyxx_tool:base_add(1, 1, H1, H2, UID), HP1}|add_withpower(T1, T2, UID)].

mv_add(L1, L2, N, M, UID) -> add_withpower(change(L1, N, M), change(L2, N, M), UID).
mv_subtract(L1, L2, N, M, UID) -> mv_add(L1, multi2(-1, L2, UID), N, M, UID).

multi_withpower_single({_, _}, [], _) -> [];
multi_withpower_single({H1, HP1}, [{H2, HP2}|T], UID) -> [{keyxx_tool:base_multiply(H1, H2, UID), polynomial:add(HP1, HP2)}|multi_withpower_single({H1, HP1}, T, UID)].

multi_withpower([], _, _) -> [];
multi_withpower([{H1, HP1}|T1], L2, UID) -> multi_withpower_single({H1, HP1}, L2, UID) ++ multi_withpower(T1, L2, UID).

merge_bypower_single({H1, HP1}, L, [], _) -> [[{H1, HP1}], L];
merge_bypower_single({H1, HP1}, L, [{H2, HP2}|T], UID) when HP1 == HP2 -> merge_bypower_single({keyxx_tool:base_add(1, 1, H1, H2, UID), HP1}, L, T, UID);
merge_bypower_single({H1, HP1}, L, [{H2, HP2}|T], UID) -> merge_bypower_single({H1, HP1}, L ++ [{H2, HP2}], T, UID).

merge_bypower([{H, HP}|T], UID) ->
	[L1, L2] = merge_bypower_single({H, HP}, [], T, UID),
	L1 ++ merge_bypower(L2, UID);
merge_bypower([], _) -> [].

mv_multi(L1, L2, N, M1, M2, UID) -> merge_bypower(multi_withpower(change(L1, N, M1), change(L2, N, M2), UID), UID).

get_zeropart(L1, L2, {H, HP}, [{H2, HP2}|T]) when H == 0 -> get_zeropart(L1, L2 ++ [{H, HP}], {H2, HP2}, T);
get_zeropart(L1, L2, {H, HP}, [{H2, HP2}|T]) -> get_zeropart(L1 ++ L2 ++ [{H, HP}], [], {H2, HP2}, T);
get_zeropart(L1, L2, {H, HP}, []) when H == 0 -> [L1, L2 ++ [{H, HP}]];
get_zeropart(L1, L2, {H, HP}, []) -> [L1 ++ L2 ++ [{H, HP}], []].

remove_zerotail([{H, HP}|T]) ->
	[L1, _] = get_zeropart([], [], {H, HP}, T),
	L1.

stepsum(N, N) -> N;
stepsum(I, N) -> I + stepsum(I + 1, N).

acculatesum(N) -> stepsum(1, N + 1).

loop_find_maxpower(I, Len) ->
	case acculatesum(I) >= Len of
		true -> I;
		false -> loop_find_maxpower(I + 1, Len)
	end.

get_maxpower(L) -> loop_find_maxpower(0, length(L)).

bv_add(L1, L2, UID) -> standardize(remove_zerotail(mv_add(L1, L2, 2, max(get_maxpower(L1), get_maxpower(L2)), UID))).
bv_subtract(L1, L2, UID) -> bv_add(L1, multi2(-1, L2, UID), UID).
bv_multi(L1, L2, UID) -> standardize(remove_zerotail(mv_multi(L1, L2, 2, get_maxpower(L1), get_maxpower(L2), UID))).

bv_multi_constant(_, []) ->
	[];
bv_multi_constant(P, [[H|HL]|L]) ->
	[[P * H|HL]|bv_multi_constant(P, L)].

standardize([]) ->
	[];
standardize([{H, _}|T]) ->
	[H|standardize(T)].

bv_recover_pow(L) ->
	change(L, 2, get_maxpower(L)).

% Cipher = [[A1, X1, Y1], B1]
% CipherF = [[A1, X1, Y1], B1] / [[A2, X2, Y2], B2] = [[[A1, X1, Y1], B1],[[A2, X2, Y2], B2]]
cipher_add(P1, P2, [L1, B1], [L2, B2], UID) ->
	[bv_add(bv_multi_constant(P1, L1), bv_multi_constant(P2, L2), UID), P1 * B1 + P2 * B2].

cipher_subtract(P1, P2, [L1, B1], [L2, B2], UID) ->
	cipher_add(P1, -1 * P2, [L1, B1], [L2, B2], UID).

cipher_multiply_constant(P, [L, B]) ->
	[bv_multi_constant(P, L), P * B].

cipher_multiply([L1, B1], [L2, B2], UID) ->
	Part1 = [bv_multi(L1, L2, UID), 0],
	Part2 = cipher_multiply_constant(B1, [L2, B2]),
	Part3 = cipher_multiply_constant(B2, [L1, B1]),
	Part4 = cipher_add(1, 1, Part1, Part2, UID),
	[L, _] = cipher_add(1, 1, Part3, Part4, UID),
	[L, B1 * B2].

cipherF_add(P1, P2, [C11, C12], [C21, C22], UID) ->
	[cipher_add(P1, P2, cipher_multiply(C11, C22, UID), cipher_multiply(C21, C12, UID), UID), cipher_multiply(C12, C22, UID)].

cipherF_subtract(P1, P2, [C11, C12], [C21, C22], UID) ->
	cipherF_add(P1, -1 * P2, [C11, C12], [C21, C22], UID).

cipherF_multiply_constant(P, [C1, C2]) ->
	[cipher_multiply_constant(P, C1), C2].

cipherF_multiply([C11, C12], [C21, C22], UID) ->
	[cipher_multiply(C11, C21, UID), cipher_multiply(C12, C22, UID)].

cipherF_divid([C11, C12], [C21, C22], UID) ->
	[cipher_multiply(C11, C22, UID), cipher_multiply(C12, C21, UID)].

bv_recover_pow_result(L) ->
	changeResult(L, 2, get_maxpower(L)).

changeResult(L, N, M) -> appendpowResult(L, fullpowlist(N, M), N).

appendpowResult([H|T], [HP|TP], N) -> [[H,HP]|appendpowResult(T, TP, N)];
appendpowResult(_, [], _) -> [];
appendpowResult([], [HP|TP], N) -> [[blanklist(N),HP]|appendpowResult([], TP, N)].