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

change(L, N, M) -> appendpow(L, fullpowlist(N, M)).

appendpow([H|T], [HP|TP]) -> [{H,HP}|appendpow(T, TP)];
appendpow(_, []) -> [];
appendpow([], [HP|TP]) -> [{0,HP}|appendpow([], TP)].

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
bv_multi_constant(P, [H|L]) -> [P * H|L].

standardize([]) ->
	[];
standardize([{H, _}|T]) ->
	[H|standardize(T)].

bv_recover_pow(L) ->
	change(L, 2, get_maxpower(L)).

% Cipher = [[A1, X1, Y1], B1]
% CipherF = [[A1, X1, Y1], B1] / [[A2, X2, Y2], B2]
cipher_add(P1, P2, [L1, B1], [L2, B2], UID) ->
	[bv_add(bv_multi_constant(P1, L1), bv_multi_constant(P2, L2),UID), P1 * B1 + P2 * B2].
