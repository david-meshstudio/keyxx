-module(polynomial).
-export([sum/1,add/2,multi/2,subtract/2,mv_add/4,mv_subtract/4,mv_multi/5,remove_zerotail/1,bv_add/2,bv_subtract/2,bv_multi/2]).
-compile(export_all).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

add([], []) -> [];
add([], [H|T]) -> [H|add([], T)];
add([H|T], []) -> [H|add(T, [])];
add([H1|T1], [H2|T2]) -> [H1 + H2|add(T1, T2)].

subtract(L1, L2) -> add(L1, multi2(-1, L2)).

multi(L1, L2) -> multi1(L1, L2).

multi1([], _) -> [];
multi1([H|T], L) -> add(multi2(H, L), multi1(T, [0|L])).

multi2(_, []) -> [];
multi2(H1, [H2|T2]) -> [H1 * H2|multi2(H1, T2)].

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

add_withpower([], []) -> [];
add_withpower([{H1, HP1}|T1], [{H2, _}|T2]) -> [{H1 + H2, HP1}|add_withpower(T1, T2)].

mv_add(L1, L2, N, M) -> add_withpower(change(L1, N, M), change(L2, N, M)).
mv_subtract(L1, L2, N, M) -> mv_add(L1, multi2(-1, L2), N, M).

multi_withpower_single({_, _}, []) -> [];
multi_withpower_single({H1, HP1}, [{H2, HP2}|T]) -> [{H1 * H2, add(HP1, HP2)}|multi_withpower_single({H1, HP1}, T)].

multi_withpower([], _) -> [];
multi_withpower([{H1, HP1}|T1], L2) -> multi_withpower_single({H1, HP1}, L2) ++ multi_withpower(T1, L2).

merge_bypower_single({H1, HP1}, L, []) -> [[{H1, HP1}], L];
merge_bypower_single({H1, HP1}, L, [{H2, HP2}|T]) when HP1 == HP2 -> merge_bypower_single({H1 + H2, HP1}, L, T);
merge_bypower_single({H1, HP1}, L, [{H2, HP2}|T]) -> merge_bypower_single({H1, HP1}, L ++ [{H2, HP2}], T).

merge_bypower([{H, HP}|T]) ->
	[L1, L2] = merge_bypower_single({H, HP}, [], T),
	L1 ++ merge_bypower(L2);
merge_bypower([]) -> [].

mv_multi(L1, L2, N, M1, M2) -> merge_bypower(multi_withpower(change(L1, N, M1), change(L2, N, M2))).

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

bv_add(L1, L2) -> standardize(remove_zerotail(mv_add(L1, L2, 2, max(get_maxpower(L1), get_maxpower(L2))))).
bv_subtract(L1, L2) -> bv_add(L1, multi2(-1, L2)).
bv_multi(L1, L2) -> standardize(remove_zerotail(mv_multi(L1, L2, 2, get_maxpower(L1), get_maxpower(L2)))).

standardize([]) ->
	[];
standardize([{H, _}|T]) ->
	[H|standardize(T)].

bv_recover_pow(L) ->
	change(L, 2, get_maxpower(L)).