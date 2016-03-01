-module(polynomial).
-export([add/2,multi/2,powlist/2,sum/1]).

add([], []) -> [];
add([], [H|T]) -> [H|add([], T)];
add([H|T], []) -> [H|add(T, [])];
add([H1|T1], [H2|T2]) -> [H1 + H2|add(T1, T2)].

multi(L1, L2) -> multi1(L1, L2).

multi1([], _) -> [];
multi1([H|T], L) -> add(multi2(H, L),multi1(T, [0|L])).

multi2(_, []) -> [];
multi2(H1, [H2|T2]) -> [H1 * H2|multi2(H1, T2)].

% nmulti(N, L1, L2) -> 

change(L, [H|T]) -> [{H,L}|change(nextpow(2, L), T)];
change(_, []) -> [].

powlist(N, M) ->
	L0 = initpow(N, M),
	[L0|addpowlist(M, L0)].

addpowlist(M, L) ->
	Ln = nextpow(M, L),
	case Ln == L of
		false ->
			[Ln|addpowlist(M, Ln)];
		true ->
			[]
	end.
	
nextpow(M, [H|T]) when H == M -> [H|T];
nextpow(_, [H|T]) when length(T) == 1, hd(T) > 0 -> [H + 1|[hd(T) - 1]];
nextpow(M, [H|T]) when length(T) > 1, hd(T) == M - H -> [H + 1|reinpow(T, M - H - 1)];
nextpow(M, [H|T]) -> [H|nextpow(M - H, T)].

initpow(N, M) when N == 1 -> [M];
initpow(N, M) -> [0|initpow(N - 1, M)].

reinpow([_|T], M) when T == [] -> [M];
reinpow([_|T], M) -> [0|reinpow(T, M)].

sum([H|T]) -> H + sum(T);
sum([]) -> 0.
