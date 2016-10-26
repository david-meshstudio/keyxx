-module(keyxx_operation).
-compile(export_all).
-define(KPL1, 
[[[[0,697,195],
   [22.555651037969,108,600],
   [101.075127136339,418,679]],
  [2,0]],
 [[[0,986,136],
   [72.8961325864771,799,441],
   [40.8786658795132,472,727]],
  [1,1]],
 [[[0,20,113],
   [41.6295476976904,404,75],
   [71.318706061555,546,100]],
  [0,2]]]).
-define(KPL, 
[[[[0,697,195],
   [0.022555651037969,108,600],
   [0.101075127136339,418,679]],
  [2,0]],
 [[[0,986,136],
   [0.0728961325864771,799,441],
   [0.0408786658795132,472,727]],
  [1,1]],
 [[[0,20,113],
   [0.0416295476976904,404,75],
   [0.071318706061555,546,100]],
  [0,2]]]).
-define(FC10000,[[0,651,221],[0.136806089497014010000,391,80],[-0.0992178268486624960000,835,37]]).
% -define(FC10000,[[0,888,63],[3.87633926642827980000e-01,283,244],[1.06035369384874000000e+02,791,678]]).

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

standardizeList([]) ->
	[];
standardizeList([[H, _]|T]) ->
	[H|standardizeList(T)].

bv_recover_pow(L) ->
	change(L, 2, get_maxpower(L)).

% Cipher = [[A1, X1, Y1],[A2, X2, Y2]]
cipher_add(P1, P2, L1, L2, UID) ->
	bv_add(bv_multi_constant(P1, L1), bv_multi_constant(P2, L2), UID).

cipher_subtract(P1, P2, L1, L2, UID) ->
	cipher_add(P1, -1 * P2, L1, L2, UID).

cipher_multiply_constant(P, L) ->
	bv_multi_constant(P, L).

cipher_multiply(L1, L2, UID) ->
	bv_multi(L1, L2, UID).

bv_recover_pow_result(L) ->
	changeResult(L, 2, get_maxpower(L)).

changeResult(L, N, M) -> appendpowResult(L, fullpowlist(N, M), N).

appendpowResult([H|T], [HP|TP], N) -> [[H,HP]|appendpowResult(T, TP, N)];
appendpowResult(_, [], _) -> [];
appendpowResult([], [HP|TP], N) -> [[blanklist(N),HP]|appendpowResult([], TP, N)].

% Simplify
cipher_simplify(L, UID) ->
	R = standardize(merge_bypower(cipher_simplify_part(bv_recover_pow_result(L), ?KPL, UID), UID)),
	% [_,[A1|_],[A2|_]|_] = R,
	% io:format("~p~n", [L]),
	% if
	% 	abs(A1) > 10000; abs(A2) > 10000 ->
	% 		% io:format("cs ~p~n", [[A1,A2]]),
	% 		cipher_simplify(cipher_multiply(cipher_multiply_constant(0.0001, R), ?FC10000, UID), UID);
	% 	true ->
	% 		bv_recover_pow_result(R)
	% end.
	bv_recover_pow_result(R).

cipher_simplify2(L, UID) ->
	R = standardize(merge_bypower(cipher_simplify_part(bv_recover_pow_result(L), ?KPL, UID), UID)),
	% [_,[A1|_],[A2|_]|_] = R,
	% io:format("~p~n", [R]),
	% if
	% 	abs(A1) > 10000; abs(A2) > 10000 ->
	% 		% io:format("cs ~p~n", [[A1,A2]]),
	% 		R2 = cipher_simplify2(cipher_multiply(cipher_multiply_constant(0.0001, R), ?FC10000, UID), UID),
	% 		io:format("~p~n", [R2]),
	% 		R2;
	% 	true ->
	% 		R
	% end.
	R.

cipher_simplify_part([], _, _) ->
	[];
cipher_simplify_part([[H1, HP1]|T1], KPL, UID) ->
	H2 = find_same_power(KPL, HP1),
	case H2 of
		[] ->
			[{H1, HP1}|cipher_simplify_part(T1, KPL, UID)];
		[_, H21, H22] ->
			[{keyxx_tool:base_multiply(1000, H1, H21, UID), [0, 1]}, {keyxx_tool:base_multiply(1000, H1, H22, UID), [1, 0]}|cipher_simplify_part(T1, KPL, UID)]
	end.

find_same_power([], _) ->
	[];
find_same_power([[H2, HP2]|T], HP) ->
	% HP2L = tuple_to_list(HP2),
	if
		HP2 =:= HP ->
			H2;
		true ->
			find_same_power(T, HP)
	end.

% to binary
to_binary(C, Range, C1, UID) ->
	to_binary_part(C, 256, Range, C1, UID).

to_binary_part(C, P, Range, C1, UID) ->
	[Q, M] = exact_divid(C, P, Range, C1, UID),
	if
		P > 2 ->
			[Q|to_binary_part(standardizeList(M), P / 2, Range, C1, UID)];
		true ->
			[Q, M]
	end.

% exact division
exact_divid(C, P, Range, C1, UID) ->
	IsPositive = keyxx_compare:is_positive(C, Range, UID),
	case IsPositive of
		true ->
			exact_divid(C, P, 0, Range, C1, UID, 0);
		false ->
			[Q, CM] = exact_divid(cipher_multiply_constant(-1, C), P, 0, Range, C1, UID, 0),
			% io:format("CM = ~p~n", [CM]),			
			% [- Q - 1, bv_recover_pow_result(cipher_subtract(P, 1, C1, standardizeList(CM), UID))];
			if
				Q < 0 ->
					io:format("here ~p~n", [Q]),			
					[-Q, bv_recover_pow_result(cipher_multiply_constant(-1, standardizeList(CM)))];
				true ->
					[Q, CM]
			end;
		unknown ->
			exact_divid(C, P, 0, Range, C1, UID, 0)
	end.

exact_divid(C, P, Q, Range, C1, UID, HasUnknown) ->
	IsPositive = keyxx_compare:is_positive(cipher_subtract(1, Q * P, C, C1, UID), Range, UID),
	case IsPositive of
		true ->
			% io:format("Q = ~p~n", [Q]),
			exact_divid_fast(C, P, Q + 16, Range, C1, UID, HasUnknown);
		false ->
			% io:format("HU = ~p~n", [HasUnknown]),
			if
				HasUnknown > 0 ->
					get_positive_remain(C, P, Q, 2, Range, C1, UID);
				HasUnknown =:= 0 ->
					if
						Q > 0 ->
							[Q - 1, bv_recover_pow_result(cipher_subtract(1, (Q - 1) * P, C, C1, UID))];
						true ->
							[Q, bv_recover_pow_result(cipher_subtract(1, Q * P, C, C1, UID))]
					end					
			end;			
		unknown ->
			% io:format("Q = ~p~n", [Q]),
			exact_divid_slow(C, P, Q + 1, Range, C1, UID, HasUnknown + 1)
	end.

exact_divid_fast(C, P, Q, Range, C1, UID, HasUnknown) ->
	IsPositive = keyxx_compare:is_positive(cipher_subtract(1, Q * P, C, C1, UID), Range, UID),
	case IsPositive of
		true ->
			% io:format("Q = ~p~n", [Q]),
			exact_divid_fast(C, P, Q + 16, Range, C1, UID, HasUnknown);
		false ->
			exact_divid_slow(C, P, Q - 15, Range, C1, UID, HasUnknown);			
		unknown ->
			% io:format("Q = ~p~n", [Q]),
			exact_divid_slow(C, P, Q + 1, Range, C1, UID, HasUnknown + 1)
	end.

exact_divid_slow(C, P, Q, Range, C1, UID, HasUnknown) ->
	IsPositive = keyxx_compare:is_positive(cipher_subtract(1, Q * P, C, C1, UID), Range, UID),
	case IsPositive of
		true ->
			% io:format("Q = ~p~n", [Q]),
			exact_divid_slow(C, P, Q + 1, Range, C1, UID, HasUnknown);
		false ->
			% io:format("HU = ~p~n", [HasUnknown]),
			if
				HasUnknown > 0 ->
					get_positive_remain(C, P, Q, 2, Range, C1, UID);
				HasUnknown =:= 0 ->
					if
						Q > 0 ->
							[Q - 1, bv_recover_pow_result(cipher_subtract(1, (Q - 1) * P, C, C1, UID))];
						true ->
							[Q, bv_recover_pow_result(cipher_subtract(1, Q * P, C, C1, UID))]
					end					
			end;			
		unknown ->
			% io:format("Q = ~p~n", [Q]),
			exact_divid_slow(C, P, Q + 1, Range, C1, UID, HasUnknown + 1)
	end.

get_positive_remain(C, P, Q, N, Range, C1, UID) ->
	if
		Q >= N ->
			C2 = cipher_subtract(1, (Q - N) * P, C, C1, UID),
			IsPositive = keyxx_compare:is_positive(C2, Range, UID),
			% io:format("IP = ~p~n", [IsPositive]),
			case IsPositive of
				true ->
					[Q - N, bv_recover_pow_result(C2)];
				unknown ->
					[Q - N, bv_recover_pow_result(C2)];
				false ->
					get_positive_remain(C, P, Q, N + 1, Range, C1, UID)
			end;
		true ->
			get_positive_remain(C, P, Q, N - 1, Range, C1, UID)
	end.

% refine accuracy
refine_accuracy(C, UID) ->
	AT = get_tormap(C, UID),
	[[SUMATor, _, _]|_] = lists:reverse(AT),
	SUMATorZ = round(SUMATor),
	io:format("~p~n", [AT]),
	io:format("~p~n", [SUMATorZ - SUMATor]),
	AList = get_refine_alist(AT, SUMATorZ, 0),
	io:format("~p~n", [AList]),
	change_refince_alist(C, AList).

get_tormap([_,[[A1, X1, Y1],_],[[A2, X2, Y2]|_]], UID) ->
	[Tor1,_] = keyxx_tool:getGTFileValue(UID, X1, Y1),
	[_,Tor2] = keyxx_tool:getGTFileValue(UID, X2, Y2),
	% [_,_,Tor] = keyxx_tool:getGTFileValue(UID, X1, X2),
	% Tor = 1,
	% [[0, 0, 0],[A1 * Tor1 * Tor, A1, Tor1 * Tor],[(A1 * Tor1 + A2 * Tor2) * Tor, A2, Tor2 * Tor]].
	[[0, 0, 0],[A1 * Tor1, A1, Tor1],[A1 * Tor1 + A2 * Tor2, A2, Tor2]].
	% [ATor, A, Tor] = keyxx_tool:base_get_tormap(H, UID, I),
	% [[SUMATor + ATor, A, Tor]|get_tormap(L, UID, SUMATor + ATor, I + 1)].

get_refine_alist([[_, A, Tor]|L], SUMATorZ, AccumulateATor) ->
	case L of
		[] ->
			[(SUMATorZ - AccumulateATor) / Tor];
		_ ->
			[A|get_refine_alist(L, SUMATorZ, AccumulateATor + A * Tor)]
	end.

change_refince_alist([], _) ->
	[];
change_refince_alist([[[_, X, Y|_],HP]|L], [A|AL]) ->
	[[[A, X, Y],HP]|change_refince_alist(L, AL)].