-module(contract_MemberRegister_api).
-compile(export_all).
-import(etherlib,[eth_getBalance/1,eth_methodCall/3,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,hexstring2de/1]).
-import(rfc4627,[encode/1,decode/1]).
-define(CA, "").
-define(ACCOUNT, "").
getBalance() ->
	[_,_|L] = binary_to_list(eth_getBalance(?ACCOUNT)),
	hex2de(L) / 1000000000000000000.
do(SessionID, _Env, Input) ->
	Data = decode(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8/r/n/r/n"],
	{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,
	Content = "",
	case binary_to_list(Command) of
		"getBalance" ->
			Content = encode(getBalance());
		"Withdraw" ->
			Content = func_Withdraw(Params);
		"Record" ->
			Content = func_Record(Params);
		"Deposite" ->
			Content = func_Deposite(Params);
		"Register" ->
			Content = func_Register(Params);
		Other ->
			Content = {"Unknown Query", Other}
	end,
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).
func_Withdraw(Params) ->
	[P__oid,P__amount|_] = Params,
	eth_methodCall(?CA,"Withdraw",[{"bytes32",binary_to_list(P__oid),64,0},{"uint256",binary_to_list(P__amount),64,0}]).
func_Record(Params) ->
	[P__oid,P__commit|_] = Params,
	eth_methodCall(?CA,"Record",[{"bytes32",binary_to_list(P__oid),64,0},{"address",binary_to_list(P__commit),64,0}]).
func_Deposite(Params) ->
	[P__oid,P__amount|_] = Params,
	eth_methodCall(?CA,"Deposite",[{"bytes32",binary_to_list(P__oid),64,0},{"uint256",binary_to_list(P__amount),64,0}]).
func_Register(Params) ->
	[P__oid,P__name|_] = Params,
	eth_methodCall(?CA,"Register",[{"bytes32",binary_to_list(P__oid),64,0},{"string",binary_to_list(P__name),64,0}]).
