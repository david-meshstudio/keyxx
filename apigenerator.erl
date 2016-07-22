-module(apigenerator).
-compile(export_all).
-import(rfc4627,[encode/1,decode/1]).
-define(CA, "0x6B015e3c7D407977fa053e577F89A319667d3A21").
-define(ACCOUNT, "0xda353ee3b7c142e4f5c99680d1371647d0672529").

update_server() ->
	File = "kylinfly_server.erl",
	{ok, S} = file:open(File, write),
	ControllerListString = string:join(qiniulib:downloadObj("contractlist"),","),
	file:write_file(File, "-module(kylinfly_server).\r\n-compile(export_all).\r\nstart() ->\r\n\tinets:stop(),\r\n\tapplication:ensure_started(inets),\r\n\tinets:start(httpd, [\r\n\t\t{modules, [mod_esi]},\r\n\t\t{port, 8368},\r\n\t\t{server_name, \"kylinfly\"},\r\n\t\t{document_root, \"www\"},\r\n\t\t{server_root, \"www\"},\r\n\t\t{erl_script_alias, {\"/api\", [" ++ ControllerListString ++ "]}}\r\n\t]).\r\n"),
	file:close(S),
	compile:file("kylinfly_server"),
	kylinfly_server:start().

update_contract_api(ContractName) ->
	Model = "contract_" ++ ContractName ++ "_api",
	compile:file(Model),
	ContractList = qiniulib:downloadObj("contractlist"),
	case sets:is_element(Model, sets:from_list(ContractList)) of
		true ->
			false;
		false ->
			qiniulib:uploadObj("contractlist", [Model|ContractList])
	end.

gen_api_sourcefile(ContractName, AbiDefs) ->
	gen_api_sourcefile(ContractName, ?CA, ?ACCOUNT, AbiDefs).

gen_api_sourcefile(ContractName, CA, ACCOUNT, AbiDefs) ->
	File = "contract_" ++ ContractName ++ "_api.erl",
	{ok, S} = file:open(File, write),
	file:write_file(File, "-module(contract_" ++ ContractName ++ "_api).\r\n-compile(export_all).\r\n-import(etherlib,[eth_getBalance/1,eth_methodCall/3,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,hexstring2de/1]).\r\n-import(rfc4627,[encode/1,decode/1]).\r\n-define(CA, \"" ++ CA ++ "\").\r\n-define(ACCOUNT, \"" ++ ACCOUNT ++ "\").\r\ngetBalance() ->\r\n\t[_,_|L] = binary_to_list(eth_getBalance(?ACCOUNT)),\r\n\thex2de(L) / 1000000000000000000.\r\ndo(SessionID, _Env, Input) ->\r\n\tData = decode(Input),\r\n\tio:format(\"~p~n\", [Data]),\r\n\tHeader = [\"Content-Type: text/plain; charset=utf-8\/r\/n\/r\/n\"],\r\n\t{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,\r\n\tContent = \"\",\r\n\tcase binary_to_list(Command) of\r\n\t\t\"getBalance\" ->\r\n\t\t\tContent = encode(getBalance());\r\n\t\t" ++ get_control_code(AbiDefs) ++ "Other ->\r\n\t\t\tContent = {\"Unknown Query\", Other}\r\n\tend,\r\n\tmod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), \"\"]).\r\n", [write]),
	Source = get_api_code(AbiDefs),
	file:write_file(File, Source, [append]),
	file:close(S).

get_control_code([]) ->
	"";
get_control_code([AbiDef|L]) ->
	{obj, [{"constant", Constant},{"inputs",_},{"name",Name},{"outputs",_},{"type",Type}]} = AbiDef,
	if
		Constant ->
			get_control_code(L);
		true ->
			case Type of
				<<"function">> ->
					"\"" ++ binary_to_list(Name) ++ "\" ->\r\n\t\t\tContent = func_" ++ binary_to_list(Name) ++ "(Params);\r\n\t\t" ++ get_control_code(L)
			end
	end.

get_api_code([]) ->
	"";
get_api_code([AbiDef|L]) ->
	{obj, [{"constant", Constant},{"inputs",InputList},{"name",Name},{"outputs",OutputList},{"type",Type}]} = AbiDef,
	if
		Constant ->
			get_ReadDataCode(OutputList),
			get_api_code(L);
			% NamedInputList = set_Names(InputList),
			% NamedOutputList = set_Names(OutputList),
			% ParaNameString = string:join(get_InputNameList(NamedInputList),","),
			% FuncParaString = string:join(get_FunctionParaList(NamedInputList),","),
			% ReadFuncName = "read_" ++ binary_to_list(Name) ++ "_Data",
			% ReadFuncCode = get_ReadDataCode(NamedOutputList),
			% "get_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\t" ++ ReadFuncName ++ "(eth_propertyMappingCall(?CA, \"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "])).\r\n" ++ ReadFuncName ++ "(Data) ->\r\n\t" ++ ReadFuncCode ++ "\r\n" ++ get_api_code(L);
		true ->
			case Type of
				<<"function">> ->
					ParaNameString = string:join(get_InputNameList(InputList),","),
					FuncParaString = string:join(get_FunctionParaList(InputList),","),
					"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\teth_methodCall(?CA,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]).\r\n" ++ get_api_code(L)
			end
	end.

set_Names([]) ->
	[];
set_Names([Input|L]) ->
	{obj, [{"name", _}, {"type", Type}]} = Input,
	Name = string:left(string:join(string:tokens(base64:encode_to_string(crypto:hash(md5,term_to_binary(erlang:unique_integer()))),"/+="),""),8),
	[{obj, [{"name", list_to_binary(Name)}, {"type", Type}]}|set_Names(L)].

get_ReadDataCode([Output|L]) ->
	[Output|L].

get_InputNameList([]) ->
	[];
get_InputNameList([Input|L]) ->
	{obj, [{"name", Name}, {"type", _}]} = Input,
	["P_" ++ binary_to_list(Name)|get_InputNameList(L)].

get_FunctionParaList([]) ->
	[];
get_FunctionParaList([Input|L]) ->
	{obj, [{"name", Name}, {"type", Type}]} = Input,
	["{\"" ++ binary_to_list(Type) ++ "\",binary_to_list(P_" ++ binary_to_list(Name) ++ "),64,0}"|get_FunctionParaList(L)].