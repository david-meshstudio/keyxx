-module(kylinfly_server).
-compile(export_all).
start() ->
	inets:stop(),
	application:ensure_started(inets),
	inets:start(httpd, [
		{modules, [mod_esi]},
		{port, 8368},
		{server_name, "kylinfly"},
		{document_root, "www"},
		{server_root, "www"},
		{erl_script_alias, {"/api", [contract_MemberRegister_api]}}
	]).
