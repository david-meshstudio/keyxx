-module(keyxx_server).
-export([start/0]).

start() ->
	inets:stop(),
	application:ensure_started(inets),
	inets:start(httpd, [
			{modules, [mod_esi]},
			{port, 8367},
			{server_name, "keyxx"},
			{document_root, "www"},
			{server_root, "www"},
			{erl_script_alias, {"/api", [keyxx_controller]}}
		]).
