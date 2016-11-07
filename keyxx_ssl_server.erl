-module(keyxx_ssl_server).
-export([start/0]).

start() ->
	inets:stop(),
	application:ensure_started(inets),
	inets:start(httpd, [
		{modules, [mod_esi,mod_get]},
		{port, 22443},
		{server_name, "keyxx"},
		{document_root, "www"},
		{server_root, "www"},
      	{bind_address, any},
      	{socket_type, {ssl, [{certfile, "./server.crt"}, {keyfile, "./server.key"}]}},
      	{mimetypes, [{"html", "text/html"}]},
		{erl_script_timeout, 18000},
		{erl_script_alias, {"/api", [keyxx_controller]}}
	]).
