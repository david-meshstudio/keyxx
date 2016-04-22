-module(test2).
-export([f1/3]).

f1(Source,Model,[H|_]) ->
	File = atom_to_list(Model) ++ ".erl",
	{ok, S} = file:open(File, write),
	file:write_file(File, Source),
	file:close(S),
	compile:file(Model),
	Model:f1(H).