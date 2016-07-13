-module(test2).
-export([f1/4,f2/4]).

% test2:f1("-module(test5).\n-export([f1/1]).\nf1(S)->S.",test5,f1,5).
% test2:f2("test.contract",test5,f1,5).

f1(Source,Model,Function,P) ->
	File = atom_to_list(Model) ++ ".erl",
	{ok, S} = file:open(File, write),
	file:write_file(File, Source),
	file:close(S),
	compile:file(Model),
	Model:Function(P).

f2(Filename,Model,Function,P) ->
	{ok, Source} = file:read_file(Filename),
	File = atom_to_list(Model) ++ ".erl",
	{ok, S} = file:open(File, write),
	file:write_file(File, Source),
	file:close(S),
	compile:file(Model),
	Model:Function(P).