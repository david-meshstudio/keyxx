-module(keyxx_api).
-compile(export_all).
-import(rfc4627,[encode/1,decode/1]).
-import(qiniulib,[uploadObjZipped/2,downloadObjZipped/1]).

uploadG(ID,G) ->
	GObj = encode(G),
	uploadObjZipped(ID, GObj).

downloadG(ID,Dir) ->
	GObj = downloadObjZipped(ID),
	File = Dir++"\\"++ID++".gmd",
	{ok, S} = file:open(File, write),
	file:write(File, GObj, [write]),
	file:close(S).
