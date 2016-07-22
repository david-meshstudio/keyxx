-module(keyxx_api).
-compile(export_all).
-import(rfc4627,[encode/1,decode/1]).
-import(qiniulib,[uploadObjZipped/2,downloadObjZipped/1]).

uploadG(ID,G) ->
	GObj = encode(G),
	uploadObjZipped(ID, GObj).