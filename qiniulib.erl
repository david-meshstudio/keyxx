-module(qiniulib).
-export([upload/2,upload/1,uploadObj/1,uploadObj/2,uploadObjZipped/1,uploadObjZipped/2,uploadJson/1,uploadJson/2,download/1,downloadObj/1,downloadObjZipped/1,downloadJson/1,delete/1,getDownloadURL/1,etag/1]).
-compile(export_all).
-import(rfc4627,[encode/1,decode/1]).
-define(AK,"SbWsVObx9qs_V1A92TlClwQjrK9oRPgPss3BAjJV").
-define(SK,"yzpctby_ZSeMgQOrq8IGne7rHsEwI7TWDeE30pdH").
-define(DOMAIN,"keyxx").
-define(UHOST,"http://upload.qiniu.com/").
-define(DHOST,"http://oa8s6st6u.bkt.clouddn.com/").
-define(AHOST,"http://rs.qiniu.com").
-define(BOUNDARY,"|||").

etag(FileContent) ->
	urlsafeBase64Encode([22|binary_to_list(crypto:hash(sha,FileContent))]).

uploadJson(Obj) ->
	FileContent = encode(Obj),
	Filename = etag(FileContent),
	upload(Filename, FileContent).

uploadJson(Filename, Obj) ->
	FileContent = encode(Obj),
	upload(Filename, FileContent).

uploadObj(Obj) ->
	FileContent = base64:encode_to_string(term_to_binary(Obj)),
	Filename = etag(FileContent),
	upload(Filename, FileContent).

uploadObj(Filename, Obj) ->
	FileContent = base64:encode_to_string(term_to_binary(Obj)),
	upload(Filename, FileContent).

uploadObjZipped(Obj) ->
	FileContent = base64:encode_to_string(term_to_binary(Obj)),
	Filename = etag(binary_to_list(zlib:zip(FileContent))),
	upload(Filename, FileContent).

uploadObjZipped(Filename, Obj) ->
	FileContent = base64:encode_to_string(term_to_binary(Obj)),
	upload(Filename, binary_to_list(zlib:zip(FileContent))).

upload(Filename, FileContent) ->
	Bucket = ?DOMAIN,
	inets:start(),
	case httpc:request(post,{?UHOST,[],"multipart/form-data;boundary="++?BOUNDARY,getRequestBody(Bucket, Filename, FileContent)},[],[]) of
		{ok, {_, _, Body}} -> Body;
		{error, Reason} -> io:format("error cause ~p~n", [Reason])
	end.

upload(Filename) ->
	Bucket = ?DOMAIN,
	inets:start(),
	case httpc:request(post,{?UHOST,[],"multipart/form-data;boundary="++?BOUNDARY,getRequestBody(Bucket, Filename)},[],[]) of
		{ok, {_, _, Body}} -> Body;
		{error, Reason} -> io:format("error cause ~p~n", [Reason])
	end.

downloadJson(Filename) ->
	FileContent = download(Filename),
	case decode(FileContent) of
		{ok, Obj, _} -> Obj;
		{error, Reason} -> io:format("error cause ~p~n", [Reason])
	end.

downloadObj(Filename) ->
	FileContent = base64:decode(download(Filename)),
	binary_to_term(FileContent).

downloadObjZipped(Filename) ->
	FileContent = base64:decode(binary_to_list(zlib:unzip(list_to_binary(download(Filename))))),
	binary_to_term(FileContent).

download(Filename) ->
	Url = ?DHOST++Filename++"?e="++getTimestampString(),
	UrlFull = Url ++"&token=" ++ getDownloadToken(Url),
	inets:start(),
	case httpc:request(get,{UrlFull,[{"Content-Type","text/html;charset=utf8"}]},[],[]) of
		{ok, {_, _, Body}} -> Body;
		{error, Reason} -> io:format("error cause ~p~n", [Reason])
	end.

getDownloadURL(Filename) ->
	Url = ?DHOST++Filename++"?e="++getTimestampString(),
	Url ++"&token=" ++ getDownloadToken(Url).

delete(Filename) ->
	Url = "/delete/"++getEncodedEntryURI(Filename),
	inets:start(),
	case httpc:request(post,{?AHOST++Url,[{"Content-Type","application/x-www-form-urlencoded"},{"Authorization","QBox "++getAccessToken(Url)}],[],[]},[],[]) of
		{ok, {_, _, Body}} -> Body;
		{error, Reason} -> io:format("error cause ~p~n", [Reason])
	end.

getRequestBody(Bucket, Filename, FileContent) ->
	"--"++?BOUNDARY++"\r\nContent-Disposition: form-data; name=\"token\"\r\n\r\n"++getUploadToken(Bucket, Filename)++"\r\n--"++?BOUNDARY++"\r\nContent-Disposition: form-data; name=\"key\"\r\n\r\n"++Filename++"\r\n--"++?BOUNDARY++"\r\nContent-Disposition: form-data; name=\"file\"; filename=\""++Filename++"\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\n"++FileContent++"\r\n--"++?BOUNDARY++"--\r\n".

getRequestBody(Bucket, Filename) ->
	{ok, File} = file:open(Filename, [raw, read]),
	{ok, FileContent} = file:read(File, filelib:file_size(Filename)),
	"--"++?BOUNDARY++"\r\nContent-Disposition: form-data; name=\"token\"\r\n\r\n"++getUploadToken(Bucket, Filename)++"\r\n--"++?BOUNDARY++"\r\nContent-Disposition: form-data; name=\"key\"\r\n\r\n"++Filename++"\r\n--"++?BOUNDARY++"\r\nContent-Disposition: form-data; name=\"file\"; filename=\""++Filename++"\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\n"++FileContent++"\r\n--"++?BOUNDARY++"--\r\n".

getUploadToken(Bucket, Filename) ->
	Policy = "{\"scope\":\""++Bucket++":"++Filename++"\",\"deadline\":"++getTimestampString()++"}",
	PolicyString = urlsafeBase64Encode(Policy),
	EncodedSign = urlsafeBase64Encode(crypto:hmac(sha, list_to_binary(?SK), PolicyString)),
	?AK++":"++EncodedSign++":"++PolicyString.

getDownloadToken(Url) ->
	EncodedSign = urlsafeBase64Encode(crypto:hmac(sha, list_to_binary(?SK), Url)),
	?AK++":"++EncodedSign.

getEncodedEntryURI(Filename) ->
	urlsafeBase64Encode(?DOMAIN++":"++Filename).

getAccessToken(Path) ->
	?AK++":"++urlsafeBase64Encode(crypto:hmac(sha, list_to_binary(?SK), Path++"\n")).

% getAccessToken(Path, Query) ->
% 	?AK++":"++urlsafeBase64Encode(crypto:hmac(sha, list_to_binary(?SK), Path++"?"++Query++"\n")).

% getAccessToken(Path, Query, Body) ->
% 	?AK++":"++urlsafeBase64Encode(crypto:hmac(sha, list_to_binary(?SK), Path++"?"++Query++"\n"++Body)).

urlsafeBase64Encode(String) ->
	TmpStr0 = base64:encode_to_string(String),
	Tmp1 = string:tokens(TmpStr0, "+"),
	TmpStr1 = string:join(Tmp1, "-"),
	Tmp2 = string:tokens(TmpStr1, "/"),
	string:join(Tmp2, "_").

getTimestampString() ->
	{M, S, _} = os:timestamp(),
	T = M * 1000000 + S + 60 * 60,
	lists:flatten(io_lib:format("~p",[T])).