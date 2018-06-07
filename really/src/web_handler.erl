-module (web_handler).
% -import (chat_handler, [register/2,login/2,trycall/2]).
-export ([init/2]).



%%%%%%%%%%%%%%%%%%%%mapped request into modules&functione%%%%%%%%%%%%%%%%%%%%%%%

init(Req, Opts) ->
	try cowboy_req:method(Req) of
		 Method ->
		 	Req1 = maybe_echo(Method,Req),
		 	{ok, Req1, Opts}
	catch
		_:_ ->
			Req1 = cowboy_req:reply(400, Req),
			{ok, Req1, Opts}
	end.
	
maybe_echo(<<"POST">>,Req) ->
	try {ok, PostVals, _Req0} = cowboy_req:read_urlencoded_body(Req),
		{IP, _Port} = cowboy_req:peer(Req),
		[Controller,Func] = analyzeRouters(cowboy_req:path(Req)),
		Response = apply(Controller,Func,[PostVals,IP]),
		cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Response, Req)
	catch
		_:_ ->
			log_the_error("response",["404"]),
			cowboy_req:reply(404, Req)
	end;

maybe_echo(<<"GET">>,Req)->
	try	QsVals = cowboy_req:parse_qs(Req),
		{IP, _Port} = cowboy_req:peer(Req),
		[Controller,Func] = analyzeRouters(cowboy_req:path(Req)),
		Response = apply(Controller,Func,[QsVals,IP]),
		cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Response, Req)
	catch
		_:_ ->
			log_the_error("response",["404"]),
			cowboy_req:reply(404, Req)
	end;

maybe_echo(_,Req)->
	cowboy_req:reply(405, Req).


log_the_error(Name,Args)->
	io:format("asking ~p ~p ~n~n~n",[Name,Args]).
	
analyzeRouters(Bin)->
	log_the_error("bin",[Bin]),
	Path = binary_to_list(Bin),
	Routers = string:tokens(Path,"/"),
	[_Home,Mod,Fun] = Routers,
	[list_to_atom(Mod),list_to_atom(Fun)].


%%%%%%%%%%%%%%%%%%%%start web servere%%%%%%%%%%%%%%%%%%%%%%%


% BigWig

