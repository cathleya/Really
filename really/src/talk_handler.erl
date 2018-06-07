-module (talk_handler).

-export ([imstart/0]).
-import (lists, [map/2,reverse/1]).
-import (imdb, [dbstart/0,add_msg/1,find_a_user/1]).

% -define (Options, [binary,{packet,0},{reuseaddr,true},{active,once}]).
-define (AuthOpts,[binary,{packet,0},{reuseaddr,true},{active,false}]).
-define(Port,2345).
% -define(Client,"192.168.31.100").

%%start & stop im server
imstart()->
	{ok,Listen} = gen_tcp:listen(?Port,?AuthOpts),
	spawn(fun()-> waitting(Listen) end).

%%waitting for listening connection from customer
waitting(Listen)->
	{ok,Socket} = gen_tcp:accept(Listen),
	{ok,{Address,Port}} = inet:peername(Socket),
	N0 = generate_flag(Address),
	io:format("A client connected:~p,~p~n",[N0,Port]),
	Pid = spawn(fun() -> user_login(Socket)end),
	gen_tcp:controlling_process(Socket,Pid),
	waitting(Listen).

%%user login
user_login(Socket)->
	case gen_tcp:recv(Socket,0) of
		{ok,Bin} ->
			Result = unpackage(Bin),
			{Uname,Reply} = existuser(Result),
			case Reply of
				0 ->
					Response = package(0,"success"),
					io:format("already access  ~p ~n",[Response]),
					gen_tcp:send(Socket, Response),
					inet:setopts(Socket,[{active,once}]),
					TheUser = list_to_atom(Uname),
					Pid = spawn(fun() -> consumer(Socket) end),
					
					case whereis(TheUser) of
						undefined ->
							io:format("registered   ~p  ~n",[TheUser]),
							register(TheUser,Pid);
						Pid1 ->
							io:format("existed   ~p  ~n",[TheUser]),
							exit(Pid1, normal),
							register(TheUser,Pid)
					end,
					gen_tcp:controlling_process(Socket,Pid),
					exit(self(), normal);
				1 ->
					Response = package(2,"the user isn't exist"),
					io:format("already access  ~p ~n",[Response]),
					gen_tcp:send(Socket, Response);
				2 ->
					Response = package(2,"wrong password"),
					io:format("already access  ~p ~n",[Response]),
					gen_tcp:send(Socket, Response);
				3 ->
					Response = package(3,"database error"),
					io:format("already access  ~p ~n",[Response]),
					gen_tcp:send(Socket, Response)
			end;
		{error,closed}->
			io:format("client Socket closed~n"),
        	exit(self(), normal)
	end.

existuser(Result)->
	Name = binary_to_list(proplists:get_value(<<"uname">>, Result)),
	try data_handler:find_a_user(Name) of
		[] -> {Name,1};
		[{_,_,_,_,Epwd,_,_,_,_,_,_}]-> 
			Upwd = proplists:get_value(<<"upwd">>, Result),
			Mpwd= binary_to_list(Upwd),
			case Mpwd =:= Epwd of
				true -> {Name,0};
				false -> {Name,2}
			end
	catch
		_:_ -> {Name,3}
	end.

consumer(Socket)->
	receive
		{tcp,Socket,Bin}->
			io:format("already receive  ~p ~n",[Bin]),
			CmsgBody = unpackage(Bin),
			Alias = binary_to_list(proplists:get_value(<<"alias">>, CmsgBody)),
			Name = binary_to_list(proplists:get_value(<<"name">>, CmsgBody)),
			Content = binary_to_list(proplists:get_value(<<"content">>, CmsgBody)),
			Srtype = binary_to_list(proplists:get_value(<<"srtype">>, CmsgBody)),
			Mtime = binary_to_list(proplists:get_value(<<"mtime">>, CmsgBody)),
			Groupid = binary_to_list(proplists:get_value(<<"groupid">>, CmsgBody)),
			SendTo = list_to_atom(Alias),
			io:format("already send to   ~p  ~n",[SendTo]),

			Reply = package(0,"success"),
			gen_tcp:send(Socket, Reply),
			inet:setopts(Socket,[{active,once}]),

			case whereis(SendTo) of
				undefined ->
					%%save message to mnesia
					Status = "0",
					SaveMsg = {Alias,Name,Content,Srtype,Mtime,Groupid,Status},
					data_handler:add_msg(SaveMsg),
					io:format("the usre off line  ~p ~n",[SaveMsg]);

				Pid ->
					%%save message to mnesia
					Status = "1",
					SaveMsg = {Alias,Name,Content,Srtype,Mtime,Groupid,Status},
					data_handler:add_msg(SaveMsg),

					SendBin = simplePackage(Bin),
					% SendBin = simplePackage(SaveMsg),
					Pid!{consumer,SendBin}

					% Pid!{consumer,Bin}
			end,
			consumer(Socket);
		{consumer,Message}->
			io:format("accept the message  ~p ~n",[Message]),
			gen_tcp:send(Socket, Message),
			inet:setopts(Socket,[{active,once}]),
			consumer(Socket);
		{tcp_closed,Socket}->
	    	io:format("client Socket closed~n"),
	    	exit(self(), normal)
    end.


% unpack package
unpackage(Bin)->
	<<_Header:32/native, Mbody/binary>> = Bin,
    jsx:decode(Mbody).

package(Status,State)->
	Result = #{status=>Status,msg=>list_to_binary(State)},
	Body = jsx:encode(Result),
	Leng = byte_size(Body),
	Header = <<Leng:32>>,
	<<Header/binary,Body/binary>>.

simplePackage(Bin)->
	<<Header:32/native,Body/binary>>=Bin,
	Leng = <<Header:32>>,
	io:format("send data  ~p~n~n",[Header]),
	<<Leng/binary,Body/binary>>.


% unpackage(Bin)->
%     jsx:decode(Bin).

% package(Status,State)->
% 	Result = #{status=>Status,msg=>list_to_binary(State)},
% 	jsx:encode(Result).

% simplePackage(Bin)->
% 	<<Header:32/native,Body/binary>>=Bin,
% 	Leng = <<Header:32>>,
% 	io:format("send data  ~p~n~n",[Header]),
% 	<<Leng/binary,Body/binary>>.



% simplePackage(Bin)->
% 	{Alias,Name,Content,Srtype,Mtime,Groupid,Status}=Bin,
% 	SaveMsg = #{alias=>list_to_binary(Alias),
% 				name=>list_to_binary(Name),
% 				content=>list_to_binary(Content),
% 				srtype=>list_to_binary(Srtype),
% 				mtime=>list_to_binary(Mtime),
% 				groupid=>list_to_binary(Groupid),
% 				status=>list_to_binary(Status)},
% 	Body = jsx:encode(SaveMsg),
% 	io:format("send message ~p  ~n~n~n~n",[Body]),
% 	Leng = byte_size(Body),
% 	io:format("send message ~p  ~n~n~n~n",[Leng]),
% 	Header = <<Leng:32>>,
% 	<<Header/binary, Body/binary>>.

%%
generate_flag(Address)->
	{N1,N2,N3,N4} = Address,
	N0 = string:join([integer_to_list(N1),integer_to_list(N2),integer_to_list(N3),integer_to_list(N4)],"."),
 	list_to_atom(N0).



