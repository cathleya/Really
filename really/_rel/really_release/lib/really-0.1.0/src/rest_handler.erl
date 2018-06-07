-module (rest_handler).
% -import (really_handler, [callback/1]).
-export ([timestamp/0,register/2,login/2,trycall/2,mateslist/2,addmate/2]).

-import (data_handler, [user_register/1,find_a_user/1,find_all/1,find_mates/1,create_a_mate/2]).

package(Status,State)->
	Result = #{status=>Status,msg=>State},
	jsx:encode(Result).

timestamp() ->
    {M, S, _} = os:timestamp(),  
    M * 1000000 + S.

general_ip(Address)->
    {N1,N2,N3,N4} = Address,
    string:join([integer_to_list(N1),integer_to_list(N2),integer_to_list(N3),integer_to_list(N4)],".").

trycall(Parameter,Addr_ip)->

	io:format("asking register ~p  ~p~n~n~n",[Parameter,Addr_ip]),

	timer:sleep(3000),

	Area = really_handler:callback("rest_handler"),

	io:format("remote compute area ~p ~n~n~n",[Area]),

	Name = proplists:get_value(<<"uname">>, Parameter),
	Pwd = proplists:get_value(<<"upwd">>, Parameter),

	io:format("asking register ~p ~p ~n~n~n",[Name,Pwd]),

	Res = [Name,Pwd],

	io:format("asking register r-> ~p~n~n~n",[Res]),

	package(0,Res).


register(Parameter,Addr_ip)->
	io:format("asking register ~p~n~n~n",[Parameter]),

	Name = proplists:get_value(<<"uname">>, Parameter),
	Pwd = proplists:get_value(<<"upwd">>, Parameter),
	Alias = proplists:get_value(<<"unick">>, Parameter),
	Email = proplists:get_value(<<"uemail">>, Parameter),
	
	Res = [binary_to_list(Name),
			binary_to_list(Pwd),
			binary_to_list(Alias),
			binary_to_list(Email),
			general_ip(Addr_ip)],

	io:format("asking register r-> ~p~n~n~n",[Res]),

	case data_handler:user_register(Res) of
		 {ok,success} ->package(0,"success");
		 {error,server_error}-> package(1,"data expection");
		 {error,repeat}-> package(2,"this user have existed already")
	end.


login(Parameter,_IP)->
	io:format("asking register ~p~n~n~n",[Parameter]),

	Name = proplists:get_value(<<"uname">>, Parameter),
	Pwd = proplists:get_value(<<"upwd">>, Parameter),

	io:format("asking register ~p ~p ~n~n~n",[Name,Pwd]),

	Res = [Name,Pwd],

	io:format("asking register r-> ~p~n~n~n",[Res]),

	package(0,Res).


mateslist(Parameter,_IP)->
	Name = proplists:get_value(<<"uname">>, Parameter),
	ClientName = binary_to_list(Name),

	try find_mates(ClientName) of
		[] -> package(0,[]);
		Res ->
			Arr = lists:map(fun(X)->
			 			{Uid,Alias,Uname,Email,Gender,Birthday}=X,
						#{uid=>Uid,alias=>list_to_binary(Alias),uname=>list_to_binary(Uname),email=>list_to_binary(Email),gender=>list_to_binary(Gender),birthday=>list_to_binary(Birthday)}
			end, Res),
			package(0,Arr)
			
	catch
		_:_ ->  package(0,[])
	end.

addmate(Parameter,_IP)->
	MyName = proplists:get_value(<<"myname">>, Parameter),
	AddName = proplists:get_value(<<"addname">>, Parameter),
	case create_a_mate(binary_to_list(MyName),binary_to_list(AddName)) of
		{error,Any} -> package(1,binary_to_list(Any));
		{ok,Any}-> package(0,binary_to_list(Any))
	end.
	
	

	



