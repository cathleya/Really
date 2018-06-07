-module (data_handler).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-record(user_sequence, {name, seq}).
-record(users,{id,alias,uname,password,email,gender,birthday,register_time,register_ip,login_ip}).

-record(mates_sequence, {name, seq}).
-record(mates,{mateid,uid}).

-record(msgs_sequence, {name, seq}).
-record(msgs,{id,alias,name,content,srtype,mtime,groupid,status}).


%%observer:start().
%%mnesia:info(). 

% mnesia:start(),
% mnesia:wait_for_tables([users,msgs], 20000).
% application:set_env(mnesia, dir, "./database"),


generate_time()->
    {{Y,Mo,D},{H,Mi,_S}} = erlang:localtime(),
    Ty = string:join([integer_to_list(Y),integer_to_list(Mo),integer_to_list(D)],"/"),
    Ts = string:join([integer_to_list(H),integer_to_list(Mi)],":"),
    string:join([Ty,Ts]," ").

do_this_once()->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user_sequence, [{attributes, record_info(fields,user_sequence)} , {type,set}, {disc_copies, [node()]} ]),
    mnesia:create_table(users,[{attributes,record_info(fields,users)},{type,set}, {disc_copies, [node()]} ]),

    mnesia:create_table(msgs_sequence, [{attributes, record_info(fields,msgs_sequence)} , {type,set}, {disc_copies, [node()]} ]),
    mnesia:create_table(msgs,[{attributes,record_info(fields,msgs)} ,{type,ordered_set}, {disc_copies, [node()]} ]),
    mnesia:stop().


dbstart() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user_sequence, [{attributes, record_info(fields,user_sequence)} , {type,set}, {disc_copies, [node()]} ]),
    mnesia:create_table(users,[{attributes,record_info(fields,users)},{type,set}, {disc_copies, [node()]} ]),

    mnesia:create_table(msgs_sequence, [{attributes, record_info(fields,msgs_sequence)} , {type,set}, {disc_copies, [node()]} ]),
    mnesia:create_table(msgs,[{attributes,record_info(fields,msgs)} ,{type,ordered_set}, {disc_copies, [node()]} ]).

dbstop()->
    mnesia:stop().

reset_table(Table)->
    mnesia:clear_table(Table).

find_all(Table)->
    Q = qlc:q([X|| X <- mnesia:table(Table)]),
    F = fun() -> 
        qlc:e(Q) 
    end,
    {atomic, Val} = mnesia:transaction(F),
    Val.


%%===========find a user
find_a_user(Uname)->
    io:format("asking find_a_user -> ~p~n~n~n",[Uname]),
    Q = qlc:q([X || X <- mnesia:table(users),X#users.uname == Uname]),
    F = fun() -> 
        qlc:e(Q) 
    end,
    {atomic, Val} = mnesia:transaction(F),
    io:format("asking find_a_user -> ~p~n~n~n",[Val]),
    Val.

%%===========register a user
user_register(User)->
    io:format("asking user_register -> ~p~n~n~n",[User]),
    [Name,Pwd,Alias,Email,Addr] = User,
    case find_a_user(Name) of
        []->
            try 
                UId = mnesia:dirty_update_counter(user_sequence, users, 1),
                RegTime = generate_time(),
                Row = #users{id=UId,
                            alias=Alias,
                            uname=Name,
                            password=Pwd,
                            email=Email,
                            gender="0",
                            birthday="1970/10/11",
                            register_time=RegTime,
                            register_ip=Addr,
                            login_ip=Addr},
                F = fun()->
                    mnesia:write(Row)
                end,
                mnesia:transaction(F),
                {ok,success}
            catch
                _:_ ->
                    {error,server_error}
            end;
        [_]->
            {error,repeat}
    end.

create_mates_table(Tname)->
    Comtable = list_to_atom(string:concat(Tname,"_mates_sequence")),
    Utable = list_to_atom(string:concat(Tname,"_mates")),
    mnesia:create_table(Comtable, [{attributes, record_info(fields,mates_sequence)} , {type,set}, {disc_copies, [node()]} ]),
    mnesia:create_table(Utable,[{attributes,record_info(fields,mates)} ,{type,ordered_set}, {disc_copies, [node()]} ]),
    ok.

create_a_mate(Myname,Addname)->
    case find_a_user(Addname) of
        []->  {error,"it's not exist"};
        [{_,Addid,_,_,_,_,_,_,_,_,_}]->
            Comtable = list_to_atom(string:concat(Myname,"_mates_sequence")),
            Utable = list_to_atom(string:concat(Myname,"_mates")),
            UId = mnesia:dirty_update_counter(Comtable, Utable, 1),
            Row = {Utable,UId,Addid},
            F = fun()->mnesia:write(Row)end,
            mnesia:transaction(F),
            {ok,"good"}
    end.

find_mates(Myname)->
    Mates = list_to_atom(string:concat(Myname,"_mates")),
    F = fun() ->  
        Q = qlc:q([{Y#users.id,Y#users.alias,Y#users.uname,Y#users.email,Y#users.gender,Y#users.birthday } || X <- mnesia:table(Mates), Y <- mnesia:table(users), X#mates.uid =:= Y#users.id ]),  
        qlc:e(Q)  
    end, 
    {atomic,Val} = mnesia:transaction(F),
    Val.  

delete_a_mate(Myname,Mid)->
    Mates = list_to_atom(string:concat(Myname,"_mates")),
    Delete= {Mates,Mid,'_'},
    Fun = fun() ->
        List = mnesia:match_object(Delete),
        lists:foreach(fun(X) ->mnesia:delete_object(X) end, List)
    end,
    mnesia:transaction(Fun).




%%===========save a new message
add_msg(Msg)->
    io:format("asking accept a message -> ~p~n~n~n",[Msg]),
    {Alias,Name,Content,Srtype,Mtime,Groupid,Status} = Msg,
    MsgId = mnesia:dirty_update_counter(msgs_sequence, msgs, 1),
    Row = #msgs{id=MsgId,alias=Alias,name=Name,content=Content,srtype=Srtype,mtime=Mtime,groupid=Groupid,status=Status},
    F = fun()->
            mnesia:write(Row)
        end,
    mnesia:transaction(F).


% fill_users()->
%     add_user({"herry","herry","123qwe","18"}),
%     add_user({"kemp","kemp","123qwe","19"}),
%     add_user({"own","own","123qwe","20"}),
%     add_user({"timo","timo","123qwe","21"}).


% find_user(User)->
%     Q = qlc:q([X#users.keyword || X <- mnesia:table(users),X#users.name == User]),
%     F = fun() -> 
%         qlc:e(Q) 
%     end,
%     {atomic, Val} = mnesia:transaction(F),
%     Val.








