-module (really_handler).
-behaviour (gen_server).
-export ([init/1,terminate/2,handle_cast/2,handle_info/2,code_change/3,handle_call/3]).
-export ([start_link/0,callback/1]).

-import (data_handler, [dbstart/0,do_this_once/0]).
-import (talk_handler, [imstart/0]).

start_link()->
     io:format("i'm starting~n"),
     startCowboy(),
     io:format("start cowboy has started (~w)~n", [self()]),

     %data_handler:do_this_once(),
     data_handler:dbstart(),
     io:format("database have launched~n"),

     %% message server loaunch
     talk_handler:imstart(),
     io:format("message server have launched~n"),

     gen_server:start_link({local,?MODULE},?MODULE,[],[]).


init(_Args) ->{ok, ch1State}.
terminate(_Reason,_State)->ok.
handle_cast(_Msg,State)->{noreply,State}.
handle_info(_info,State)->{noreply,State}.
code_change(_OldVsn,State,_Extra)->{ok,State}.

handle_call({trycall,Thing},_From,State)->
     Reply = string:concat(Thing,"all the things are ok"),
     {reply,Reply,State}.

callback(Thing)->gen_server:call(?MODULE,{trycall,Thing}).


%% cowboy have launched
startCowboy()->
    Routes    = routes(),
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(my_http_listener, 100,[{port, 8088}],#{env => #{dispatch => Dispatch}}).

routes()->
[
    {'_', [
            {"/home/[...]",web_handler,[]},

            {"/", cowboy_static, {priv_file, really, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, really, "static"}}

    ]}
].

 % cowboy:set_env(my_http_listener, dispatch,cowboy_router:compile(Dispatch)),

