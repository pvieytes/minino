%%minino application

-module(async).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3]).

%% minino funs

init(_MConf) ->
    {ok, []}.



dispatch_rules() ->
    [%% {Id::atom(), RegexUrlPath::string(), view::atom()}
     {home_page, "^/$", home_view}
    ].

%% views
home_view(MReq, _Args, _Term) ->
    spawn(fun() ->  create_response(MReq) end),
    minino_api:noresponse().


create_response(MReq) ->
    minino_api:async_response("async response", MReq).
