%%minino application

-module(json).

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
    Headers =  [{<<"Content-Type">>, <<"application/json">>}],
    Body = "{\"json\": \"Hello World!\"}",
    minino_api:response(Body, MReq, Headers).
