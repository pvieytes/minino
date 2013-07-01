%%minino application

-module(app_tests).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3,
	 crash_view/3]).

%% minino funs

init(_MConf) ->
    {ok, []}.

dispatch_rules() ->
    [%% {Id::atom(), RegexUrlPath::string(), view::atom()}
     {home_page, "^/$", home_view},
     {home_page2, "^/crash$", crash_view}
    ]. 



%% views

home_view(MReq, _Args, _Term) ->
    {ok, Html} = minino_api:render_template("home.html", [{text, "Meow!!"}]),
    minino_api:response(Html, MReq).

crash_view(MReq, [], _Term) ->
    crash().

crash() ->   
    1=2.
