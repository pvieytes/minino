%%minino application

-module({{ application }}).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/2,
	 test_view/2]).


%% minino funs

init(_MConf) ->
    ok.

dispatch_rules() ->
    [%% {Id::atom(), Path::[string()|atom()], view::atom()}
     {root_page, [], home_view},
     {home_page, ["home"], home_view},
     {test_page, ["test", testvalue], test_view}
    ].


%% views

home_view(Req, _Args) ->
    minino_api:response("<html><body>hello world!</body></html>", Req).

test_view(Req, Args) ->
    [TestVal] = Args,
    Html = lists:flatten(io_lib:format("<html><body>test: ~s</body></html>", [TestVal])),
    minino_api:response(Html, Req).
