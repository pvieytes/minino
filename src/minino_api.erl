%%%-------------------------------------------------------------------
%%% Copyright (c) Openshine s.l.  and individual contributors.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%% 
%%%     1. Redistributions of source code must retain the above copyright notice, 
%%%        this list of conditions and the following disclaimer.
%%%     
%%%     2. Redistributions in binary form must reproduce the above copyright 
%%%        notice, this list of conditions and the following disclaimer in the
%%%        documentation and/or other materials provided with the distribution.
%%% 
%%%     3. Neither the name of Minino nor the names of its contributors may be used
%%%        to endorse or promote products derived from this software without
%%%        specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
%%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% 
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%% Minino api. 
%%% @end
%%% Created :  15 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_api).
-include("include/minino.hrl").

-export([response/3,
	 response/2,
	 path/1,
	 render_template/2,
	 build_url/2,
	 get_cookies/1,
	 get_cookie/2,
	 set_cookie/3,
	 get_session_dict/1,
	 update_session_dict/2,
	 get_session_cookie_domain/0,
	 get_session_cookie_httponly/0,
	 get_session_cookie_path/0,
	 get_session_cookie_secure/0,
	 get_file/2,
	 get_method/1,
	 get_conf/0,
	 read_settings_file/0,
	 url_params/1
	]).

%% @doc Create a valid minino response.
%% 
%% @end

-spec response(Msg::{status, Code::integer()}|
		    {status, Code::integer(), ErrorMsg::string()}|
		    string(), 
	       MReq::minino_req(),
	      Header::term()) -> R::hidden().
response(Msg, MReq, Headers) ->
    minino_req:response(Msg, MReq, Headers).

%% @doc Create a valid minino response.
%% 
%% @end

-spec response(Msg::{error, Code::integer()}|string(), MReq::minino_req()) -> R::hidden().
response(Msg, MReq) ->
    minino_req:response(Msg, MReq).


%% @doc get the request path
%% 
%% @end
-spec path(MReq::minino_req()) -> string().
path(MReq) ->
    minino_req:path(MReq).


%% @doc render a template.
%% 
%% @end
-spec render_template(template_path(), template_args()) -> {ok, string()} | {error, term()}.
render_template(Template, Args) ->
    minino_templates:render(Template, Args).



%% @doc build url.
%% <p>This function creates an url from a view id.</p>
%% @end
-spec build_url(Id::atom(), Args::[string()]) -> 
		       {ok, string()} | {error, term()}.
build_url(Id, Args) ->
  minino_dispatcher:build_url(Id, Args).


%% Sessions

%% @doc get minino session dict.
%% 
%% @end
-spec get_session_dict(MReq::minino_req()) -> dict().
get_session_dict(MReq) ->
    minino_sessions:get_dict(MReq).


%% @doc update minino session dict.
%% 
%% @end
-spec update_session_dict(MReq::minino_req(), Dict::dict()) -> 
				 {ok, MReq1::minino_req()} | {error, Error::term()}.
update_session_dict(MReq, Dict) ->
    minino_sessions:update_dict(MReq, Dict).



%% @doc get cookies.
%% 
%% @end
-spec get_cookies(MReq::minino_req()) ->[{string(), string()}]|undefined.
get_cookies(MReq) ->
    minino_sessions:get_cookies(MReq).

%% @doc get cookie.
%% 
%% @end
-spec get_cookie(MReq::minino_req(), CookieName::string()) -> string()|undefined.
get_cookie(MReq, CookieName) ->
    minino_sessions:get_cookie(MReq, CookieName).

%% @doc set cookie.
%% 
%% @end
-spec set_cookie(MReq::minino_req(), CookieName::string(), CookieVal::string()) -> 
			MReq1::minino_req() | {error, term()}.
set_cookie(MReq, CookieName, CookieVal) ->
    minino_sessions:set_cookie(MReq, CookieName, CookieVal).

%% @doc get session cookie domain
%% 
%% @end
-spec get_session_cookie_domain() -> string().
get_session_cookie_domain() ->
    minino_sessions:get_session_cookie_domain().


%% @doc get session cookie httponly
%% 
%% @end
-spec get_session_cookie_httponly() ->  true | false.
get_session_cookie_httponly() ->
    minino_sessions:get_session_cookie_httponly().


%% @doc get session cookie path
%% 
%% @end
-spec get_session_cookie_path() ->  string().
get_session_cookie_path() ->
    minino_sessions:get_session_cookie_path().

%% @doc get session cookie secure
%% 
%% @end
-spec get_session_cookie_secure() ->  true | false.
get_session_cookie_secure() ->
    minino_sessions:get_session_cookie_secure().


%% @doc get file
%% 
%% @end
-spec get_file(MReq::minino_req(), Path::string()) -> ok | {error, Error::term()}.
get_file(MReq, Path) ->
    minino_cowboy_handler:get_file(MReq, Path).

%% @doc get method
%% 
%% @end
-spec get_method(MReq::minino_req()) -> string().
get_method(MReq) ->
    minino_req:get_method(MReq).

%% @doc get conf
%% 
%% @end
-spec get_conf() -> [tuple()].
get_conf() ->
    minino_config:get().


%% @doc read minino settings file.
%% 
%% @end
-spec read_settings_file() -> {ok, [term()]} | {error, Error::term()}. 
read_settings_file() ->
    minino_config:read_file().


%% @doc get request args
%% 
%% @end
-spec url_params(MReq::minino_req()) -> [{Key::binary(), Value::binary()}].
url_params(MReq) ->
    minino_req:get_params(MReq).

