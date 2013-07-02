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
%%%
%%% @end
%%% Created : 3 Apr 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_req).


-include("include/minino.hrl").

-export([response/2,
	 response/3,
	 path/1,
	 get_method/1,
	 get_params/1,
	 get_body/1,
	 get_headers/1,
	 get_header/2
	]). 





response(M, MReq) ->
    response(M, MReq, []).
    
response({status, Code}, MReq, Headers) when is_integer(Code) ->
        response({status, Code, <<"">>}, MReq, Headers);

response(Msg, MReq, Headers) when is_list(Msg)->
    response({status, 200, list_to_binary(Msg)}, MReq, Headers);

response(Msg, MReq, Headers) when is_binary(Msg)->
    response({status, 200, Msg}, MReq, Headers);

response({status, Code, Msg}, MReq, Headers) when is_binary(Msg)->
    {Msg, Code, MReq, Headers};

response({status, Code, Msg}, MReq, Headers) when is_list(Msg)->
    {list_to_binary(Msg), Code, MReq, Headers}.

path(MReq) ->
    {BinPath, _} = cowboy_req:path(MReq#mreq.creq),
    binary_to_list(BinPath).

%% @doc get method
-spec get_method(MReq::minino_req()) -> string().
get_method(MReq) ->
    {MethodBin,_Req} = cowboy_req:method(MReq#mreq.creq),
    binary_to_list(MethodBin).
    

%% @doc get request args
-spec get_params(MReq::minino_req()) -> [{Key::binary(), Value::binary()}].
get_params(MReq) ->
    {Args, _Creq} = cowboy_req:qs_vals(MReq#mreq.creq),
    Args.

%% @doc get request body
-spec get_body(MReq::minino_req()) -> [{Key::binary(), Value::binary()}].
get_body(MReq) ->
    case  cowboy_req:body_qs(MReq#mreq.creq) of
	{ok, Body, _} ->
	    {ok, Body};
	Else -> Else
    end.

%% @doc get request headers
-spec get_headers(MReq::minino_req()) -> [{Key::binary(), Value::iodata()}].
get_headers(MReq) ->
   cowboy_req:headers(MReq#mreq.creq).

%% @doc get request header
-spec get_header(Name::binary(), MReq::minino_req()) -> binary()|undef.
get_header(Name, MReq) ->
    {Val, _CReq} = cowboy_req:header(Name, MReq#mreq.creq),
    Val.
	
