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
%%% Created :  6 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_WITH_PARAMS(I, Type, Params), {I, {I, start_link, [Params]}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link(Params) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Params).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Params) ->
    [MConf] = Params,
    ConfServer = ?CHILD_WITH_PARAMS(minino_config, worker, Params),
    SessionsServer = ?CHILD_WITH_PARAMS(minino_sessions, worker, Params),
    DispatcherSup = ?CHILD_WITH_PARAMS(minino_dispatcher_sup, supervisor, Params),
    TemplatesSup = ?CHILD_WITH_PARAMS(minino_templates_sup, supervisor, Params),
    Specs = [ConfServer, SessionsServer, DispatcherSup, TemplatesSup],
    AppMod = proplists:get_value(app_mod, MConf),
    AppChildSpecs = 
    	try
	    AppMod:add_children_to_main_sup(MConf)
    	catch _:_E ->
    		[]
    	end,
    {ok, {{one_for_one, 5, 10}, Specs ++ AppChildSpecs}}.
