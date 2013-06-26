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
%%% Created :  26 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_templates).

-behaviour(gen_server).

%%API

-export([start_link/1,
	 render/1,
	 render/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE). 


-record(state, {compiled_templates}).


-type template_path() :: string().
-type template_args() :: [{atom(), string()}].



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Params::[term()]) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).


%% @doc render a template.
-spec render(template_path()) -> {ok, string()} | {error, term()}.
render(Template) ->
    render(Template, []).

%% @doc render a template.
-spec render(template_path(), template_args()) -> {ok, string()} | {error, term()}.
render(Template, Args) ->
    Mod = gen_server:call(?SERVER, {get_template_mod, filename:split(Template)}),
    case Mod of
	undefined -> {error, "undefined template"};
	Mod -> 
	    case Mod:render(Args) of
		{ok, IoList} -> {ok, binary_to_list(
				       erlang:iolist_to_binary(IoList))
				};
		Else -> Else
	    end
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([MConf]) ->
   TemplatesDir = 
    	case proplists:get_value(templates_dir, MConf) of
    	    undefined ->  filename:join(["priv", "templates"]);
    	    Else -> Else
    	end,
    error_logger:info_msg("templates dir: ~p~n", [TemplatesDir]),
    Templates = get_templates(TemplatesDir),
    CompiledT =
    	try	    
    	    compile_templates(Templates)
    	catch _:Error ->
    		error_logger:error_msg("compile templates error: ~p~n", [Error]),
    		[]
    	end,
    {ok, #state{compiled_templates=CompiledT}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_template_mod, Template}, _From, State) ->
    Mod = proplists:get_value(Template, State#state.compiled_templates),
    {reply, Mod, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



compile_templates(Templates)->
    compile_templates_loop(Templates, []).

compile_templates_loop([], Acc)->
    Acc;

compile_templates_loop([{RelativePath, Path}|Tail], Acc) ->
    Aux = string:tokens(string:join(RelativePath, "_"), "."),
    Mod = list_to_atom("tpl_" ++ string:join(Aux, "_")),
    ok = erlydtl:compile(Path, Mod),
    compile_templates_loop(Tail, [{RelativePath, Mod}|Acc]).


get_templates(TemplatesDir) ->
    case filelib:is_dir(TemplatesDir) of
	false -> [];
	true ->
	    
	    Templates = get_templates_recursive(TemplatesDir),
	    SplitedTemDir = filename:split(TemplatesDir),
	    lists:map(fun(T) -> 
			      RPath = get_relative_path(SplitedTemDir, filename:split(T)),
			      {RPath, T}
		      end,
		      Templates)
    end. 


get_templates_recursive(Filename) ->
    case filelib:is_dir(Filename) of
	true ->
	    Sons = filelib:wildcard(filename:join(Filename, "*")),
	    lists:foldl(
	      fun(F, Acc)->
		      R = get_templates_recursive(F),
		      Acc ++ R
	      end,
	      [],
	      Sons);
	false ->
	    [Filename]
    end.



get_relative_path([], T) ->
    T;			  
get_relative_path([_|BaseTail],[_|T]) ->
    get_relative_path(BaseTail, T).
