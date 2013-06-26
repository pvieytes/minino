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

-module(minino_escript).

%%{Name::string(), Help()::string(), Available:: always | appcreated
-define(COMMANDS, 
	[
	 %%{Name::string(), Help()::string(), Available:: always | appcreated
	 {"create-app", "create a new app; create-app id=myapp", always},
	 {"runserver", "runserver [port]", appcreated},
	 {"debug", "debug [port]", appcreated},
	 {"stop", "stop minino", always},
	 {"compile", "compile", appcreated}

	]).

%% API
-export([main/1]).


main(Args) ->

    Status = get_status(),
    Commands = get_commands(Status),
    {ok, {[], CommandArgs}} = getopt:parse([], Args),
    case is_command(CommandArgs, Commands) of
	true -> 
	    case command(CommandArgs) of
		ok -> ok;
		notavailable -> usage(Commands)
	    end;
	false ->
	    usage(Commands)
    end.


is_command([],_) ->
    false;
is_command([Command|_], {AvailableCommnads, _}) ->
    is_command_loop(Command, AvailableCommnads).
	
is_command_loop(_Command, []) -> 
    false;
is_command_loop(Command, [{Command, _, _}|_]) -> 
    true;
is_command_loop(Command, [_Head|Commands]) -> 
    is_command_loop(Command, Commands). 

%% noapp | appcreated
get_status()->
    Settings = 
	case os:getenv("MININOSETTINGS") of
	    false -> filename:join(["priv", "settings.cfg"]);
	    S -> S
	end,
    case filelib:is_regular(Settings) of
	true -> 
	    appcreated;
	false -> noapp
    end.


get_commands(Status)->
    Acc0 = {[],[]},
    lists:foldr(
      fun({_Command, _Help, always}=C, {CAvai, CNoAvai})->
      	      {[C|CAvai], CNoAvai};
      	 (C, {CAvai, CNoAvai})->	      
      	      case Status of
      	      	  appcreated -> 
      	      	      {[C|CAvai], CNoAvai};
      	      	  _Esle ->
      		      {CAvai, [C|CNoAvai]}
      	      end
      end,
      Acc0, 
      ?COMMANDS).


usage({AvailableCommands, NoAvailableCommands}) ->  
    io:format("~n"),
    io:format("minino commands: ~n~n"),
    print_commands(AvailableCommands),
    case NoAvailableCommands of
	[] -> ok;
	NoAvailableCommands ->
	    io:format("~n"),
	    io:format("no app was created yet. Please create one~n"),
	    io:format("These commands will be available after app is created~n~n"),
	    print_commands(NoAvailableCommands)
    end.

print_commands(Commands) ->
    lists:foreach(
      fun({Name, Help, _A}) ->
	      io:format("~s\t\t\t~s~n", [Name, Help])
      end,
      Commands).


command(CommandArgs)->
    case CommandArgs of
	["create-app",[$i,$d,$=|AppName]] ->
	    create_app(AppName);
	["runserver" | PortList] ->
	    runserver(PortList, nodebug, undefined);
     	["debug" | PortList] ->
	    debug(PortList);
	["stop"] ->
	    stop_minino();
	["compile"|_Args] ->
	    compile_files(),
	    io:format("~n");
	_ -> notavailable
    end.



debug(PortList) ->
    {ok, Settings} = minino_api:read_settings_file(),
    set_node_name('escript', Settings),
    MininoNode = get_minino_node(Settings),
    io:format("~p ping -> ~p~n", [MininoNode, net_adm:ping(MininoNode)]),
    case net_adm:ping(MininoNode) of
	pang ->
	    runserver(PortList, debug, undefined);
	pong ->
	    error_logger:info_msg("Minino was already running.~n" ++
				      "This a remote shell~n"),
	    debug_shell(MininoNode)
    end.
    

runserver(PortList, Mode, RemoteNode) ->
    case PortList of
	[PStr] when is_list(PStr) ->	
	    {Port, _} = string:to_integer(PStr),
	    application:set_env(minino, port, Port);
	_ -> ignore
    end,
    compile_files(),
    ok = minino:start(),
    erlang:register(?MODULE, self()),
    case Mode of 
 	nodebug ->
	    receive 
		stop -> ok
	    end;
	debug -> 
	    debug_shell(RemoteNode)
    end.


debug_shell(undefined) ->
    spawn(fun()-> user_drv:start() end),
    receive
	stop -> ignore
    end;
debug_shell(Node) ->
    Pname = 'tty_sl -c -e',
    Shell = {Node,shell,start,[]},
    spawn(fun()-> user_drv:start(Pname, Shell) end),
    receive
	stop -> ignore
    end.

create_app(AppName) ->
    io:format("~s app created.~n", [AppName]),

    %%create app
    AppFileName = filename:join(["src", AppName ++ ".erl"]),
    case filelib:is_regular(AppFileName) of
	true -> 
	    ignore;
	false ->
	    AppBin = get_bin("template.application.erl"),
	    AppCtx = dict:from_list([{application, AppName}]),
	    AppStr = render(binary_to_list(AppBin), AppCtx),
	    filelib:ensure_dir(AppFileName),
	    file:write_file(AppFileName, list_to_binary(AppStr))
    end,

    %%create app.src
    AppSrcFileName = filename:join(["src", AppName ++ ".app.src"]),
    case filelib:is_regular(AppSrcFileName) of
	true -> 
	    ignore;
	false ->
	    AppSrcBin = get_bin("template.application.app.src"),
	    AppSrcCtx = dict:from_list([{application, AppName}]),
	    AppSrcStr = render(binary_to_list(AppSrcBin), AppSrcCtx),
	    filelib:ensure_dir(AppSrcFileName),
	    file:write_file(AppSrcFileName, list_to_binary(AppSrcStr))
    end,

    %% create settings.cfg
    SettingsFileName = filename:join(["priv", "settings.cfg"]),
    case filelib:is_regular(SettingsFileName) of
	true -> 
	    ignore;
	false ->
	    SetBin = get_bin("template.settings.cfg"),
	    SetCtx = dict:from_list([{application, AppName},
				     {random_string, create_random_string()}]),
	    SetStr = render(binary_to_list(SetBin), SetCtx),
	    filelib:ensure_dir(SettingsFileName),
	    file:write_file(SettingsFileName, list_to_binary(SetStr))
    end,

    %% create html templates
    HomeTemplatePath = filename:join(["priv", "templates", "home.html"]),
    case filelib:is_regular(HomeTemplatePath) of
	true -> 
	    ignore;
	false ->
	    HomeBin = get_bin("template.home.html"),
	    filelib:ensure_dir(HomeTemplatePath),
	    file:write_file(HomeTemplatePath, HomeBin)
    end.

get_bin(FileName) ->
    {ok, PropList} = escript:extract("minino", []),
    Archive = proplists:get_value(archive, PropList),
    {ok, [{FileName,Bin}]} = 
	zip:foldl(
	  fun(FileInArchive, _GetInfo, GetBin, Acc) ->
		  case FileInArchive of
		      FileName -> [{FileInArchive, GetBin()}|Acc];
		      _ -> Acc
		  end		      
	  end,
	  [],
	  {"minino", Archive}),
    Bin.

render(Bin, Context) ->
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
    mustache:render(Str1, Context).


create_random_string()->
       create_random_string(50, 1, []).

create_random_string(Length, Counter, Acc) when Length == Counter ->
    Acc;

create_random_string(Length, Counter, Acc)->
    FirstChar = 64,
    LastChar = 122,
    Char = random:uniform(LastChar - FirstChar + 1) + FirstChar - 1,
    create_random_string(Length, Counter + 1, [Char|Acc]).
    


compile_files()->
    case filelib:is_regular("rebar") of
	true ->
	    rebar_compilation();
	false ->
	    Files = filelib:wildcard("src/*.erl"),
	    ok = filelib:ensure_dir("ebin/dummy.file"),
	    Opts = [verbose,
		    report_errors,
		    report_warnings,
		    {i, ["include"]},
		    {outdir, "ebin"}
		    
		   ],
	    lists:foreach(
	      fun(Path) ->
		      Result = compile:file(Path, Opts),
		      io:format("compile ~p -> ~p", [Path, Result]),
		      {ok, _Mod} = Result
	      end,
	      Files)
    end.

stop_minino()->
    {ok, Settings} = minino_api:read_settings_file(),
    set_node_name('escript', Settings),
    MininoNode = get_minino_node(Settings),
    io:format("stop: ~p~n", [MininoNode]),
    rpc:call(MininoNode, minino, stop, []),
    ok.


set_node_name(Name, Settings)->
    net_kernel:start([Name, shortnames]),
    Cookie = proplists:get_value(cookie, Settings, 'minino_default_cookie'),
    erlang:set_cookie(node(), Cookie).

get_minino_node(Settings) ->
    Node = node(),
    MininoNode = proplists:get_value(node_name, Settings),
    [_Name, Domain] = string:tokens(atom_to_list(Node), "@"),
    list_to_atom(
      atom_to_list(MininoNode) ++ 
	  "@" ++ 
	  Domain).



rebar_compilation() ->
    Pid = self(),
    spawn(fun() -> rebar_comp_process(Pid) end),
    receive
	finished -> ok
    after 5*60*1000 ->
	    timeout
    end.

rebar_comp_process(Pid)->
    Port = open_port({spawn,"./rebar get-deps compile"},
		     [binary,
		      {line, 255}, 
		      exit_status]),
    rebar_comp_loop(Port, Pid).


rebar_comp_loop(Port, Pid) ->
    receive
	{Port,{data, {eol, Bin}}} ->
	    io:format("~s~n",[erlang:binary_to_list(Bin)]),
	    rebar_comp_loop(Port, Pid);
	{Port,{exit_status,0}} ->
	    Pid ! finished
    end.
