%%%----------------------------------------------------------------------
%%% File    : mdb_logger.erl
%%% Author  : Dimitri Fontaine <dim@mail.cvf.fr>
%%% Purpose : Manage to write the logs to a file
%%% Created :  7 Oct 2003 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------
%%%
%%% This file is part of Manderlbot.
%%%
%%% Manderlbot is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% Manderlbot is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% See LICENSE for detailled license
%%%
%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two. If you modify this file, you may extend this exception
%%% to your version of the file, but you are not obligated to do
%%% so.  If you do not wish to do so, delete this exception
%%% statement from your version.
%%%
%%%----------------------------------------------------------------------

-module(mdb_logger).
-author('dim@tuxfamily.org').

%%-export([Function/Arity, ...]).

-behaviour(gen_event).

%% External exports
-export([start_link/0, add_handler/1]).
-export([log/2, log/3, emerg/2, alert/2, critic/2, error/2,
	 warn/2, notice/2, info/2, debug/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

%% include
-include("log.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?MODULE}). 

add_handler({LogFile, Level}) ->
    gen_event:add_handler(?MODULE, ?MODULE, [LogFile, Level]).

%% Default to DEBUG level
log(Mesg, Args) ->
    log(Mesg, Args, ?DEBUG).

%% log with given level
log(Mesg, Args, Level) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, Level}).

%% specific logs
emerg(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?EMERG}).

alert(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?ALERT}).

critic(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?CRIT}).

error(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?ERR}).

warn(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?WARN}).

notice(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?NOTICE}).

info(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?INFO}).

debug(Mesg, Args) ->
    gen_event:notify(?MODULE, {log, Mesg, Args, ?DEBUG}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init([LogFile, Level]) ->
    {ok, Fd} = file:open(LogFile, write),
    Level_num = lvl2numeric(Level),
    {ok, #log{fd=Fd, level=Level_num}}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event({log, Mesg, Args}, State) ->
    do_log(Mesg, Args, State#log.fd),
    {ok, State};

%% when the loglevel is given, log only if the level is high enough
handle_event({log, Mesg, Args, Level}, State) when State#log.level >= Level ->
    do_log(Mesg, Args, State#log.fd),
    {ok, State};

handle_event(Event, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    file:close(State#log.fd).

%%----------------------------------------------------------------------
%% Func: code_change/3
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
do_log(Mesg, Args, Fd) ->
    {{Y, Mth, D}, {H, Min, S}} = calendar:local_time(),
    io:format(Fd, "~p/~p/~p ~p:~p:~p ", [Y, Mth, D, H, Min, S]),
    io:format(Fd, Mesg, Args).

lvl2numeric(emerg)  -> ?EMERG;
lvl2numeric(alert)  -> ?ALERT;
lvl2numeric(crit)   -> ?CRIT;
lvl2numeric(err)    -> ?ERR;
lvl2numeric(warn)   -> ?WARN;
lvl2numeric(notice) -> ?NOTICE;
lvl2numeric(info)   -> ?INFO;
lvl2numeric(debug)  -> ?DEBUG;
lvl2numeric(_)      -> ?DEBUG.
