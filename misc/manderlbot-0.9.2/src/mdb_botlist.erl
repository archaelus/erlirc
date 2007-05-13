%%%----------------------------------------------------------------------
%%% File    : mdb_botlist.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Manage the mbd_bot running servers, and the Sockets
%%%           already in use for each bot.
%%% Created : 16 Aug 2002 by Dimitri Fontaine <dim@tuxfamily.org>
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

-module(mdb_botlist).
-author('dim@tuxfamily.org').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).
-export([add/6, add/7, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).

-include("config.hrl").
-include("log.hrl").

%% We need a big timeout in order to be able to connect to the server.
-define(timeout, 10000).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Name, Controler, Host, Port, Passwd, Chan) ->
    gen_server:call(?MODULE,
		    {add, Name, Controler, Host, Port, Passwd, Chan, []},
		    ?timeout).

add(Name, Controler, Host, Port, Passwd, Chan, BList) ->
    gen_server:call(?MODULE,
		    {add, Name, Controler, Host, Port, Passwd, Chan, BList},
		    ?timeout).

list() ->
    gen_server:call(?MODULE, {list}, ?timeout).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(
  {add, Name, Controler, Host, Port, Pass, Chan, BList}, From, State) ->
    AlreadyStarted = fun({H, C}) when H == Host, C == Chan ->
			     true;
			(_) ->
			     false
		     end,

    %% We search our entry in the State
    case length(lists:filter(AlreadyStarted, State)) of
	1 ->
	    %% This Bot is already running
	    mdb_logger:notice(
	      "Bot ~p already running on ~p ~n", [Name, Chan#channel.name]),
	    {reply, {error, running}, State};
	
	NotFound ->
	    %% We have to start this bot
	    mdb_logger:notice("starting bot ~p on ~p~n",
			      [Name, Chan#channel.name]),

	    case mdb_bot_sup:start_child(Name, Controler,
					 Host, Port, Pass, Chan, BList) of
		{ok, Sock}      -> {reply, ok, State ++ [{Host, Chan}]};
		{error, Reason} -> {reply, {error, Reason}, State}
	    end
    end;

handle_call({list}, From, State) ->
    {reply, {ok, State}, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
