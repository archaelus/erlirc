%%%----------------------------------------------------------------------
%%% File    : mdb_connection.erl
%%% Author  : Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% Purpose : Connection management library.
%%%           Used by mdb_bot.erl
%%% Created : 16 Sep 2001, Mickaël Rémond <mickael.remond@erlang-fr.org>
%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or modify  
%%% it under the terms of the GNU General Public License as published by 
%%% the Free Software Foundation; either version 2 of the License, or   
%%% (at your option) any later version.                                
%%%
%%%----------------------------------------------------------------------
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
%%%----------------------------------------------------------------------

-module(mdb_connection).

-author('mickael.remond@erlang-fr.org').
-created('Date: 20010916').
-revision(' $Id: mdb_connection.erl,v 1.10 2003/10/07 09:48:28 dim Exp $ ').
-vsn(' $Revision: 1.10 $ ').

%% External exports (API)
-export([connect/2, log/4, manage_reconnect/1]).

%% Configure debugging mode:
-include("mdb_macros.hrl").
-include("config.hrl").
-include("mdb.hrl").

%%----------------------------------------------------------------------
%% connect/2
%% Physically connects to the IRC server
%%----------------------------------------------------------------------
connect(Server, Ip_port) ->
    %% TCP connection to the IRC server
    Connect = fun() -> gen_tcp:connect(Server, Ip_port,
				       [binary,
					{packet, 0},
					{nodelay, true},
					{keepalive, true}, 
					{active, true},
					{reuseaddr, true}])
	      end,

    case Connect() of
	{ok, Sock} ->
	    %% ?dbg("Connected to ~p", [Server]),
	    {ok, Sock};

	{error, Reason} ->
	    %% If there is an error, wait 30 secondes and try to reconnect
	    ?dbg("Server connection error: ~p", [Reason]),
	    timer:sleep(30000),
	    connect(Server, Ip_port)
    end.

%% [#Port<0.80>,"#gli","h4ckd4w0rld","dtc"]},

%%----------------------------------------------------------------------
%% log/3
%% connect to a given channel
%%----------------------------------------------------------------------
log(Sock, Channel = #channel{}, Passwd, RealName) ->
    %% Logging in
    log_in(Sock, Channel#channel.botname, Passwd, RealName),

    %% Join the given channel
    irc_lib:join(Sock, Channel#channel.name);
% sometimes, Channel is not a record but the channel name only (bug ?)
log(Sock, ChannelName, Passwd, RealName) ->
    %% Logging in
    log_in(Sock, ChannelName, Passwd, RealName),

    %% Join the given channel
    irc_lib:join(Sock, ChannelName).


%%----------------------------------------------------------------------
%% log_in/3
%% Logging in: Give nick and realname to the server
%%----------------------------------------------------------------------
%%log_in(Sock, Nickname, RealName, Password) ->
log_in(Sock, Nickname, Passwd, RealName) ->
    irc_lib:login(Sock, Nickname, Passwd, RealName).
    %%irc_lib:passwd(Sock, "Password")

%%----------------------------------------------------------------------
%% manage_reconnect/1
%% When something fails, automatically reconnects the bot
%%----------------------------------------------------------------------
manage_reconnect(State) ->
    Host = State#state.host,
    Port = State#state.port,
    Chan = State#state.channel,
    Pass = State#state.passwd,
    Nick = State#state.nickname,

    {ok, Sock} = connect(Host, Port),
    log(Sock, Chan, Pass, Nick),

    {ok, State#state{socket = Sock,
		     date   = calendar:local_time(),
		     joined = false
		    }}.
