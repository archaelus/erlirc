%%%----------------------------------------------------------------------
%%% File    : mdb_bot.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Bot behaviours and connection manager
%%% Created : 11 Aug 2002 by Dimitri Fontaine <dim@tuxfamily.org>
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

-module(mdb_bot).
-author('dim@tuxfamily.org').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([say/2, say/3, action/2, mute/2, rejoin/1, reconf/3]).

-define(timeout, 25000).

%% Configure debugging mode:
-include("mdb_macros.hrl").

%% Include record description
-include("mdb.hrl").
-include("config.hrl").
-include("log.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?MODULE, {stop}).

%% Controling the bot environment
%% Send a message a the channel the bot is connected to
say(BotPid, Message) ->
    gen_server:call(BotPid, {say, Message}, ?timeout).

say(BotPid, Message, To) ->
    gen_server:call(BotPid, {say, Message, To}, ?timeout).

action(BotPid, Message) ->
    gen_server:call(BotPid, {action, Message}, ?timeout).

mute(BotPid, NickName) ->
    gen_server:call(BotPid, {mute, NickName}, ?timeout).

%% Rejoin the channel (Use it when you have been kicked)
rejoin(BotPid) ->
    gen_server:call(BotPid, rejoin, ?timeout).

reconf(BotPid, NickName, ConfigFile) ->
    gen_server:call(BotPid, {reconf, NickName, ConfigFile}, ?timeout).

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
init([RealName, Controler, Host, Port, Passwd, Channel, BList]) ->
    mdb_logger:debug("launching a new bot: ~p~n", [Channel]),

    {ok, Sock} = mdb_connection:connect(Host, Port),
    mdb_connection:log(Sock, Channel, Passwd, RealName),

    %% To avoid some re-entrance issue when starting bot from a reconf,
    %% we may start the bot giving it its behaviours list...
    {ok, RealBList} = case BList of
			  [] ->
			      config_srv:getBList(Channel#channel.name,
						  Channel#channel.botname);
			  List ->
			      {ok, BList}
		      end,

    State = #state{bot_pid=self(),
		   channel    = Channel#channel.name,
		   controler  = Controler,
		   socket     = Sock,
		   nickname   = Channel#channel.botname,
		   passwd     = Passwd,
		   date       = calendar:local_time(),
		   behaviours = RealBList,
		   host       = Host,
		   port       = Port,
		   joined     = false,
		   mode       = unmuted
		  },
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({say, Message}, From, State=#state{socket=Sock, channel=Chan}) ->
    irc_lib:say(Sock, Chan, Message),

    {reply, ok, State};

handle_call({say, Message, To}, From,
	    State=#state{socket=Sock, channel=Chan}) ->

    irc_lib:say(Sock, To, Message),
    {reply, ok, State};

handle_call({action, Message}, From,
	    State=#state{socket=Sock, channel=Chan}) ->

    irc_lib:action(Sock, Chan, Message),
    {reply, ok, State};

handle_call(rejoin, From, State=#state{socket=Sock, channel=Chan}) ->
    irc_lib:join(Sock, Chan),
    {reply, ok, State};

handle_call({reconf, NickName, ConfigFile}, From,
	    State=#state{socket=Sock, nickname=Nick, channel=Chan}) ->

    %% First read the conf file given
    %% Then get our behaviours list, and replace it in the State
    case is_controler(NickName, State#state.controler) of
	true ->
	    case config_srv:reconf(Chan, Nick, ConfigFile) of
		{ok, BList} ->
		    irc_lib:say(Sock, Chan, NickName ++ ": reconf done !"),
		    {reply, ok, State#state{behaviours=BList}};
		Error      ->
		    irc_lib:say(Sock, Chan,
				NickName ++ ": could not reconf "
				++ ConfigFile ++ " !"),
		    {reply, {error, reconf}, State}
	    end;

	false ->
	    irc_lib:say(Sock, Chan,
			NickName ++ ": " ++
			"Who do you think you are to 'reconf' me ?"),
	    {reply, {error, controller}, State}
    end;

handle_call({mute, NickName}, From, State=#state{socket=Sock, channel=Chan}) ->
    case is_controler(NickName, State#state.controler) of
	true ->
	    case State#state.mode of
		muted   ->
		    irc_lib:action(Sock, Chan, "is back"),
		    {reply, ok, State#state{mode = unmuted}};

		unmuted ->
		    irc_lib:action(Sock, Chan, "is away"),
		    {reply, ok, State#state{mode = muted}}
	    end;

	false ->
	    irc_lib:say(Sock, Chan,
			NickName ++ ": " ++
			"Who do you think you are to mute me ?"),
	    {reply, {error, controller}, State}
    end.
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
handle_info({tcp, Socket, Data}, State = #state{joined = false}) ->
    Buffer = binary_to_list(State#state.buffer),
    List = Buffer ++ binary_to_list(Data),

    case mdb_dispatch:process_data(Socket, List, State) of
	{joined, Rest} ->
	    {noreply, State#state{joined = true,
				  buffer=list_to_binary(Rest)}};

	{pong, Rest} ->
	    %% This was just a 'ping' request
	    %% We can now join the channel
	    irc_lib:join(Socket, State#state.channel),
	    {noreply, State#state{joined = true,
				  buffer=list_to_binary(Rest)}};
	
	{ok, Rest} ->
	    {noreply, State#state{buffer=list_to_binary(Rest)}}
    end;

handle_info({tcp, Socket, Data}, State) ->
    Buffer = binary_to_list(State#state.buffer),
    List = Buffer ++ binary_to_list(Data),

    case mdb_dispatch:process_data(Socket, List, State) of
	{pong, Rest} ->
	    {noreply, State#state{buffer=list_to_binary(Rest)}};
	{ok, Rest} ->
	    {noreply, State#state{buffer=list_to_binary(Rest)}}
    end;

handle_info({tcp_einval, Socket}, State) ->
    {noreply, State};

handle_info({tcp_error, Socket}, State) ->
    {ok, NewState} = mdb_connection:manage_reconnect(State),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, State) ->
    {ok, NewState} = mdb_connection:manage_reconnect(State),
    {noreply, NewState}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    mdb_logger:notice("~p is quitting ~p [~p]~n",
		      [State#state.nickname, State#state.channel, Reason]),
    irc_lib:quit(State#state.socket, Reason),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% This function support old way where controler is not a list
is_controler(Nickname, Nickname) ->
    true;

is_controler(Nickname, Controlers) ->
    lists:member(Nickname, Controlers).
