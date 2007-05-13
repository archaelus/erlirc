%%%----------------------------------------------------------------------
%%% File    : mdb_bhv_bloto.erl
%%% Author  : Dimitri Fontaine <tux@tuxfamily.org>
%%% Purpose : Count the buzzwords and give a winner
%%% Created :  6 Mar 2002 by Dimitri Fontaine <dim@tuxfamily.org>
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

-module(mdb_bhv_bloto).
-author('tux@tuxfamily.org').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([behaviour/5]). % MDB behaviour API
-export([start_link/0, add/2, reset/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("mdb.hrl").

-define(timeout, 5000).
-define(MAX, 4). %% put here MAX-1

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  Play to business loto...
%%%----------------------------------------------------------------------
behaviour(Input, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    case bloto:add(NickFrom, Channel) of
	{winner, Nick} ->
	    mdb_bot:say(BotPid, Nick ++ " " ++ Data);

	Other ->
	    ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Nick, Channel) ->
    gen_server:call(?MODULE, {add, Nick, Channel}, ?timeout).

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
handle_call({add, Nick, Channel}, From, List) ->
    case lists:keysearch({Nick, Channel}, 1, List) of
	{value, {{Nick, Channel}, ?MAX}} ->
	    {reply, {winner, Nick}, reset(Channel, List)};

	{value, {{Nick, Channel}, N}} ->
	    NewList = lists:keyreplace({Nick, Channel}, 1, List,
				       {{Nick, Channel}, N+1}),
	    {reply, ok, NewList};

	false ->
	    {reply, ok, [{{Nick, Channel}, 1}|List]}
    end;

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


%%----------------------------------------------------------------------
%% Func: reset/3
%%  Clean the List of all the Chan occurences
%%----------------------------------------------------------------------
reset(Chan, List) -> reset(Chan, List, []).

reset(Chan, [], Acc) -> lists:reverse(Acc);

reset(Chan, [{{_Nick, Chan}, _Score}|Tail], Acc) ->
    reset(Chan, Tail, Acc);

reset(Chan, [Head|Tail], Acc) ->
    reset(Chan, Tail, [Head|Acc]).
