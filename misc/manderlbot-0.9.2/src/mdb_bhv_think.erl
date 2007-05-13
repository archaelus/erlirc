%%% File    : mdb_bhv_think.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : Answer the given data, but waiting for random time
%%% Created : 12 Aug 2003 by Nicolas Niclausse <nico@niclux.org>
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

-module(mdb_bhv_think).
-vc('$Id: mdb_bhv_think.erl,v 1.2 2003/08/20 16:26:28 nico Exp $ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  Answer the given data, but waiting for random time
%%%           between the 2 lines to say.
%%%----------------------------------------------------------------------
behaviour(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    case length(Data) of
	1 ->
	    [String] = Data,
	    mdb_bot:say(BotPid, String, NickFrom);

	2 ->
	    [H|T] = Data,
	    mdb_bot:say(BotPid, H, NickFrom),
	    %% we sleep for a random time
	    {A, B, C} = now(),
	    random:seed(A, B, C),
	    timer:sleep(random:uniform(?RNDTIME) + ?TIME),
	    mdb_bot:say(BotPid, T, NickFrom);

	More ->
	    behaviour(Input, BotName, Data, BotPid, Channel)
    end;

behaviour(Input, BotName, Data, BotPid, Channel) ->
    case length(Data) of
	1 ->
	    [String] = Data,
	    mdb_bot:say(BotPid, String);

	2 ->
	    [H|T] = Data,
	    mdb_bot:say(BotPid, H),
	    %% we sleep for a random time
	    {A, B, C} = now(),
	    random:seed(A, B, C),
	    timer:sleep(random:uniform(?RNDTIME) + ?TIME),
	    mdb_bot:say(BotPid, T);

	More ->
	    behaviour(Input, BotName, Data, BotPid, Channel)
    end.

