%%% File    : mdb_bhv_fortune.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Choose at random a line in Data and answer it.
%%% Created :  5 Dec 2003 by Dimitri Fontaine <dim@mail.cvf.fr>
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

-module(mdb_bhv_fortune).
-vc('$Id: mdb_bhv_fortune.erl,v 1.1 2003/12/05 15:09:53 dim Exp $ ').
-author('dim@tuxfamily.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  Choose at random a line in Data and answer it.
%%%----------------------------------------------------------------------
behaviour(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    {A, B, C} = now(),
    random:seed(A, B, C),
    list:foreach(fun(Message) ->
			 mdb_bot:say(BotPid, Message, NickFrom)
		 end,
		 lists:nth(random:uniform(length(Data)), Data));

behaviour(Input, BotName, Data, BotPid, Channel) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    mdb_bot:say(BotPid, lists:nth(random:uniform(length(Data)), Data)).

