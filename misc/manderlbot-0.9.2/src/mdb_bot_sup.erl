%%%----------------------------------------------------------------------
%%% File    : mdb_sup.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Supervise all the bot instances (dynamic)
%%% Created : 19 Feb 2002 by Dimitri Fontaine <dim@tuxfamily.org>
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

-module(mdb_bot_sup).
-author('dim@tuxfamily.org').

-include("mdb.hrl").
-include("config.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_child/6, start_child/7]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name, Controler, Host, Port, Pass, Chan) ->
    supervisor:
	start_child(?MODULE, [[Name, Controler, Host, Port, Pass, Chan, []]]).

start_child(Name, Controler, Host, Port, Pass, Chan, BList) ->
    supervisor:
	start_child(?MODULE,
		    [[Name, Controler, Host, Port, Pass, Chan, BList]]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([]) ->
    Bot = {manderlbot, {mdb_bot, start_link, []},
	   transient, 2000, worker, [mdb_bot]},

    {ok, {{simple_one_for_one, 3, 60}, [Bot]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
