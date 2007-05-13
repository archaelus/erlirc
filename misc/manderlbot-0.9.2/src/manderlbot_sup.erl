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

-module(manderlbot_sup).
-author('dim@tuxfamily.org').

-include("mdb.hrl").
-include("config.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Config_file, Log_file) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config_file, Log_file]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([Config_file, Log_file]) ->
    Logger = {mdb_logger, {mdb_logger, start_link, []},
	      permanent, 2000, worker, [mdb_logger]},

    BotSup = {mdb_bot_sup, {mdb_bot_sup, start_link, []},
	      permanent, 2000, supervisor, [mdb_bot_sup]},

    BotLst = {mdb_botlist, {mdb_botlist, start_link, []},
	      permanent, 2000, worker, [mdb_botlist]},

    BServ  = {config_srv, {config_srv, start_link, [Config_file]},
	      permanent, 2000, worker, [config_srv]},

    BLoto  = {mdb_bhv_bloto, {mdb_bhv_bloto, start_link, []},
	      permanent, 2000, worker, [mdb_bhv_bloto]},

    Pyramid = {mdb_bhv_pyramid, {mdb_bhv_pyramid, start_link, []},
	       permanent, 2000, worker, [mdb_bhv_pyramid]},

    BSearch = {mdb_search, {mdb_search, start_link, []},
	      permanent, 2000, worker, [mdb_search]},

    {ok, {{one_for_one, 3, 60},
	  [Logger, BotSup, BotLst, BServ, BLoto, Pyramid, BSearch]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
