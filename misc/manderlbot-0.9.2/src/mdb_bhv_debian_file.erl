%%% File    : mdb_bhv_debian_pkg.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : search for files in debian package
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

-module(mdb_bhv_debian_file).
-vc('$Id: mdb_bhv_debian_file.erl,v 1.4 2003/11/04 20:10:38 dim Exp $ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").
-include("log.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  search for files in debian package
%%%----------------------------------------------------------------------
behaviour(Input, BotName, Data, BotPid, Channel) ->
    mdb_logger:info("DEBIAN file input: ~p~n", [Input#data.body]),
    [Key, String | Args] = string:tokens(Input#data.body," "),
    case Args of 
        [] ->
            mdb_logger:info("DEBIAN criteria: ~p~n", [String]),
            debian:search([file, String], Input, BotPid, BotName, Channel);

        [Version | _] -> % which debian distrib
            mdb_logger:info("DEBIAN criteria: ~p,~p~n", [String, Version]),
            debian:search([file, String, Version], Input,
			  BotPid, BotName, Channel)
    end.

