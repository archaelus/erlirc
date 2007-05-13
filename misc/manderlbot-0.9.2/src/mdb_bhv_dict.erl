%%% File    : mdb_bhv_dict.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : search for word definition using the DICT protocol (RFC 2229)
%%% Created : 16 Jul 2002 by Nicolas Niclausse <nico@niclux.org>
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

-module(mdb_bhv_dict).
-author('nniclausse@idealx.com').
-revision(' $Id: mdb_bhv_dict.erl,v 1.4 2003/11/04 20:10:38 dim Exp $ ').
-vsn(' $Revision: 1.4 $ ').

-export([behaviour/5]). % MDB behaviour API
-export([search/5, search/6, parse/1, set_request/1]).

-include("mdb.hrl").
-include("log.hrl").

%%%----------------------------------------------------------------------
%%% Function: dict/5
%%% Purpose:  ask dict for a word definition
%%%----------------------------------------------------------------------
behaviour(Input, BotName, Data, BotPid, Channel) ->
    [DictName] = Data,
    mdb_logger:info("DICT input: ~p~n", [Input#data.body]),
    mdb_logger:info("DICT name:  ~p~n", [DictName]),

    [Key | Args] = string:tokens(Input#data.body," "),
    Criteria = string:strip(Args),
   
    mdb_logger:info("DICT criteria: ~p~n", [Criteria]),

    case DictName of
	[] ->
	    search(Criteria, Input, BotPid, BotName, Channel);
	_ ->
	    search(Criteria, Input,
			    BotPid, BotName, Channel, DictName)
    end.

%% search with default dictionnary 
search(Keywords, Input, BotPid, BotName, Channel) ->
    mdb_logger:debug("params: ~p~n", [getConf()]),
    mdb_search:search({Keywords, Input, BotPid, BotName, Channel, getConf()}).

search(Keywords, Input, BotPid, BotName, Channel, Dict) ->
    mdb_logger:debug("params: ~p~n", [getConf()]),
    mdb_search:search({[Keywords, Dict],
		       Input, BotPid, BotName, Channel, getConf()}).

getConf() ->
    {ok, {Host, Port, Default}} = config_srv:getDictConf(),
    #search_param{type = ?MODULE, server = Host, port = Port}.

%%----------------------------------------------------------------------
%% Func: parse/1
%% Purpose: Parse data
%% Returns: {stop, Result} | {stop} | {continue} | {continue, Result}
%%      continue -> continue parsing of incoming data
%%      stop     -> stop parsing of incoming data
%%      Result   -> String to be printed by mdb
%%----------------------------------------------------------------------
parse("250" ++ Data) -> %% completed
    {stop};
parse("552" ++ Data) -> %% no match
    {stop, "not found"};
parse("150" ++ Data) -> %% response headers (n def. found)
    {continue};
parse("151" ++ Data) -> %% response headers (database name)
    {continue};
parse("") ->
    {continue};
parse(".\r\n") ->
    {continue};
parse(Data) ->
    case regexp:first_match(Data, "^[0-9][0-9][0-9] ") of
	{match,Start,Length} -> % skip protocol data
	    {continue};
	_ -> 
	    {continue, lists:subtract(Data,"\r\n")}
    end.

%%%search using Dict dictionnary
set_request([Keyword, Dict]) ->
    set_request(Keyword, Dict);

%%% get default dict configuration
set_request(Keyword) ->
    {ok, {_, _, Default}} = config_srv:getDictConf(),
    set_request(Keyword, Default).

set_request(Keyword, Dict) ->
    "DEFINE " ++ Dict ++ " " ++ Keyword	++ io_lib:nl().
