%%% File    : debian.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : search for debian package names or files in debian package
%%% Created : 23 Jul 2002 by Nicolas Niclausse <nico@niclux.org>
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

-module(debian).
-author('nico@niclux.org').
-revision(' $Id: debian.erl,v 1.5 2003/08/20 16:26:27 nico Exp $ ').
-vsn(' $Revision: 1.5 $ ').

-export([search/5, parse/1, set_request/1]).

-include("mdb.hrl").

-define(debian_name, "packages.debian.org").
-define(debian_port, 80).
-define(margin, 50). % left margin for file results

search(Keywords, Input, BotPid, BotName, Channel) ->
    mdb_search:search({Keywords,
                       Input, BotPid, BotName, Channel,
                       #search_param{type   = ?MODULE, 
                                     server = ?debian_name,
                                     port   = ?debian_port }
                      }).

%%----------------------------------------------------------------------
%% Func: parse/1
%% Purpose: Parse data
%% Returns: {stop, Result} | {stop} | {continue} | {continue, Result}
%%      continue -> continue parsing of incoming data
%%      stop     -> stop parsing of incoming data
%%      Result   -> String to be printed by mdb
%%----------------------------------------------------------------------
parse("HTTP/1.1 404" ++ Data) ->
	{stop, "HTTP not found"};
parse("HTTP/1.0 404" ++ Data) ->
	{stop, "HTTP not found"};
parse("</body>" ++ Data) ->
    {stop};
parse("No responses" ++ Data) ->
    {stop,"not found"};
%% Package short description
parse("<tr><td>&nbsp; <td COLSPAN=2>" ++ Data) ->
    [Description | _Other ] = string:tokens(Data,"<"),
    {continue, "  " ++ Description};
%% URL of package
parse("\t<td><b><a HREF=\"" ++ Data) ->
    [URL | _Other ] = string:tokens(Data,"\""),
    {continue, URL };
%% Package short description, caps
parse("<TR><TD>&nbsp; <TD COLSPAN=2>" ++ Data) ->
    [Description | _Other ] = string:tokens(Data,"<"),
    {continue, "  " ++ Description};
%% URL of package, caps
parse("\t<TD><B><A HREF=\"" ++ Data) ->
    [URL | _Other ] = string:tokens(Data,"\""),
    {continue, URL };
parse(Data) ->
    %% search for files
	case regexp:first_match(Data, "[^\t]+\t+\s+<a href") of
		{match, Start, Length} -> % ok, found
            case httpd_util:split(Data, "(<|>)", 10) of
                {ok, [File, URL, Package | _Other ]} ->
                    {continue, string:left(File, ?margin, $ )++ Package };
                _ -> 
                    {continue}
            end;
        _B ->
            {continue}
    end.

%%----------------------------------------------------------------------
%% Func: set_request/1
%% Purpose: Set the request given Keywords 
%% Returns: String
%%----------------------------------------------------------------------
set_request([Type, Keyword]) ->
	set_request([Type, Keyword, "testing"]);

set_request([package, Keyword, Version]) ->
    "GET /cgi-bin/search_packages.pl?keywords=" ++ Keyword
        ++ "&searchon=names&subword=1&version=" ++ Version
        ++"&release=all HTTP/1.1" ++  io_lib:nl()
        ++ "Host: " ++ ?debian_name ++io_lib:nl() ++io_lib:nl();

set_request([file, Keyword, Version]) ->
    "GET /cgi-bin/search_contents.pl?word=" ++ Keyword 
        ++ "&searchmode=searchfiles&case=insensitive&version=" ++ Version
        ++ "&arch=i386&directories=yes HTTP/1.1" ++ io_lib:nl()
        ++ "Host: " ++ ?debian_name ++ io_lib:nl() ++ io_lib:nl().
