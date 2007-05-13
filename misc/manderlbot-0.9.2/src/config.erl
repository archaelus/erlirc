%%%----------------------------------------------------------------------
%%% File    : config.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Read the manderlbot XML xonfig file
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

-module(config).
-author('dim@tuxfamily.org').

-include("config.hrl").
-include("xmerl.hrl").
-include("log.hrl").

-define(default_passwd, "h4ckd4w0rld").

-export([read/1]).
-export([read_fortunes/1]).

%%%----------------------------------------------------------------------
%%% Function: read/1
%%% Purpose:  read the xml config file
%%%----------------------------------------------------------------------
read(Filename) ->
    case xmerl_scan:file(Filename) of
        {ok, Root = #xmlElement{}} ->
            mdb_logger:debug("root: ~p~n~p~n", [Root#xmlElement.name,
						Root#xmlElement.content]),
	    {ok, parse(Root, #config{})};

	Error ->
	    {error, Error}
    end.


%%%----------------------------------------------------------------------
%%% Function: parse/2
%%% Purpose:  parse the xmerl structure
%%%----------------------------------------------------------------------
parse(Element = #xmlElement{parents = []}, #config{}) ->
    Name = getAttr(Element#xmlElement.attributes, name),
    Controler = build_list(
		  getAttr(Element#xmlElement.attributes, controler),
		  ", "),
	
    lists:foldl(fun parse/2,
		#config{name = Name, controler = Controler},
		Element#xmlElement.content);


%% parsing the Dict elements
parse(Element = #xmlElement{name=dict}, Conf = #config{dict=Dict}) ->
    Server  = getAttr(Element#xmlElement.attributes, host),
    Port    = getAttr(Element#xmlElement.attributes, port),
    Default = getAttr(Element#xmlElement.attributes, default),

    {ok, [{integer,1,IPort}],1} = erl_scan:string(Port),

    lists:foldl(fun parse/2,
		Conf#config{dict = {Server, IPort, Default}},
		Element#xmlElement.content);

%% parsing the Server elements
parse(Element = #xmlElement{name=server}, Conf = #config{servers=SList}) ->
    Server = getAttr(Element#xmlElement.attributes, host),
    Port   = getAttr(Element#xmlElement.attributes, port),
    Passwd = getAttr(Element#xmlElement.attributes, password, ?default_passwd),

    {ok, [{integer,1,IPort}],1} = erl_scan:string(Port),

    lists:foldl(fun parse/2,
		Conf#config{servers = [#server{host=Server,
					       port=IPort,
					       passwd=Passwd
					      }|SList]},
		Element#xmlElement.content);

%% Parsing the Channel element
parse(Element = #xmlElement{name=channel},
      Conf = #config{servers=[CurServ|SList]}) ->

    ChanList = CurServ#server.channels,
    Chan     = getAttr(Element#xmlElement.attributes, name),
    Bot      = getAttr(Element#xmlElement.attributes, botname),

    {ok, List, _Count} =
	regexp:gsub(getAttr(Element#xmlElement.attributes, behaviours),
		    "\s+|\t+|\n+", ","),

    B        = string:tokens(List, ","),

    lists:foldl(fun parse/2,
		Conf#config{servers = [CurServ#server{channels =
						      [#channel{name=Chan,
								botname=Bot,
								behaviours=B}
						       |ChanList]}
				       |SList]},
		Element#xmlElement.content);


%% Parsing the behaviour element
parse(Element = #xmlElement{name=behaviour},
      Conf = #config{servers=[CurServ|SList], behaviours=BList}) ->

    [CurChan|ChanList] = CurServ#server.channels,

    Name     = getAttr(Element#xmlElement.attributes, name),
    Action   = getAttr(Element#xmlElement.attributes, action),

    From     = getAttr(Element#xmlElement.attributes, from, '_'),
    To       = getAttr(Element#xmlElement.attributes, to, '_'),
    Op       = getAttr(Element#xmlElement.attributes, op, '_'),
    Option   = getAttr(Element#xmlElement.attributes, option, '_'),
    Pattern  = getAttr(Element#xmlElement.attributes, pattern, '_'),

    EFrom    = getAttr(Element#xmlElement.attributes, exl_from, '_'),
    ETo      = getAttr(Element#xmlElement.attributes, exl_to, '_'),
    EOp      = getAttr(Element#xmlElement.attributes, exl_op, '_'),
    EOption  = getAttr(Element#xmlElement.attributes, exl_option, '_'),
    EPattern = getAttr(Element#xmlElement.attributes, exl_pattern, '_'),

    Data     = getText(Action, Element#xmlElement.content),

    lists:foldl(fun parse/2,
		Conf#config{behaviours =
			    [#cfg_behaviour{name=Name, action=Action,
					    from=From, to=To,
					    op=Op, option=Option,
					    pattern = Pattern,

					    exl_from=EFrom, exl_to=ETo,
					    exl_op=EOp, exl_option=EOption,
					    exl_pattern = EPattern,

					    data=Data}
			     | BList]},
		Element#xmlElement.content);


%% Parsing other elements
parse(Element = #xmlElement{}, Conf = #config{}) ->
    lists:foldl(fun parse/2, Conf, Element#xmlElement.content);

%% Parsing non #xmlElement elements
parse(Element, Conf = #config{}) ->
    Conf.


%%%----------------------------------------------------------------------
%%% Function: getAttr/2
%%% Purpose:  search the attibute list for the given one
%%%----------------------------------------------------------------------
getAttr(Attr, Name) -> getAttr(Attr, Name, "").

getAttr([Attr = #xmlAttribute{name=Name}|Tail], Name, Default) ->
    case Attr#xmlAttribute.value of
	[] -> Default;
	A  -> A
    end;

getAttr([H|T], Name, Default) ->
    getAttr(T, Name, Default);

getAttr([], Name, Default) ->
    Default.


%%%----------------------------------------------------------------------
%%% Function: getText/2
%%% Purpose:  get the text of the XML node
%%%----------------------------------------------------------------------
getText("fortune", [Text = #xmlText{value=Value}|Tail]) ->
    %% The value is a file name, we have to read it as a fortune file
    read_fortunes(string:strip(Value, both));

getText(Action, [Text = #xmlText{value=Value}|Tail]) ->
    build_list(string:strip(Value, both));

getText(Action, _Other) ->
    "".

%%%----------------------------------------------------------------------
%%% Function: build_list/1, build_list/2
%%% Purpose:  Build a list from a string, using given separator.
%%%    Default separator is '%'
%%%----------------------------------------------------------------------
build_list(String) -> build_list(String, "%").
build_list(String, Sep) ->
    string:tokens(String, Sep).


%%%----------------------------------------------------------------------
%%% Function: getText/2
%%% Purpose:  get the text of the XML node
%%%----------------------------------------------------------------------
read_fortunes(Filename) ->
    case file:read_file(Filename) of
	{ok, Content} ->
	    case lists:foldl(
		   fun([$%|Tail], Acc)       -> [[] | Acc];
		      (List, [[]|Acc])       -> [[List] | Acc];
		      (List, [Buffer | Acc]) -> [Buffer++[List] | Acc]
		   end,
		   [],
		   string:tokens(binary_to_list(Content), "\r\n")) of

		[[]| Fortunes] ->
		    %% This happens when first line of file is '%'
		    Fortunes;
		Fortunes ->
		    Fortunes
	    end;

	{error, Reason} ->
	    mdb_logger:error("Config: could not read fortune file ~s: ~s",
			     [Filename, Reason]),
	    []
    end.
