%%%----------------------------------------------------------------------
%%% File    : irc_lib.erl
%%% Author  : Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% Purpose : This library gathers all functions to format and send
%%%           IRC commands.
%%%           It manage IRC server connexion, automatically answer to
%%%           server pings (necessary to stay online) and behaviour
%%%           management.
%%% Created : 11 Sep 2001, Mickaël Rémond <mickael.remond@erlang-fr.org>
%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or modify  
%%% it under the terms of the GNU General Public License as published by 
%%% the Free Software Foundation; either version 2 of the License, or   
%%% (at your option) any later version.                                
%%%
%%%----------------------------------------------------------------------
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
%%%----------------------------------------------------------------------

-module(irc_lib).

-author('mickael.remond@erlang-fr.org').
-created('Date: 20010911').
-revision(' $Id: irc_lib.erl,v 1.11 2003/11/21 13:15:45 dim Exp $ ').
-vsn(' $Revision: 1.11 $ ').

%% IRC operations
-export([pong/2, join/2, quit/2, say/3, action/3, login/4, who/4]).

%% IRC helper functions
-export([is_chanop/1,
	 nickname/1,
	 split_nick_user/1]).

-include("irc.hrl").
-include("log.hrl").

%%----------------------------------------------------------------------
%% Function: pong/2
%% Purpose:  Send a pong answer with the right id
%% Args:     Sock = socket
%%           Id   = ping id to send back to the server
%% Returns:  ok
%%     or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
pong(Sock, Id)->
    Pong = lists:append("PONG ", Id),
    command(Sock, Pong).

%%----------------------------------------------------------------------
%% join/2
%% Join a discussion channel
%%----------------------------------------------------------------------
join(Sock, Channel) ->
    Command = lists:append("JOIN ", Channel),
    command(Sock, Command).

%%----------------------------------------------------------------------
%% quit/2
%% Inform the server we are quitting
%%----------------------------------------------------------------------
quit(Sock, Message) ->
    Command = lists:append("QUIT :", Message),
    command(Sock, Command).

%%----------------------------------------------------------------------
%% say/3
%% Say something in the given channel
%%----------------------------------------------------------------------
say(Sock, Channel, Message) ->
    Command = lists:concat(["PRIVMSG ", Channel, " :", Message]),
    %% Command = io_lib:format("PRIVMSG ~s :~s", [Channel, Message]),
    command(Sock, Command).

%%----------------------------------------------------------------------
%% say/3
%% Say something in the given channel
%%----------------------------------------------------------------------
action(Sock, Channel, Message) ->
    Command = "PRIVMSG " ++ Channel ++ " :"
	++ [1] ++ "ACTION " ++ Message ++ [1],
    command(Sock, Command).

%%----------------------------------------------------------------------
%% login/4
%% Send the user information to terminate the log in phase
%%----------------------------------------------------------------------
login(Sock, Nickname, Passwd, Realname) ->
    PassCommand = "PASS " ++ Passwd,
    command(Sock, PassCommand),

    NickCommand = "NICK " ++ Nickname,
    command(Sock, NickCommand),

    %% The username, hostname, servername and realname. Hostname and
    %% servername are only used in server to server communication
    UserCommand = lists:concat(["USER ", Nickname,
				" dummy dummy :", Realname]),
    command(Sock, UserCommand).

%%----------------------------------------------------------------------
%% who/4
%% Get the list of people connected to the given channel.
%%
%% In order not to break the way manderlbot get the data and parses them,
%% I have prefered to open a new connexion here.
%%----------------------------------------------------------------------
who(Host, Port, Channel, Botname) ->
    case mdb_connection:connect(Host, Port) of
	{ok, Sock} ->
	    login(Sock, "manderlbot", "passwd", Botname),
	    command(Sock, "who " ++ Channel),
	    {ok, String} = getData(Sock, []),
	    gen_tcp:close(Sock),

	    Lines = string:tokens(String, "\r\n"),

	    KeepRE = ":" ++ Host ++ " 352",
	    UserList = lists:filter(fun(Line) ->
					    case regexp:match(Line, KeepRE) of
						{match, S, L} -> true;
						NoMatch       -> false
					    end
				    end,
				    Lines),
	    lists:map(fun parseUserLine/1, UserList);
	Whatever ->
	    []
    end.

%%----------------------------------------------------------------------
%% getData/2
%% internal who/4 function, used to get the data from the server.
%%----------------------------------------------------------------------
getData(Sock, Buffer) ->
    receive 
	{tcp, Sock, Data} ->
	    case regexp:match(binary_to_list(Data), ":End of /WHO list.") of
		{match, S, L} ->
		    {ok, Buffer ++ binary_to_list(Data)};
		NoMatch ->
		    getData(Sock, Buffer ++ binary_to_list(Data))
	    end;
	
	{Error, Sock} ->
	    mdb_logger:error("Error: ~p~n", [Error]),
	    {error, Buffer};
	
	Whatever ->
	    mdb_logger:notice("Whatever: ~p~n", [Whatever]),
	    getData(Sock, Buffer)
    
    after 10000 ->
	    {ok, Buffer}
    end.

%%----------------------------------------------------------------------
%% parseUserLine/1
%% internal who/4 function, used to build the records from the irc line.
%%----------------------------------------------------------------------
parseUserLine(Line) ->    
    List = string:tokens(Line, ": "),
    #user{login = lists:nth(5, List),
	  from  = lists:nth(6, List),
	  nick  = lists:nth(8, List),
	  name  = lists:nthtail(10, List)}.


%%----------------------------------------------------------------------
%% command/2
%% Send a command to the IRC server
%%----------------------------------------------------------------------
command(Sock, Command) ->
    CompleteCmd = io_lib:format("~s~s", [Command, "\r\n"]),
    mdb_logger:debug("COMMAND: ~s~n", [Command]),
    gen_tcp:send(Sock, CompleteCmd).

%%----------------------------------------------------------------------
%% is_chanop/1
%% Returns true if the given nick is a chanop or false otherwise
%% A chanop as an '@' before its nickname.
%%----------------------------------------------------------------------
is_chanop([$@ | Nickname]) ->
    true;
is_chanop(Nickname) ->
    false.

%%----------------------------------------------------------------------
%% nickname/1
%% Return the nickname (removing '@' to chanop)
%% A chanop as an '@' before its nickname.
%%----------------------------------------------------------------------
nickname([$@ | Nickname]) ->
    Nickname;
nickname(Nickname) ->
    Nickname.

%%----------------------------------------------------------------------
%% split_nick_user/1
%% Return the nickname in lower case and
%% the user
%%----------------------------------------------------------------------
split_nick_user(HeaderFrom) ->
    %% Split the string between Nick and User (separated by !)
    [Nick, User] = string:tokens(HeaderFrom, "!"),
    
    %% Remove chanop indicator from nick
    Nick2 = nickname(Nick),

    %% convert Nickname to lowercase
    Nick3 = misc_tools:lower_string(Nick2),

    %% Return a list: Nick, User
    [Nick3, User].
