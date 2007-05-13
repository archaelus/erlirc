%%%----------------------------------------------------------------------
%%% File    : misc_tools.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Purpose : This module gather various generic functions
%%%           we used for Manderlbot developpment
%%% Created : 16 Nov 2000, Mickael Remond <mickael.remond@erlang-fr.org>
%%%
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
%%%
%%%----------------------------------------------------------------------

-module(misc_tools).
-author('mickael.remond@erlang-fr.org').
-created('Date: 20001116').
-revision(' $Revision: 1.11 $ ').
-vsn(' $Id: misc_tools.erl,v 1.11 2003/11/04 20:10:38 dim Exp $ ').

-export([nth/2,
	 regexpize/1,
	 date_to_integer/1,
	 date_to_string/2,
	 join/2,
	 last_event/1,
	 lower_string/1,
	 upper_string/1,
	 downcase/1,
	 is_matching_regexp/2,
	 is_matching_regexp/3]).

-include("log.hrl").

%%----------------------------------------------------------------------
%% Function: nth/2
%% Purpose:  Returns the element of a list according to its position
%% Args:     N = element position
%%           List = list to extract the element from
%% Returns:  The requested element
%%           or "" if the requested element does not exist
%%----------------------------------------------------------------------
nth(N,List) when N > length(List) ->
    "";
nth(N, List) when N =< length(List) ->
    lists:nth(N, List).

%%---------------------------------------------------------------------
%% Utility fonction : regexpize
%% arg : a token in caps (ex DATE)
%% return : a regexp allowing any combination of uppercase/lowercase
%% for example : DATE gives [Dd][Aa][Tt][Ee]
%% --------------------------------------------------------------------
regexpize(Token) when list(Token) ->
    %% This fonction transform the element "E" for example to
    %% "[Ee]" 
    F = fun(Element) ->
		"["++
		    [Element] ++
		    [lower_char(Element)] ++
		    "]"
	end,

    %% Convert all the token using this function
    lists:flatmap(F, Token).  

%%--------------------------------------------------------------------
%% lower_char/1
%% It seems there isn't a lower/upper function in stdlib, so here's a
%% quick hack, using specific ASCII charset
%% FIXME : maybe not portable
%%--------------------------------------------------------------------
lower_char(Char) when Char >= $A, Char =< $Z ->
    Char + 32 ;
lower_char(OtherChar) ->
    OtherChar.

%%----------------------------------------------------------------------
%% Function: lower_string/1
%% Purpose:  Convert a string to lower case.
%% It seems there isn't a lower/upper function in stdlib, so here it is
%% Args:     String is an erlang string.
%% Returns:  A string
%%----------------------------------------------------------------------
lower_string(String) ->
    lower_string(String, []).
lower_string([], Acc) ->
    lists:reverse(Acc);
lower_string([H|T], Acc) when H >= $A, H =< $Z ->
    LowerChar = H + 32,
    lower_string(T, [LowerChar|Acc]);
lower_string([H|T], Acc) ->
    lower_string(T, [H|Acc]).

%%----------------------------------------------------------------------
%% Function: upper_string/1
%% Purpose:  Convert a string to upper case.
%% Args:     String is an erlang string.
%% Returns:  A string
%%----------------------------------------------------------------------
upper_string(String) ->
    upper_string(String, []).
upper_string([], Acc) ->
    lists:reverse(Acc);
upper_string([H|T], Acc) when H >= $a, H =< $z ->
    UpperChar = H - 32,
    upper_string(T, [UpperChar|Acc]);
upper_string([H|T], Acc) ->
    upper_string(T, [H|Acc]).

%%--------------------------------------------------------------------
%% date_to_integer/1
%% Converts a date to an Integer
%%--------------------------------------------------------------------
date_to_integer({}) ->
    0;
date_to_integer({Date,Time}) ->
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    Second + (Minute * 100) + (Hour * 10000) +
	(Day * 1000000) + (Month * 100000000) + (Year * 10000000000).

%%--------------------------------------------------------------------
%% date_to_string/2
%% Converts a date into an English string
%%--------------------------------------------------------------------
date_to_string(_Language, {}) ->
    "";
date_to_string(en, {Date, Time}) ->
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    integer_to_list(Year) ++ "-" ++
	integer_to_list(Month) ++ "-" ++
	integer_to_list(Day) ++ " " ++
	integer_to_list(Hour) ++ ":" ++
	integer_to_list(Minute) ++ ":" ++
	integer_to_list(Second).

%%--------------------------------------------------------------------
%% last_event/1
%% TODO: Rewrite it to make it more generic and pass it to misc_tools.
%% Return the last event from a given list of events:
%% Events are of the form:
%%      {eventname, Date}
%% Return the eventname of the latest event.
%%--------------------------------------------------------------------
last_event(Events) ->
    last_event(Events, {none, {{0,0,0}, {0,0,0}}}).
last_event([], LastEvent)->
    LastEvent;
last_event([Event|Events], LastEvent) ->
    {EventName, Date} = Event,
    {LastEventName, LastDate} = LastEvent,

    DateInt = date_to_integer(Date),
    LastDateInt = date_to_integer(LastDate),

    case DateInt > LastDateInt of
	true ->
	    last_event(Events, Event);
	false ->
	    last_event(Events, LastEvent)
    end.


%% A Perl-style join --- concatenates all strings in Strings,
%% separated by Sep.
join(Sep, []) ->
	[];
join(Sep, [First | List]) ->
	lists:foldl(fun(X, Sum) -> X ++ Sep ++ Sum end, First, List).


%%----------------------------------------------------------------------
%% downcase/1
%% All upper cars of the String will be down cased
%%----------------------------------------------------------------------
downcase(String) ->
    lists:map(fun(X) when $A =< X, X =< $Z -> X + $a - $A;
		 (X)                       -> X
	      end,
	      String).
		     

%%----------------------------------------------------------------------
%% is_matching_regexp/2
%% is_matching_regexp/3
%% Check the match based on a regexp expression
%% Here again you can pass in arguments the true and false values to be
%% used.
%%----------------------------------------------------------------------
is_matching_regexp(String, Regexp) ->
    is_matching_regexp(String, Regexp, {true, false}).

is_matching_regexp(String, Regexp, {True, False}) ->
    mdb_logger:debug("is_matching_regexp: ~p ~p~n", [String, Regexp]),
    case regexp:match(misc_tools:downcase(String), 
		      misc_tools:downcase(Regexp)) of
	{match, _Start, _Length} ->  True;
	nomatch                  ->  False;
	{error, Error}           ->  False
    end.
