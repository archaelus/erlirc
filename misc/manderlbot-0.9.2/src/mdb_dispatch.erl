%%%----------------------------------------------------------------------
%%% File    : mdb_dispatch.erl
%%% Author  : Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% Purpose : Library gather the process of IRC event and the execution
%%%           of "behaviours" (event handling code).
%%% Created : 16 Sep 2001, Mickaël Rémond <mickael.remond@erlang-fr.org>
%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or modify  
%%% it under the terms of the GNU General Public License as published by 
%%% the Free Software Foundation; either version 2 of the License, or   
%%% (at your option) any later version.                                
%%%
%%%----------------------------------------------------------------------
%%%
%%% See COPYING for detailled license
%%%
%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two. If you modify this file, you may extend this exception
%%% to your version of the file, but you are not obligated to do
%%% so.  If you do not wish to do so, delete this exception
%%% statement from your version.
%%%----------------------------------------------------------------------

-module(mdb_dispatch).

-author('mickael.remond@erlang-fr.org').
-created('Date: 20010916').
-revision(' $Id: mdb_dispatch.erl,v 1.17 2003/10/07 09:48:28 dim Exp $ ').
-vsn(' $Revision: 1.17 $ ').

%% External exports (API)
-export([process_data/3, treat_recv/3, append_botname/2]).

%% -- Includes --
%% Configure debugging mode:
-include("mdb_macros.hrl").

%% Include record description
-include("mdb.hrl").
-include("config.hrl").

-define(NL, "\r\n").
-define(BOTNAME, "%BOTNAME").

%%----------------------------------------------------------------------
%% process_data/3
%% Parse the incoming data into lines,
%% and spawn a treat_recv process on each one
%%----------------------------------------------------------------------
process_data(Sock, [], State=#state{}) -> 
    {ok, []};

process_data(Sock, [$P, $I, $N, $G, $ , $: | Tail], State=#state{}) -> 
    %% We consider PING separately
    {Id, Rest} = cut_line(Tail),

    irc_lib:pong(Sock, Id),
    {pong, Rest};

process_data(Sock, Data, State=#state{joined = false}) -> 
    %% When we do not have joined, we have to test for a join chan
    %% response. This can occurs when server require a pong before
    %% anything else
    %% So this code will manage the MOTD of the server

    {Line, Rest} = cut_line(Data),
    %% mdb_logger:log("MOTD ~p~n", [Line]),
    
    Chan = [$: | State#state.channel],

    case string:tokens(Line, " ") of
	[_Name, "JOIN", Chan | Tail] ->
	    mdb_logger:log(
	      "~s joined channel ~s~n", [State#state.nickname,
					 State#state.channel]),

	    %% There could be some behaviours on login
	    process_data(Sock, Line ++ ?NL, State#state{joined=true}),
	    {joined, Rest};
	_ ->
	    process_data(Sock, Rest, State)
    end;

process_data(Sock, Data, State=#state{joined = true}) -> 
    case cut_line(Data) of
	%% If we don't find ?NL in the data, then we add it in the buffer
	{Data, []}   -> {ok, Data};

	{Line, Rest} ->
	    proc_lib:spawn(?MODULE, treat_recv,
			   [Sock, list_to_binary(Line), State]),
	    process_data(Sock, Rest, State)
    end;

process_data(Sock, Data, State) ->
    ?dbg("process_data: ~p ~p ~p ~n", [Sock, Data, State]),
    {ok, []}.

cut_line(Data) ->
    Pos = string:str(Data, ?NL),
    case Pos of 
	0 -> {Data, []};

	_ ->
	    Line = string:substr( Data, 1, Pos-1 ),
	    Rest = string:substr( Data, Pos+2, string:len(Data) - (Pos-1) ),
	    
	    {Line, Rest}
    end.

%%----------------------------------------------------------------------
%% treat_recv/3
%% Otherwise:
%%----------------------------------------------------------------------
treat_recv(Sock, Data, State=#state{}) ->
    Result = binary_to_list(Data),

    %% The Parsed_Result is a list of data records.
    Parsed_result = input_to_record(Result),

    %% Get the list of behaviours that match to the current IRC line
    %% And for which the corresponding fun will be executed
    {ok, BList} = config_srv:getBehaviours(State#state.behaviours),
    %% mdb_logger:log("BList: ~p~n", [State#state.behaviours]),
    lists:map(fun(X) -> 
		      MatchingList =
			  match_event(X, BList, State#state.nickname),
		      dispatch_message(MatchingList, X, State)
		      end,
	      Parsed_result),

    %% Trace
    lists:map(fun(Res) ->
		      [NickFrom|_] = string:tokens(Res#data.header_from, "!"),
		      mdb_logger:log("~s ~s <~s> ~s~n", 
				     [State#state.nickname,
				      Res#data.header_to,
				      NickFrom,
				      Res#data.body])
	      end,
    	      Parsed_result).

%%----------------------------------------------------------------------
%% dispatch_message/3
%% We are executing the behaviour whose pattern is matching with
%% the input from the IRC server
%%----------------------------------------------------------------------
dispatch_message(Behaviours, Input, State = #state{mode=muted}) ->
    lists:map(fun(Behaviour = #behaviour{id = "mute"}) ->
		      {M, F} = Behaviour#behaviour.function,
		      apply(M, F, [Input,
				   State#state.nickname,
				   Behaviour#behaviour.data,
				   State#state.bot_pid,
				   State#state.channel]);
		 (_) ->
		      mdb_logger:log("~s MUTED", [State#state.nickname])
	      end,
	      Behaviours);

dispatch_message(Behaviours, Input, State = #state{}) ->
    lists:map(fun(Behaviour) ->
		      mdb_logger:log("Match= ~p~n",
				     [Behaviour#behaviour.function]),

		      {M, F} = Behaviour#behaviour.function,
		      apply(M, F, [Input,
				   State#state.nickname,
				   Behaviour#behaviour.data,
				   State#state.bot_pid,
				   State#state.channel])
	      end,
	      Behaviours).

%%----------------------------------------------------------------------
%% input_to_record/1
%% Convert a given input to a list of preparsed data records
%%----------------------------------------------------------------------
input_to_record(ServerData) ->
    Lines = string:tokens(ServerData, ?NL),
    parse_lines(Lines, []).

%%----------------------------------------------------------------------
%% parse_lines/2
%% Each input line will be a data record
%%----------------------------------------------------------------------
parse_lines([], Result) ->
    lists:reverse(Result);
parse_lines([Line|Lines], Result) ->
    parse_lines(Lines, [parse_line(Line) | Result]).

%%----------------------------------------------------------------------
%% parse_line/1
%% Each line is split between the data record fields
%%----------------------------------------------------------------------
parse_line([$: | ServerData]) ->
    BodyPos = string:chr(ServerData, $:),

    case BodyPos > 0 of
	true ->
	    Header  = string:substr(ServerData, 1, BodyPos - 1),
	    Body    = string:substr(ServerData, BodyPos + 1),
	    Result  = string:tokens(Header, " "),

	    [Header_from, Header_op, Header_to, Header_options] =
		case Result of
		    [From]     -> [From, ?nodata, ?nodata, ?nodata];
		    [From, Op] -> [From, Op, ?nodata, ?nodata];
		    [From, Op, To | Options] ->
			[From, Op, To, lists:flatten(Options)]
		end,
	    
	    #data{header_from    = Header_from,
		  header_op      = Header_op,
		  header_to      = Header_to,
		  header_options = Header_options,
		  body           = Body};

	false ->
	    [Header_from, Header_op, Header_to | _Rest] =
		string:tokens(ServerData, " "),

	    #data{header_from = Header_from,
		  header_op = Header_op,
		  header_to = Header_to,
		  body = ""}
    end;

%% I think that missing a ping generate a message that fall in this case and
%% crash the process
parse_line(ServerData) ->
    ?dbg("In ParseLine: unidentified: ~p", [ServerData]),
    %% Ignore.
    #data{}.


%%----------------------------------------------------------------------
%% match_event/3
%% Returns the list of behaviour that should be executed on an irc input
%%----------------------------------------------------------------------
match_event(Data, Behaviours, Nickname) ->
    match_event(data_as_list(Data), Behaviours, Nickname, []).

match_event(Data, [], Nickname, Acc) ->
    lists:reverse(Acc);

match_event(Data, [Behaviour|Behaviours], Nickname, Acc) ->
    MatchCritList =
	append_botname(data_as_list(Behaviour#behaviour.pattern), Nickname),

    ExlCritList = 
	append_botname(
	  data_as_list(Behaviour#behaviour.exclude_pattern), Nickname),

    %% We react on the behaviour only when the pattern is matched and
    %% the exclude pattern is not
    case {is_matching(Data, MatchCritList),
	  is_matching(Data, ExlCritList, exclude)} of
	{true, false} ->
	    match_event(Data, Behaviours, Nickname, [Behaviour|Acc]);

	_DontMatch ->
	    match_event(Data, Behaviours, Nickname, Acc)
    end.


%%----------------------------------------------------------------------
%% data_as_list/1
%% Convert the data record to a list of values
%%----------------------------------------------------------------------
data_as_list(Data) ->
    DataList = tuple_to_list(Data),
    [RecordName | Rest] = DataList,
    Rest.

%%----------------------------------------------------------------------
%% append_botname/2
%% Convert '%BOTNAME' wherever in the list by its real name
%%----------------------------------------------------------------------
append_botname(List, Botname) ->
    lists:map(fun(Exp = {regexp, '_'}) -> Exp;

		 ({regexp, String}) ->
		      {ok, NewString, _C} =
			  regexp:sub(String, ?BOTNAME, Botname),
		      {regexp, NewString};

		 (Other) ->
		      Other
	      end,
	      List).

%%----------------------------------------------------------------------
%% is_matching/3
%% Check if the first list (data record field values) match the 
%% Criterium
%%
%% The last parameter is the mathing mode.
%%
%%----------------------------------------------------------------------
is_matching(Data, Criterium) ->
    is_matching(Data, Criterium, true, normal).

is_matching(Data, Criterium, exclude) ->
    %% Weh excluding, we fail the test by default
    is_matching(Data, Criterium, false, exclude).

is_matching(_Data, _Criterium, Result = false, normal) ->
    %% We cut the tests when in normal mode and found false result
    Result;

is_matching(_Data, _Criterium, Result = true, exclude) ->
    %% We cut the tests when in exclude mode and found true result
    Result;

is_matching([],[], Result, Mode) ->
    Result;

is_matching([E|Elements], [C|Criteria], Result, Mode) ->
    %% mdb_logger:log("is_matching: ~p ~p~n", [C, E]),
    %%
    %% When we have no criterium, in exclude mode, we consider
    %% the match has failed.
    NoCrit = case Mode of
		 normal -> true;
		 exclude -> false
	     end,
		     
    case C of
	?nodata ->
	    is_matching(Elements, Criteria, lop(NoCrit, Result, Mode), Mode);

	{regexp, ?nodata} ->
	    is_matching(Elements, Criteria, lop(NoCrit, Result, Mode), Mode);

	{regexp, Expr} ->
	    is_matching(Elements, Criteria,
			misc_tools:is_matching_regexp(E, Expr), Mode);

	%% Should tag the Criterium value as {exact, Criterium}	    
	E ->
	    is_matching(Elements, Criteria, lop(true, Result, Mode), Mode);

	_Other ->
	    is_matching(Elements, Criteria, lop(false, Result, Mode), Mode)
    end.

%% Logic Operator
%% When excluding, keep current state if there is no criterium
lop(false, true, exclude) -> true;
lop(Bool, State, Mode)    -> Bool.
