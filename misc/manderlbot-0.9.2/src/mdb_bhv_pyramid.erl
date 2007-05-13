%%%-------------------------------------------------------------------
%%% File    : mdb_bhv_pyramid.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Description : Implementation of french TV game « Pyramide »
%%%
%%% Created :  7 Nov 2002 by Dimitri Fontaine <dfontaine@mail.cvf.fr>
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

-module(mdb_bhv_pyramid).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("mdb.hrl").
-include("log.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, setWord/3, start/4, guess/3, behaviour/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(timeout, 5000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

setWord(Nick, Channel, Word) ->
    gen_server:call(?MODULE, {setWord, Nick, Channel, Word}, ?timeout).

start(Nick, Channel, Player2, Nguess) ->
    gen_server:call(?MODULE,
		    {start, Nick, Channel, Player2, Nguess}, ?timeout).

guess(Nick, Channel, Word) ->
    gen_server:call(?MODULE, {guess, Nick, Channel, Word}, ?timeout).

%%%----------------------------------------------------------------------
%%% Function: pyramid/5
%%% Purpose:  implements a pyramid game, see comments
%%%----------------------------------------------------------------------
behaviour(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    %%  - first player giving the bot the answer, before beginning the game,
    %%    in private dialog
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    [Header, Word] =  string:tokens(Input#data.body, ": "),

    case setWord(NickFrom, Channel,
			 misc_tools:downcase(string:strip(Word))) of
	{ok, Message} ->
	    mdb_bot:say(BotPid, Message, NickFrom),
	    mdb_bot:say(BotPid, NickFrom ++ " has set a word to guess !");

	{error, Reason} ->
	    mdb_bot:say(BotPid, Reason, NickFrom)
    end;

behaviour(Input, BotName, Data, BotPid, Channel) ->
    %% For this game, we have to detect some different cases on the
    %% same behaviour, that is :
    %%
    %%  - beginning of game, first player inviting second and giving the
    %%    number of tries
    %%
    %%  - second player guess

    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    mdb_logger:info("body: ~p~n", [Input#data.body]),

    case regexp:match(Input#data.body, "[A-Za-z0-9_]+/[0-9]+") of
	{match, S, L} ->
	    [Player2, Nguess] =
		string:tokens(string:substr(Input#data.body, S, L), "/"),

	    {ok, [{integer, 1, Iguess}],1} = erl_scan:string(Nguess),
	    
	    mdb_logger:info("pyramid: ~p invite ~p in ~p~n",
			    [NickFrom, Player2, Iguess]),

	    {_ok, SMsg} = start(NickFrom, Channel, Player2, Iguess),
	    mdb_bot:say(BotPid, SMsg);

	_Whatever  ->
	    %% That is a guess
	    [Header, Word] =  string:tokens(Input#data.body, ": "),
	    {_State, GMsg}  =
		guess(NickFrom, Channel,
			      misc_tools:downcase(string:strip(Word))),

	    mdb_bot:say(BotPid, GMsg)
    end.

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({setWord, Nick, Channel, Word}, From, State) ->
    %% We receive the word to guess in private. The game has to be started
    %% to be able to set the word.
    case lists:keysearch({Nick, Channel}, 1, State) of
	{value, {{Player1, Channel}, noword, Player2, Nguess, Ntries}} ->
	    {reply, {ok, Word ++ " has been set"}, 
	     lists:keyreplace({Player1, Channel}, 1, State,
			      {{Player1, Channel},
			       Word, Player2, Nguess, Ntries})
	    };

	{value, {{Player1, Channel}, GWord, Player2, Nguess, Ntries}} ->
	    {reply,
	     {error, "word to guess has already been set to: " ++ GWord},
	     State};

	false ->
	    {reply, {error, "No game started"}, State}
    end;

handle_call({start, Nick, Channel, Player2, Nguess}, From, State) ->
    %% The game will start once the word to guess will be given in private
    %% We juste prepare and tell the players
    %% Attention: one game at a time !
   
    case lists:keysearch({Nick, Channel}, 1, State) of
	{value, _} ->
	    %% Game already started
	    {reply, {error, "Game already started"}, State};

	false ->
	    %% Check the second player is not engaged
	    case lists:keysearch(Player2, 3, State) of
		{value, _} ->
		    {reply, {error, Player2 ++ " is already playing."}, State};

		false -> 
		    %% We can start a new game
		    {reply,
		     {ok,
		      Nick ++ ": please give me the word to guess (pv)"},
		     [{{Nick, Channel}, noword, Player2, Nguess, 1}|State]}
	    end
    end;

handle_call({guess, Nick, Channel, Word}, From, State) ->
    %% The second player is trying to guess the word
    case lists:keysearch(Nick, 3, State) of
	{value, {{Player1, Channel}, noword, Player2, Nguess, Ntries}} ->
	    {reply, {ko, Player2 ++ ": please wait for " ++
		     Player1 ++ " to give a word to guess"}, State};
	    
	{value, {{Player1, Channel}, Word, Player2, Nguess, Ntries}} ->
	    {reply, {ok, Player2 ++ " won in " ++ [48+Ntries] ++ " tries !"},
	     lists:keydelete({Player1, Channel}, 1, State)};
	 
	{value, {{Player1, Channel}, GWord, Player2, Nguess, Nguess}} ->
	    {reply, {ko, Player2 ++ " failed to guess the word: " ++ GWord},
	     lists:keydelete({Player1, Channel}, 1, State)};

	{value, {{Player1, Channel}, GWord, Player2, Nguess, Ntries}} ->
	    %% Don't forget to increment the Ntries
	    {reply, {ko, Player2 ++ ": try again ! "},
	     lists:keyreplace({Player1, Channel}, 1, State,
			     {{Player1, Channel},
			      GWord, Player2, Nguess, Ntries + 1})};
	 
	false ->
	    {reply, {ko, "No game started"}, State}
    end;

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
