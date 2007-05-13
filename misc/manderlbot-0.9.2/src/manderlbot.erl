%%%----------------------------------------------------------------------
%%% File    : manderlbot.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : This app is an IRC bot
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

-module(manderlbot).
-author('dim@tuxfamily.org').

-include("config.hrl").
-include("log.hrl").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start(Type, StartArgs) ->
    %% First read args
    {Config_file, Log_file, Log_level} = read_args(),

    case manderlbot_sup:start_link(Config_file, Log_file) of
	{ok, Pid} -> 
	    %% init the manderlbot system
	    case init(Config_file, Log_file, Log_level) of
		ok              -> {ok, Pid};
		{error, Reason} -> {error, Reason}
	    end;
	Error ->
	    Error
    end.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%----------------------------------------------------------------------
stop(State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
init(Config_file, Log_file, Log_level) ->
    %% Init first the logger
    mdb_logger:add_handler({Log_file, Log_level}),
    mdb_logger:notice("Manderlbot starting with ~s~n", [Config_file]),

    %% Read the config and start the bots
    case config_srv:readConf() of
	ok -> ok;
	{error, Reason} ->
	    mdb_logger:error("Could not init manderlbot: ~p~n", [Reason]),
	    {error, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: read_args/0
%% Returns: {Config_file, Log_file}
%%----------------------------------------------------------------------
read_args() ->
    %% We take the manderlbot application defaults
    {ok, ConfigFile} = application:get_env(manderlbot, config_file),
    {ok, LogFile}  = application:get_env(manderlbot, log_file),
    {ok, LogLevel} = application:get_env(manderlbot, log_level),

    {ConfigFile, LogFile, LogLevel}.

