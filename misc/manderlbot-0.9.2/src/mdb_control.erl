%%%----------------------------------------------------------------------
%%% File    : mdb_control.erl
%%% Author  : Dimitri Fontaine <dim@mail.cvf.fr>
%%% Purpose : Control manderlbot, rpc to the running node
%%% Created : 26 Aug 2003 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-module(mdb_control).
-author('dim@tuxfamily.org').

-export([stop/0, status/0]).

-include("config.hrl").
-include("log.hrl").
-define(mdb_node, "manderlbot").

%%
%% The main control function
%%

stop() ->
    rpc:call(getNode(), application, stop, [manderlbot]),
    rpc:call(getNode(), init, stop, []),
    init:stop().

status() ->
    {ok, List} = rpc:call(getNode(), mdb_botlist, list, []),
    
    lists:map(fun({Host, Chan = #channel{}}) ->
		      mdb_logger:notice("~s connected on ~s ~s~n", 
					[Chan#channel.botname,
					 Host,
					 Chan#channel.name])
	      end, List),
    init:stop().


%%
%% Some util functions
%%
getNode() ->
    [Node, Host] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom(?mdb_node ++ "@" ++ Host).
