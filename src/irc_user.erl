%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_user).

-include_lib("irc.hrl").

%% API
-export([gproc_name/1,
         gproc_name/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () ->
%% @doc 
%% @end 
gproc_name(#user{net=Net,nick=Nick}) when is_list(Net), is_list(Nick) ->
    gproc:name({irc_user, Net, Nick}).

gproc_name(Net, Nick) when is_list(Net), is_list(Nick) ->
    gproc:name({irc_user, Net, Nick}).

%%====================================================================
%% Internal functions
%%====================================================================
