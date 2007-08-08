%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Channel collection supervisor
%% @end
%%%-------------------------------------------------------------------
-module(irc_channel_sup).

-behaviour(supervisor).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").
-include_lib("irc.hrl").


%% API
-export([start_link/0,
         start/0,
         start_chan/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start() ->
    {ok, Pid} = start_link(),
    unlink(Pid),
    {ok, Pid}.

start_chan(Net, C = #chan{name=Chan}) ->
    case supervisor:start_child(?SERVER, [Net, C]) of
        {ok, Pid} -> {ok, Pid};
        {error, already_started} ->
            {value, {_Id,Pid,_Type,_Modules}} =
                lists:keysearch({irc_channel, Net, Chan}, 1,
                                supervisor:which_children(?SERVER)),
            {ok, Pid}
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Chan = {"AChannel",
              {irc_channel,start_link,[]},
              permanent,2000,worker,
              [irc_channel]},
    {ok,{{simple_one_for_one,0,1}, [Chan]}}.

%%====================================================================
%% Internal functions
%%====================================================================
