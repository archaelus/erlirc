%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ErlIRC top level supervisor
%% @end
%%%-------------------------------------------------------------------
-module(irc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link
%% @spec (term()) -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor.
%% @end
%%--------------------------------------------------------------------
start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
    CS = {"Channel Supervisor",
          {irc_channel_sup,start_link,[]},
          permanent,2000,worker,
          [irc_channel_sup, irc_channel]},
    {ok,{{one_for_all,2,10}, [CS]}}.

%%====================================================================
%% Internal functions
%%====================================================================
