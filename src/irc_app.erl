%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc IRC OTP application
%% @end
%%%-------------------------------------------------------------------
-module(irc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,
         start/0, stop/0,
         config/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case irc_sup:start_link(StartArgs) of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%% @spec start() -> ok
%% @doc Start the erlirc server.
start() ->
    application:load(erlirc),
    {ok, Deps} = application:get_key(erlirc, applications),
    true = lists:all(fun ensure_started/1, Deps),
    application:start(erlirc).

%% @spec stop() -> ok
%% @doc Stop the erlirc server.
stop() ->
    application:stop(erlirc).

%% @spec config(Item::atom()) -> term()
%% @doc Retrieve the configuration value for key Item from the erlirc
%% OTP application environment.
config(Item) ->
    case application:get_env(erlirc, Item) of
        {ok, Term} -> Term;
        undefined ->
            error_logger:error_msg("erlirc not correctly configured: missing ~p",
                                   [Item]),
            exit(erlirc_misconfigured)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    true;
	{error, {already_started, App}} ->
	    true;
        Else ->
            error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
            Else
    end.
