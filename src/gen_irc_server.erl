%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Generic IRC Server behaviour
%% @end
%%%-------------------------------------------------------------------
-module(gen_irc_server).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").
-include_lib("irc.hrl").
-include_lib("kernel/include/inet.hrl").

%% API
-export([start_link/3, start/3, listen/2, listen/3, new_client/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).


-record(state, {users, channels, listeners,
                mod, mod_state, servername}).

-define(SERVER, ?MODULE).

behaviour_info(callbacks) ->
    [{handle_call,3},
     {handle_cast,2},
     {handle_info,2},
     {init,1},
     {terminate,2},
     {code_change,3},
     % My new callbacks
     {handle_nick,3}];
behaviour_info(_) ->
    undefined.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------

start_link(Module, ServerName, Args) ->
    gen_server:start_link(?MODULE, [{Module, ServerName, Args}], []).

start(Module, ServerName, Args) ->
    gen_server:start(?MODULE, [{Module, ServerName, Args}], []).

listen(Server, Port) ->
    listen(Server, {0,0,0,0}, Port).

listen(Server, Addr, Port)  ->
    gen_server:call(Server, {listen, Addr, Port}).

new_client(Server, Socket, ServerName) ->
    {ok, {Addr, Port}} = inet:peername(Socket),
    {ok, Pid} = irc_server_fsm:start(Server, Socket, [{servername, ServerName}]),
    case inet:gethostbyaddr(Addr) of
        {ok, #hostent{h_name=L}} when is_list(L) ->
            ?INFO("New IRC client ~s:~p (fsm ~p)", [L, Port, Pid]);
        _ ->
            ?INFO("New IRC client ~p:~p (fsm ~p)", [Addr, Port, Pid])
    end,
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([{Mod, ServerName, Args}]) ->
    init(Mod, ServerName, Mod:init([{servername, ServerName}|Args])).

init(Mod, ServerName, {ok, MS, Timeout}) ->
    {ok, #state{users=[], channels=[], listeners=[],
		servername=ServerName,
                mod=Mod, mod_state=MS}, Timeout};
init(Mod, ServerName, {ok, MS}) ->
    {ok, #state{users=[], channels=[], listeners=[],
		servername=ServerName,
                mod=Mod, mod_state=MS}};
init(_Mod, _ServerName, Other) ->
    Other.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({listen, Addr, Port}, _From, State = #state{listeners=L, servername=ServerName}) ->
    case gen_tcp_server:listen(Addr,
			       Port,
			       [{client_handler,
				 fun (Server, Sock) -> new_client(Server, Sock, ServerName) end},
				{reuseaddr, true},
				{packet, line}]) of
	{ok, Server} ->
	    {reply, {ok, Server}, State#state{listeners=[Server|L]}};
	Else ->
	    {reply, Else, State}
    end;
handle_call({validate_nick, Nick, Pass}, _From, State = #state{mod=M, mod_state=MS}) ->
    handle_call_result(M:handle_nick(Nick, Pass, MS), State);
handle_call(Call, From, State = #state{mod=M, mod_state=MS}) ->
    handle_call_result(M:handle_call(Call, From, MS), State).

handle_call_result({reply, Reply, MS}, State) ->
    {reply, Reply, State#state{mod_state=MS}};
handle_call_result({reply, Reply, MS, Timeout}, State) ->
    {reply, Reply, State#state{mod_state=MS}, Timeout};
handle_call_result(Result, State) ->
    handle_common_result(Result, State).

%%--------------------------------------------------------------------
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State = #state{mod=M, mod_state=MS}) ->
    handle_common_result(M:handle_cast(Msg, MS), State).

%%--------------------------------------------------------------------
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State = #state{mod=M, mod_state=MS}) ->
    handle_common_result(M:handle_info(Info, MS), State).

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{mod=M, mod_state=MS}) ->
    M:terminate(Reason, MS).

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle_common_result({noreply, MS}, State) ->
    {noreply, State#state{mod_state=MS}};
handle_common_result({noreply, MS, Timeout}, State) ->
    {noreply, State#state{mod_state=MS}, Timeout};
handle_common_result({stop, Reason, MS}, State) ->
    {stop, Reason, State#state{mod_state=MS}};
handle_common_result({stop, Reason, Reply, MS}, State) ->
    {stop, Reason, Reply, State#state{mod_state=MS}}.
