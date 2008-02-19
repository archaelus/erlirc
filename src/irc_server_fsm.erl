%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Server->Client connection state machine.
%% @end
%%%-------------------------------------------------------------------
-module(irc_server_fsm).

-behaviour(gen_fsm).

-include_lib("logging.hrl").
-include_lib("irc.hrl").

%% API
-export([start/3]).

-export([connecting/2,
	 login_pass/2,
	 login_nick/2,
	 login_user/2,
	 connected/2,
         closing/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {con,
                user = #user{},
                irc_server = #irc_server{},
                pass,
                server,
                server_mod}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start(Server, Socket, Options) when (is_port(Socket) or is_pid(Socket)),
				    is_list(Options), is_pid(Server) ->
    {ok, Fsm} = gen_fsm:start(?MODULE, [{server, Server},{socket, true}|Options], []),
    SendFn = fun (Pid, Term) -> 
                     gen_fsm:send_event(Pid, {irc, self(), Term})
             end,
    {ok, Pid} = irc_connection:sock_start(Fsm, Socket,
                                          [{sendfn, SendFn}|Options]),
    ok = irc_connection:connect(Pid, Socket),
    {ok, Fsm}.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init(Options) ->
    Server = proplists:get_value(server, Options),
    Mod = proplists:get_value(mod, Options, gen_irc_server),
    IrcServer = proplists:get_value(irc_server, Options),
    init(Mod, Server, proplists:get_value(socket, Options, false),
         #state{irc_server=IrcServer}).

init(Mod, Server, true, State) ->
    {ok, connecting, State#state{server=Server, server_mod=Mod}}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
connecting({irc, IrcConnection, connected}, State = #state{user=User}) ->
    erlang:link(IrcConnection),
    {ok, {Address, _Port}} = irc_connection:peername(IrcConnection),
    {next_state, login_pass, State#state{con=IrcConnection,user=User#user{host=Address}}};
connecting(Msg, State) ->
    {stop, {unexpected, Msg}, State}.

login_pass({irc, _, #irc_cmd{name=pass, args=Args}}, State) ->
    Pass = proplists:get_value(password, Args),
    {next_state, login_nick, State#state{pass=Pass}};
login_pass(E = {irc, _, _}, State) ->
    login_nick(E, State).

login_nick({irc, _, #irc_cmd{name=nick, args=Args}},
           S = #state{server_mod=M, user=U}) ->
    Name = proplists:get_value(name, Args),
    case M:nick(S#state.server, Name, S#state.pass) of
	{ok, GprocUserName} ->
            true = gproc:reg(GprocUserName, self()),
	    {next_state, login_user, S#state{user=U#user{nick=Name}}};
	{error, Numeric, Reason} ->
	    numreply(S, Numeric, Reason),
            {next_state, login_nick, S}
    end;
login_nick({irc, _, _}, State) ->
    csend(State, err_nonicknamegiven),
    {next_state, login_nick, State}.

login_user({irc, _, #irc_cmd{name=user,args=Args}},
           State = #state{user=U}) ->
    UserName = proplists:get_value(user_name, Args),
    RealName = proplists:get_value(real_name, Args),
    User = U#user{name=UserName,
                  realname=RealName},
    ?INFO("~p logged in.", [User]),
    welcome(State#state{user=User});
login_user(Cmd = {irc, _, _}, State) ->
    ?INFO("Got ~p in state login_user", [Cmd]),
    csend(State, notregistered),
    {next_state, login_user, State}.

welcome(State) ->
    serversend(State, #irc_cmd{name=welcome}),
    serversend(State, #irc_cmd{name=yourhost}), % XXX probably need to ask parent server our version
    serversend(State, #irc_cmd{name=created}), % XXX probably need to ask parent server when we were created
    serversend(State, #irc_cmd{name=myinfo}), % XXX probably need to ask parent server what we support
    %serversend(State, #irc_cmd{name=isupport}), % XXX probably need to ask parent server what we support
    {next_state, connected, State}.

connected({irc, _, C = #irc_cmd{name=quit}}, State) ->
    serversend(State, C#irc_cmd{name=error}),
    {stop, normal, State};
connected({irc, _, #irc_cmd{name=ping}}, State = #state{irc_server=S}) ->
    serversend(State, #irc_cmd{name=pong, args=[{servers, {S#irc_server.host, S#irc_server.net}}]}),
    {next_state, connected, State};
connected({irc, _, #irc_cmd{name=Cmd}} = Event, State) ->
    ?INFO("Got ~p in state connected.", [Event]),
    serversend(State, #irc_cmd{name=unknowncommand,
                               args=[{command, Cmd}, {message, "Ye cannot do that here."}]}),
    {next_state, connected, State}.

closing({irc, _Con, _Cmd}, State) ->
    {next_state, closing, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
%state_name(_Event, _From, State) ->
%    Reply = ok,
%    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
%handle_info({'DOWN',_Ref,process,Pid,Reason}, _StateName, S = #state{con=Pid}) when is_pid(Pid) ->
%    ?INFO("Should really shutdown properly here.", []),
%    {stop, Reason, S#state{con=undefined}};
handle_info(Info, StateName, State) ->
    ?INFO("Unexpected info (state ~p): ~p", [Info, StateName]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, StateName, S = #state{con=C}) when C =/= undefined ->
    ?INFO("Closing irc_connection ~p (state ~p, reason ~p)", [C, StateName, Reason]),
    erlang:unlink(C),
    irc_connection:close(C),
    ok;
terminate(Reason, StateName, State) ->
    ?INFO("Shutting down (statename ~p) - ~p~nServer state:~p", [StateName, Reason, State]),
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

numreply(Where, Numeric, Message) when is_atom(Numeric) ->
    csend(Where, #irc_cmd{name=Numeric, args=[{message, Message}]}).

serversend(#state{con=C,user=U,irc_server=S}, #irc_cmd{} = Cmd) ->
    csend(C, Cmd#irc_cmd{source=S,target=U}).

csend(#state{con=C}, Term) ->
    csend(C, Term);
csend(Pid, Cmd) when is_pid(Pid), is_record(Cmd, irc_cmd) ->
    irc_connection:send_cmd(Pid, Cmd).
