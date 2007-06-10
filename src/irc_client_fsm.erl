%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_client_fsm).

-behaviour(gen_fsm).

-include_lib("logging.hrl").
-include_lib("irc.hrl").
-include_lib("eunit.hrl").


%% API
-export([start_link/2,
         start_link/6,
         shutdown/1,
         send/2,
         reset_to_connected/1]).

-export([connecting/2,
         wait_connected/2,
         welcome/2,
         connected/2,
         joining_channel/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {nick,
                host,
                port,
                conf,
                con,
                channels
               }).

-record(conf, {host,
               port,
               options,
               username,
               realname}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Nick, Host) ->
    start_link(Nick, Nick, Nick, Host, 6667, [{reconnect, 60}]).

start_link(Nick, Username, Realname, Host, Port, Options) ->
    gen_fsm:start_link(?MODULE, [#conf{host=Host,
                                       port=Port,
                                       username=Username,
                                       realname=Realname,
                                       options=Options}, Nick],
                       []).

shutdown(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, shutdown).

send(Pid, C = #irc_cmd{}) ->
    gen_fsm:send_all_state_event(Pid, {to_irc, C}).

reset_to_connected(Pid) ->
    gen_fsm:send_all_state_event(Pid, reset_to_connected).

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
init([Conf = #conf{}, Nick]) ->
    {ok, connecting,
     #state{conf=Conf, nick=Nick,
            channels=dict:new()},
     0}.

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
connecting(timeout, State = #state{conf=Conf}) ->
    {ok, Pid} = irc_connection:start_link(Conf#conf.host,
                                          Conf#conf.port,
                                          [{sendfn, fun (Pid, Term) -> 
                                                            gen_fsm:send_event(Pid, {irc, self(), Term})
                                                    end}]),
    {next_state, wait_connected, State#state{con=Pid}}.

wait_connected({irc, Pid, connected}, State = #state{conf=Conf}) ->
    irc_connection:send_cmd(Pid, #irc_cmd{name=nick,
                                          args=[{name, State#state.nick}]}),
    irc_connection:send_cmd(Pid, #irc_cmd{name=user,
                                          args=[{user_name, Conf#conf.username},
                                                {real_name, Conf#conf.realname}]}),
    {next_state, welcome, State}.

welcome({irc, Pid, #irc_cmd{source=S, name=privmsg, ctcp=[#ctcp_cmd{name=version}]}}, State) ->
    CtcpReply = #ctcp_cmd{name=version,
                          args=[{client, atom_to_list(?MODULE)},
                                {version, "0.0.1"},
                                {environment, "erlang"}]},
    irc_connection:send_cmd(Pid, #irc_cmd{name=notice,
                                          target=S#user.nick,
                                          args=[{message, ""}],
                                          ctcp=[CtcpReply]}),
    {next_state, welcome, State};
welcome({irc, Pid, #irc_cmd{name=ping, args=[{token, T}]}}, State) ->
    irc_connection:send_cmd(Pid, #irc_cmd{name=pong,
                                          args=[{token, T}]}),
    {next_state, welcome, State};
welcome({irc, _Pid, C = #irc_cmd{name=welcome, args=A}}, State) ->
    ?INFO("~s", [C#irc_cmd.raw]),
    {next_state, welcome,
     State#state{nick=proplists:get_value(target, A, State#state.nick)}};
welcome({irc, _Pid, C = #irc_cmd{name=MOTD}}, State) when MOTD == endofmotd; MOTD == nomotd ->
    ?INFO("~s", [C#irc_cmd.raw]),
    ?INFO("Now connected.", []),
    {next_state, connected, State};
welcome({irc, _Pid, C = #irc_cmd{}}, State) ->
    ?INFO("~s", [C#irc_cmd.raw]),
    {next_state, welcome, State}.


connected({irc, Pid, C = #irc_cmd{source=S, name=privmsg,
                                  target=T,
                                  args=[{message, M}],
                                  ctcp=[#ctcp_cmd{name=version}]}}, State) ->
    CtcpReply = #ctcp_cmd{name=version,
                          args=[{client, atom_to_list(?MODULE)},
                                {version, "0.0.1"},
                                {environment, "erlang"}]},
    ?INFO("CTCP requests: ~p~nCTCP reply: ~p", [C#irc_cmd.ctcp, CtcpReply]),
    ?INFO("~s!~~~s@~s -> ~s: ~s", [S#user.nick,S#user.user,S#user.host,T,M]),
    irc_connection:send_cmd(Pid, #irc_cmd{name=notice,
                                          target=S#user.nick,
                                          args=[{message, ""}],
                                          ctcp=[CtcpReply]}),
    {next_state, connected, State};
connected({irc, _Pid, C = #irc_cmd{name=privmsg, target=T,
                                   args=[{message,M}], ctcp=undefined}}, State) ->
    U = C#irc_cmd.source,
    ?INFO("~s!~~~s@~s -> ~s: ~s", [U#user.nick,U#user.user,U#user.host,T,M]),
    {next_state, connected, State};

connected({irc, _Pid, C = #irc_cmd{name=privmsg, target=T,
                                   args=A, ctcp=Ctcp}}, State) ->
    U = C#irc_cmd.source,
    M = proplists:get_value(message, A, "<no message>"),
    ?INFO("~s!~~~s@~s -> ~s: ~s", [U#user.nick,U#user.user,U#user.host,T,M]),
    ?INFO("CTCP requests: ~p", [Ctcp]),
    {next_state, connected, State};
connected({irc, Pid, #irc_cmd{name=ping, args=[{token, T}]}}, State) ->
    irc_connection:send_cmd(Pid, #irc_cmd{name=pong,
                                          args=[{token, T}]}),
    {next_state, connected, State};
connected(E = {irc, _Pid, #irc_cmd{name=join}}, State) ->
    handle_join(E, State);
connected(E = {irc, _Pid, #irc_cmd{name=part}}, State) ->
    handle_part(E, State);
connected({irc, _Pid, C}, State) ->
    ?INFO("(~p) ~s~n~p", [C#irc_cmd.name, C#irc_cmd.raw, C]),
    {next_state, connected, State}.

handle_join({irc, _Pid, #irc_cmd{source=#user{nick=Me},
                                 args=[{channels, [Chan]}]}},
            State = #state{nick=Me,channels=Chans}) ->
    ?INFO("Joining ~s", [Chan]),
    {next_state, joining_channel,
     State#state{channels=dict:store(Chan, #chan{name=Chan}, Chans)}};

handle_join({irc, _Pid, #irc_cmd{source=#user{nick=Nick},
                                 args=[{channels, [Chan]}]}},
            State = #state{channels=Chans}) ->
    ?INFO("~s joined ~s.", [Nick, Chan]),
    ChanR = dict:fetch(Chan, Chans),
    NewChanR = ChanR#chan{members=[{user, Nick}|ChanR#chan.members]},
    {next_state, connected,
     State#state{channels=dict:store(Chan, NewChanR, Chans)}}.

handle_part({irc, _Pid, #irc_cmd{source=#user{nick=Me},
                                 args=[{channels, [Chan]}|_]}},
            State = #state{nick=Me,channels=Chans}) ->
    ?INFO("Parted ~s.", [Chan]),
    {next_state, connected,
     State#state{channels=dict:erase(Chan, Chans)}};
handle_part({irc, _Pid, #irc_cmd{source=#user{nick=Nick},
                                 args=[{channels, [Chan]},
                                       {message, Msg}]}},
            State = #state{channels=Chans}) ->
    ?INFO("~s parted ~s (~s).", [Nick, Chan, Msg]),
    ChanR = dict:fetch(Chan, Chans),
    NewChanR = ChanR#chan{members=[M ||
                                      M = {_Type, N} <- ChanR#chan.members
                                          ,N /= Nick]},
    {next_state, connectead,
     State#state{channels=dict:store(Chan, NewChanR, Chans)}}.

joining_channel({irc, _Pid, #irc_cmd{name=endofnames,args=A}},
                State) ->
    Chan = proplists:get_value(channel, A, "ERROR - No channel"),
    ?INFO("Finished joining ~s.", [Chan]),
    {next_state, connected, State};
joining_channel({irc, _Pid, C = #irc_cmd{name=namreply, args=A}},
                State = #state{channels=Chans}) ->
    Members = proplists:get_value(members, A, []),
    Type = proplists:get_value(channel_type, A, undefined),
    ChanName = proplists:get_value(channel, A, "ERROR - No channel"),
    Chan = dict:fetch(ChanName, Chans),
    NewChans = dict:store(ChanName, 
                          Chan#chan{members=lists:usort([Members|Chan#chan.members]),
                                    type=Type},
                          Chans),
    ?INFO("Joining: ~s~nMembers: ~p", [ChanName,
                                       lists:usort([Members|Chan#chan.members])]),
    {next_state, joining_channel, State#state{channels=NewChans}};
joining_channel(E = {irc, _Pid, C}, State) ->
    ?INFO("Unknown message in join state, retreating to connected state.~n~p",
          [C#irc_cmd.raw]),
    connected(E, State).

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
handle_event(reset_to_connected, _State, State) ->
    {next_state, connected, State};
handle_event({to_irc, Cmd}, StateName, State = #state{con=Pid}) when is_pid(Pid) ->
    irc_connection:send_cmd(Pid, Cmd),
    {next_state, StateName, State};
handle_event(Event, StateName, State) ->
    ?WARN("Unexpected event in state ~p: ~p", [StateName, Event]),
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
handle_sync_event(shutdown, _From, _StateName, S = #state{con=P}) when is_pid(P) ->
    unlink(P),
    irc_connection:close(P),
    {stop, normal, ok, S#state{con=undefined}};
handle_sync_event(shutdown, _From, _StateName, S) ->
    {stop, normal, ok, S};
handle_sync_event(Event, _From, StateName, State) ->
    ?WARN("Unexpected event ~p in state ~p", [Event, StateName]),
    {next_state, StateName, State}.

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
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
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
