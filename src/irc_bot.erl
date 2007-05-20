%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_bot).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("irc.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/3,
         connect/1, connect/2,
         connections/0,
         disconnect/1, disconnect/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conf, connections, plugin_mgr, raw_plugin_mgr}).
-record(conf, {nick, username, realname}).
-record(coninfo, {host, port}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Nick, Username, Realname) when is_list(Nick), is_list(Username), is_list(Realname) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [#conf{nick=Nick,
                                 username=Username,
                                 realname=Realname}], []).

connect(Host) ->
    connect(Host, 6667).

connect(Host, Port) ->
    gen_server:call(?SERVER, {connect, Host, Port}).

connections() ->
    gen_server:call(?SERVER, connections).

disconnect(Host) ->
    disconnect(Host, 6667).

disconnect(Host, Port) ->
    gen_server:call(?SERVER, {disconnect, Host, Port}).

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
init([Conf]) ->
    {ok, PluginPid} = gen_event:start_link(),
    {ok, RawPid} = gen_event:start_link(),
    {ok, #state{conf=Conf,
                connections=dict:new(),
                plugin_mgr=PluginPid,
                raw_plugin_mgr=RawPid}}.

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
handle_call({disconnect, Host, Port}, _From, State = #state{connections=C}) ->
    Pids = [Pid || {Pid, [#coninfo{host=H,port=P}]} <- dict:to_list(C),
                   Host == H, P == Port],
    NewC = lists:foldl(fun (Pid, D) ->
                               unlink(Pid),
                               irc_connection:send_cmd(Pid, #cmd{name=quit}),
                               dict:erase(Pid, D)
                       end,
                       C,
                       Pids),
    {reply, {ok, Pids}, State#state{connections=NewC}};
handle_call(connections, _From, State = #state{connections=C}) ->
    {reply, {ok, dict:to_list(C)}, State};
handle_call({connect, Host, Port}, _From, State) ->
    {ok, Pid} = irc_connection:start_link(Host, Port, [{sendfn, fun client_cmd/2}]),
    {reply, {ok, Pid}, 
     State#state{connections=dict:append(Pid, #coninfo{host=Host, port=Port},
                                         State#state.connections)}};
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({client, Pid, Term}, State) ->
    handle_client_cmd(Pid, hd(dict:fetch(Pid,State#state.connections)),
                      Term, State);
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

handle_client_cmd(Pid, _Coninfo, {connected, _Pid}, State = #state{conf=Conf}) ->
    irc_connection:send_cmd(Pid, #cmd{name=nick,
                                      args=[{name, nick(Conf)}]}),
    irc_connection:send_cmd(Pid, #cmd{name=user,
                                      args=[{user_name, Conf#conf.username},
                                            {real_name, Conf#conf.realname}]}),
    {noreply, State};
handle_client_cmd(_Pid, #coninfo{host=Host,port=Port}, #cmd{name=notice,
                                                      args=A}, State) ->
    ?INFO("~s:~p [~s] ~s", [Host, Port,
                            proplists:get_value(facility, A),
                            proplists:get_value(text, A)]),
    {noreply, State};
handle_client_cmd(Pid, _cmdinfo, #cmd{name=ping,args=[{token, T}]}, S) ->
    irc_connection:send_cmd(Pid, #cmd{name=pong, args=[{token, T}]}),
    {noreply, S};


handle_client_cmd(_Pid, #coninfo{host=Host,port=Port}, UnknownCommand, State) ->
    ?INFO("~s:~p -- unknown command: ~p", [Host, Port, UnknownCommand]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

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

client_cmd(Owner, Cmd) ->
    gen_server:cast(Owner, {client, self(), Cmd}).

nick(#state{conf=Conf}) ->
    nick(Conf);
nick(#conf{nick=Nick}) ->
    Nick.
