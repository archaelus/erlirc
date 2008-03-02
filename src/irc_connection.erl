%%%-------------------------------------------------------------------
%%% File    : irc_connection.erl
%%% Author  : Geoff Cant <nem@erlang.geek.nz>
%%% Description : 
%%%
%%% Created : 25 Mar 2006 by Geoff Cant <nem@erlang.geek.nz>
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Low level irc connection handling - wraps a tcp socket and
%% translates raw irc messages to and from irc_commands
%% @end
%%%-------------------------------------------------------------------
-module(irc_connection).

-include_lib("logging.hrl").
-include_lib("irc.hrl").

-behaviour(gen_server).

%% API
-export([start_link/4,
         start_link/3,
         start_link/2,
         sock_start/3,
         send_line/2,
         send_cmd/2,
         close/1,
         connect/2,
         peername/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conf, sock, owner, connector}).
-record(conf, {host,
               port,
               timeout = 30 * 1000,
               sendfn,
               parsefn}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Port) ->
    start_link(Host, Port, []).
start_link(Host, Port, Options) ->
    start_link(Host, Port, self(), Options).
start_link(Host, Port, Owner, Options) ->
    C = #conf{host=Host,
              port=Port,
              timeout=proplists:get_value(timeout,Options, 30*1000),
              sendfn=proplists:get_value(sendfn, Options, fun default_sendfn/2),
              parsefn=proplists:get_value(parsefn, Options, fun irc_messages:parse_line/1)
             },
    gen_server:start_link(?MODULE,
                          [{connect, C, Owner}],
                          []).


sock_start(Owner, Socket, Options) ->
    C = #conf{timeout=proplists:get_value(timeout, Options, 30*1000),
              sendfn=proplists:get_value(sendfn, Options, fun default_sendfn/2),
              parsefn=proplists:get_value(parsefn, Options, fun irc_messages:parse_line/1)
             },
    gen_server:start(?MODULE,
                     [{sock, C, Socket, Owner}],
                     []).

send_line(Con, Line) ->
    gen_server:cast(Con, {send_line, Line}).

send_cmd(Con, Cmd = #irc_cmd{}) ->
    Line = irc_messages:to_list(Cmd),
    send_line(Con, Line).

close(Con) ->
    gen_server:call(Con, close).

connect(Server, Socket) ->
    ok = gen_tcp:controlling_process(Socket, Server),
    gen_server:call(Server, {connected, Socket}).

peername(Server) ->
    gen_server:call(Server, peername).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([{connect, Conf, Owner}]) ->
    {ok, #state{conf=Conf,
                owner=Owner,
                connector=connector(Conf)}};
init([{sock, Conf, Sock, Owner}]) ->
    {ok, #state{owner=Owner,
                sock=Sock, % unnecessary
                conf=Conf}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(peername, _From, State = #state{sock=Sock}) ->
    {reply, inet:peername(Sock), State};
handle_call({connected, Socket}, _From, State) ->
    inet:setopts(Socket, [{active, true}]),
    o_send(State, connected),
    {reply, ok, State#state{sock=Socket}};
handle_call({send_line, Line}, _From, State = #state{sock=Sock}) ->
    case gen_tcp:send(Sock, Line) of
        ok -> {reply, ok, State};
        {error, Reason} -> 
            {stop, {error, {line_send, Line, Reason}}, {error, Reason}, State}
    end;
handle_call(close, _From, State) ->
    {stop, normal, State};
handle_call(Call, _From, State) ->
    ?WARN("Unhandled call ~p", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send_cmd, Cmd = #irc_cmd{}}, State) ->
    Line = irc_messages:to_list(Cmd),
    handle_cast({send_line, Line}, State);
handle_cast({send_line, Line}, State = #state{sock=Sock}) ->
    case gen_tcp:send(Sock, Line) of
        ok -> {noreply, State};
        {error, Reason} -> 
            {stop, {error, {line_send, Line, Reason}}, {error, Reason}, State}
    end;
handle_cast(Cast, State) ->
    ?WARN("Unhandled cast ~p", [Cast]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, _Port, Line}, State) ->
    try parse(State, Line) of
        Term -> o_send(State, Term)
    catch
        throw:Reason -> ?ERR("Parser crashed ~nLine: ~s~nLine: ~p~nReason: ~p~n", [Line, Line, Reason]);
        exit:Reason -> ?ERR("Parser crashed ~nLine: ~s~nLine: ~p~nReason: ~p~n", [Line, Line, Reason]);
        error:Reason -> ?ERR("Parser crashed ~nLine: ~s~nLine: ~p~nReason: ~p~n", [Line, Line, Reason])
    end,
    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    o_send(State, {disconnected, tcp_closed}),
    {stop, normal, State};
handle_info(Info, State) ->
    ?WARN("Unknown message: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State = #state{sock=S}) when S =/= undefined ->
    gen_tcp:close(State#state.sock),
    terminate(Reason, State#state{sock=undefined});
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    ?ERR("Exiting because ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

connect_to(#conf{host=Host,port=Port,timeout=Timeout}) ->
    connect_to(Host, Port, Timeout).

connect_to(Host, Port, Timeout) ->
    gen_tcp:connect(Host, Port,
                    [{active, false},
                     {packet, line},
                     {packet_size, 512},
                     {keepalive, true}],
                    Timeout).

parse(#state{conf=Conf}, Line) ->
    parse(Conf, Line);
parse(#conf{parsefn=F}, Line) ->
    F(Line).

connector(Conf) ->
    Owner = self(),
    proc_lib:spawn_link(fun () ->
                                {ok, Sock} = connect_to(Conf),
                                ok = connect(Owner, Sock),
                                unlink(Owner),
                                ok
                        end).
                                
default_sendfn(Owner, Term) ->
    Owner ! {irc, self(), Term}.

o_send(#state{owner=O, conf=#conf{sendfn=F}}, Term) ->
    F(O, Term).
