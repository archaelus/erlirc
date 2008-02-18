%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc A wrapper library to give gen_tcp a more Erlangy interface for
%% listen/accept.
%% @end
%%%-------------------------------------------------------------------
-module(gen_tcp_server).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([listen/2
         ,listen/3
         ,controlling_process/2
         ,close/1
         ,close/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3
         ,acceptor/2
        ]).

-record(state, {listen_socket,
                acceptor,
                controlling_process,
                cp_monitor,
                host,
                port,
                options}).
-define(SERVER, ?MODULE).
-define(LOCALHOST, {127,0,0,1}).
-define(ALLINTERFACES, {0,0,0,0}).

%%====================================================================
%% API
%%====================================================================

%% @spec listen(Port::integer, OptionsProplist::list()) -> {ok,Pid} | ignore | {error,Error}
%% @see listen/3
listen(Port, Options) ->
    listen(?ALLINTERFACES, Port, Options).

%% @spec listen(Host, Port::integer, OptionsProplist) -> {ok,Pid} | ignore | {error,Error}
%%    Host = {int(),int(),int(),int()} | string()
%%    OptionsProplist = [Option]
%%    Option = {client_handler, HandlerFn} | GenTcpOption
%%    GenTcpOption = term()
%%    HandlerFn = function(Parent::pid(), ClientSocket::port())
%% @doc Starts processes listening for connections on Port with
%% Options. The controlling process for the gen_tcp_server will be set
%% to the calling process.
%%
%% Host becomes the gen_tcp {ip, Host} option.
%% 
%% @see gen_tcp:listen/2
%% @end
listen(Host, Port, Options) ->
    gen_server:start(?MODULE, [self(), Host, Port, Options], []).

my_option({client_handler, _Fn}) -> true;
my_option(_) -> false.

%% @spec controlling_process(Server::pid(), NewControllingProcess::pid()) -> ok | {error,Error}
%% @doc Causes all new client notifications to be sent to
%% NewControllingProcess. The gen_tcp_server lifetime will track
%% NewControllingProcess too - when NewControllingProcess exits, the
%% gen_tcp_server will too.
%% @end
controlling_process(Server, Pid) ->
    gen_server:call(Server, {controlling_process, Pid}).

%% @spec close(Server::pid()) -> ok | {error, Error}
%% @see close/2
close(Server) ->
    close(Server, timer:seconds(5)).

%% @spec close(Server::pid(), Timeout::integer()) -> ok | {error, Error}
%% @doc Cleanly shuts down the gen_tcp_server with a Timeout in milliseconds.
%% @end
close(Server, Timeout) ->
    gen_server:call(Server, close, Timeout).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc gen_server callback.
%% @end
%% Initiates the server
%%--------------------------------------------------------------------
init([Parent, Host, Port, Options]) ->
    GenTcpOptions = gen_tcp_options([{ip, Host}, {active, false} | Options]),
    %?INFO("GenTCP Options: ~p", [GenTcpOptions]),
    init(gen_tcp:listen(Port, GenTcpOptions), [Parent, Host, Port, Options]).

init({ok, Socket}, [Parent, Host, Port, Options]) ->
    %?INFO("Got Socket ~p", [Socket]),
    {ok, Acceptor} = proc_lib:start_link(?MODULE, acceptor, [self(), Socket]),
    {ok, #state{listen_socket=Socket,
                acceptor=Acceptor,
                controlling_process=Parent,
                cp_monitor=erlang:monitor(process, Parent),
                host=Host,
                port=Port,
                options=Options}};
init({error, Reason}, _) ->
    {stop, {gen_tcp, Reason}}.

%%--------------------------------------------------------------------
%% @private
%% @spec
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc gen_server callback.
%% @end
%% @see gen_server.
%%--------------------------------------------------------------------
handle_call(close, _From, S) ->
    {stop, normal, ok, S};
handle_call({controlling_process, Pid}, _From, S = #state{controlling_process=P,
                                                          cp_monitor=Ref}) when P =/= Pid ->
    erlang:demonitor(Ref),
    {reply, ok, S#state{controlling_process=Pid,
                        cp_monitor=erlang:monitor(process, Pid)}};
handle_call({new_client, _CliSock}, _From, S = #state{controlling_process=P,
                                                     listen_socket=Socket,
                                                     options=O}) ->
    ClientHandler =  proplists:get_value(client_handler, O,
                                         fun default_client_handler/2),
    {ok, Acceptor} = proc_lib:start_link(?MODULE, acceptor, [self(), Socket]),
    {reply, {ok, ClientHandler, P}, S#state{acceptor=Acceptor}};
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc gen_server callback.
%% @end
%% Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc gen_server callback.
%% @end
%% Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', MonitorRef, process, ContollingProcess, Info},
            S = #state{controlling_process=ContollingProcess,
                       cp_monitor=MonitorRef}) ->
    {stop, {controlling_process_exit, Info}, S};
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%% @doc gen_server callback.
%% @end
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, S = #state{listen_socket=Sck}) when Sck =/= undefined ->
    gen_tcp:close(Sck),
    terminate(Reason, S#state{listen_socket=undefined});
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc gen_server callback.
%% @end
%% Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @private
%% @spec acceptor(Parent::pid(), Socket::port()) -> ok | closed | {error, term()}
%% @doc Proc lib wrapper process for gen_tcp:accept/1.
%% @see gen_tcp:accept/1 
%% @see proc_lib:init_ack/2
acceptor(Parent, Socket) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    case gen_tcp:accept(Socket) of
        {ok, ClientSock} ->
            new_client(Parent, Socket, ClientSock);
        {error, closed} ->
            closed;
        {error, Reason} ->
            exit(Reason)
    end.

new_client(Parent, _ListenSock, ClientSock) ->
    case gen_server:call(Parent, {new_client, ClientSock}) of
        {ok, ClientHandlerFn, ClientParent} when is_function(ClientHandlerFn) ->
            try 
                Result = ClientHandlerFn(ClientParent, ClientSock),
                handle_new_client(Result)
            catch
                Error:Reason ->
                    ?WARN("Couldn't handle new client, ~p ~p", [Error, Reason]),
                    gen_tcp:close(ClientSock),
                    {error, Reason}
            end;
        Else ->
            ?WARN("Couldn't handle new client, ~p", [Else])
    end.

handle_new_client(ok) -> ok;
handle_new_client(Error) -> {error, {client_handler, Error}}.


gen_tcp_options(Options) ->
    lists:filter(fun (O) -> my_option(O) == false end,
                 proplists:unfold(proplists:normalize(Options, []))).

default_client_handler(Parent, ClientSocket) ->
    gen_tcp:controlling_process(ClientSocket, Parent),
    Parent ! {?MODULE, new_client, ClientSocket},
    ok.

gtcps_test() ->
    TestPort = 56432,
    TestValue = "TestValue",
    {ok, Pid} = listen(TestPort, [{packet, 4}]),
    {ok, Socket} = gen_tcp:connect(?LOCALHOST, TestPort, [{packet, 4}]),
    receive
        {?MODULE, new_client, CliSock} ->
            ?assertMatch(ok, gen_tcp:controlling_process(CliSock, self())),
            ?assertMatch(ok, gen_tcp:send(Socket, TestValue)),
            ?assertMatch({ok, TestValue}, gen_tcp:recv(CliSock, 0)),
            ?assertMatch(ok, gen_tcp:close(Socket))
    after timer:seconds(5) ->
            ?assertMatch(no_timeout, timeout)
    end,
    {ok, Socket2} = gen_tcp:connect(?LOCALHOST, TestPort, [{packet, 4}]),
    receive
        {?MODULE, new_client, CliSock2} ->
            ?assertMatch(ok, gen_tcp:controlling_process(CliSock2, self())),
            ?assertMatch(ok, gen_tcp:send(Socket2, TestValue)),
            ?assertMatch({ok, TestValue}, gen_tcp:recv(CliSock2, 0)),
            ?assertMatch(ok, gen_tcp:close(Socket2))
    after timer:seconds(5) ->
            ?assertMatch(no_timeout, timeout)
    end,
    ok = close(Pid).
