%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Models an IRC channel
%% @end
%%%-------------------------------------------------------------------
-module(irc_channel).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").
-include_lib("irc.hrl").

%% API
-export([start_link/2,
         join/3,
         get_evtmgr/1,
         info/1,
         alter/3,
         exists/2,
         part/2,
         call/2,
         members/1,
         topic/1,
         topic/2,
         topicinfo/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {net, chan, eventpid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Net, Chan) when is_list(Chan)->
    start_link(Net, #chan{name=Chan});
start_link(Net, C = #chan{}) ->
    gen_server:start_link(?MODULE, [Net, C], []).

call(C = {irc_channel, _Net, _Chan}, Arg) ->
    gen_server:call(proc_reg:where(C), Arg);
call(Pid, Arg) when is_pid(Pid) ->
    gen_server:call(Pid, Arg).

join(Chan, U = #user{}, Pid) ->
    call(Chan, {join, U, Pid}).

part(Chan, U = #user{}) ->
    call(Chan, {part, U}).

members(Chan) ->
    call(Chan, members).

topic(Chan) ->
    call(Chan, topic).

topic(Chan, Topic) ->
    call(Chan, {topic, Topic}).

topicinfo(Chan, Ts, Author) ->
    call(Chan, {topicinfo, Ts, Author}).

get_evtmgr(Chan) ->
    call(Chan, get_evtmgr).

info(Chan) ->
    call(Chan, info).

alter(Chan, Field, Value) ->
    call(Chan, {alter, Field, Value}).

exists(Net, Name) ->
    proc_reg:where({?MODULE, Net, Name}) /= undefined.

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
init([Net, C = #chan{}]) ->
    proc_reg:reg({?MODULE, Net, C#chan.name}, self()),
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, {irc_gen_event_debug_callback, debug},
                          [debug, fun (_Id, E) -> 
                                          io_lib:format("~s -- ~p", [C#chan.name,E])
                                  end]),
    {ok, #state{chan=C, eventpid=Pid}};
init(_Options) ->
    throw(not_implemented).

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
handle_call(topic, _From, S = #state{chan=#chan{topic=T}}) ->
    {reply, T, S};
handle_call({topicinfo, TS, Author}, _From, S = #state{chan=C}) ->
    NewTopic = (C#chan.topic)#topic{topic_ts=TS,
                                    author=Author},
    notify(S, {topicinfo, TS, Author}),
    {repl, ok, S#state{chan=C#chan{topic=NewTopic}}};
handle_call({topic, Topic}, _From, S = #state{chan=C}) ->
    NewTopic = (C#chan.topic)#topic{text=Topic},
    notify(S, {topic, NewTopic}),
    {repl, ok, S#state{chan=C#chan{topic=NewTopic}}};
handle_call(members, _From, S = #state{chan=C}) ->
    {reply, C#chan.members, S};
handle_call({alter, Field, Value}, _From, S = #state{chan=C}) ->
    case i_alter(C, Field, Value) of
        {error, E} -> {reply, {error, E}, S};
        Chan = #chan{} ->
            {reply, Chan, S#state{chan=Chan}}
    end;
handle_call(info, _From, S = #state{chan=C}) ->
    {reply, C, S};
handle_call(get_evtmgr, _From, S) ->
    {reply, S#state.eventpid, S};
handle_call({join, User, Pid}, _From, S = #state{chan=Chan}) ->
    Ref = erlang:monitor(process, Pid),
    NewMembers = lists:usort([User#user{info=Ref}|Chan#chan.members]),
    NewChan = Chan#chan{members=NewMembers},
    notify(S, {join, User}),
    {reply, {ok, NewChan},S#state{chan=NewChan}};
handle_call({part, User}, From, S = #state{chan=Chan}) when is_list(User)->
    handle_call({part, #user{nick=User}}, From, S);
handle_call({part, User = #user{nick=Nick}}, From, S = #state{chan=Chan})
    {Parting, Staying} = lists:partition(fun (#user{nick=N}) when N == Nick -> true;
                                             (_) -> false end,
                                         Chan#chan.members),
    lists:foreach(fun (U = #user{info=R, nick=N}) when is_reference(R) ->
                          erlang:demonitor(R),
                          notify(S, {part, U});
                      (U) -> notify(S, {part, U})
                  end,
                  Parting),
    {reply, ok, S#state{chan=Chan#chan{members=Staying}}};
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
handle_info({'DOWN', Ref, process, _Pid, _Reason}, S = #state{chan=Chan}) ->
    {Parting, Staying} = lists:partition(fun (#user{info=R}) when R == Ref -> true;
                                             (_) -> false end,
                                         Chan#chan.members),
    lists:foreach(fun (U) ->
                          notify(S, {part, U})
                  end,
                  Parting),
    {noreply, S#state{chan=Chan#chan{members=Staying}}};
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
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

i_alter(C = #chan{}, numeric, Value) ->
    C#chan{numeric=Value};
i_alter(C = #chan{}, name, Value) ->
    C#chan{name=Value};
i_alter(C = #chan{}, chan_ts, Value) ->
    C#chan{chan_ts=Value};
i_alter(C = #chan{}, mode, Value) ->
    C#chan{mode=Value};
i_alter(C = #chan{}, topic, Value) ->
    C#chan{topic=Value};
i_alter(C = #chan{}, type, Value) ->
    C#chan{type=Value};
i_alter(C = #chan{}, info, Value) ->
    C#chan{info=Value};
i_alter(_, _, _) ->
    {error, unknown_field}.

notify(#state{eventpid=P}, E) ->
    gen_event:notify(P, E).
