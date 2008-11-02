%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc irc channel prototype
%% @end
%%%-------------------------------------------------------------------
-module(irc_channel).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("irc.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/2
         ,start_monitor/2
         ,shutdown/1
         ,gproc_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(member, {nick, pid, role, mode, user, ref}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Net, Name) ->
    gen_server:start_link(?MODULE, [#chan{net=Net,name=Name}], []).

start_monitor(Net, Name) ->
    case gen_server:start(?MODULE, [#chan{net=Net,name=Name}], []) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {ok, Pid, Ref};
        Else ->
            Else
    end.

gproc_name(#chan{net=Net,name=Name})->
    gproc:name({irc_chan, Net, Name}).

shutdown(Pid) ->
    gen_server:call(Pid, shutdown).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialises the server's state
%% @end
%%--------------------------------------------------------------------
init([Chan = #chan{}]) ->
    true = gproc:reg(gproc_name(Chan),self()),
    {ok, Chan#chan{topic=undefined,
                   mode=""}}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Call message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_call(shutdown, _From, State) ->
    {stop, shutdown, ok, State};
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Cast message handler callbacks
%% @end
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
%% @doc Non gen-server message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_info({irc, channel, Ref, From, Msg}, Chan) ->
    Member = find_member(From, Chan),
    %%?INFO("Got chanmsg from ~p ~p/~p:~n~p", [Member, From, Ref, Msg]),
    NewChan = case handle_chan_msg(Msg, Member, From, Chan) of
                  {reply, ReplyMsg, Chan2} ->
                      %%?INFO("Replying to ~p: ~p", [From, ReplyMsg]),
                      send(chan_msg(ReplyMsg, Ref, Chan2), From),
                      Chan2;
                  {reply_all, ReplyMsg, Chan2} ->
                      broadcast_chan_msg(ReplyMsg, Ref, Chan2);
                  {complex_reply,
                   UserMsg,
                   ChanMsg,
                   Chan2} ->
                      %%?INFO("Replying to ~p: ~p", [From, UserMsg]),
                      send(chan_msg(UserMsg, Ref, Chan), From),
                      broadcast_chan_msg(ChanMsg, Ref, Chan),
                      Chan2
              end,
    {noreply, NewChan};
handle_info({'DOWN', Ref, process, Pid, _}, C = #chan{members=Members}) ->
    case find_member(Ref, Members) of
        no_member ->
            ?WARN("DOWN from unmonitored process, ~p", [Pid]),
            {noreply, C};
        Pid -> {noreply, C};
        M = #member{nick=Nick} ->
            NewChan = C#chan{members=lists:delete(M, Members)},
            broadcast_chan_msg({parted, Nick}, make_ref(), NewChan),
            {noreply, NewChan}
    end;
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.


handle_chan_msg(join, #member{}, _From, Chan) ->
    {reply, {error, already_joined}, Chan};
handle_chan_msg(join, non_member, {Pid, Nick}, Chan) ->
    URef = erlang:monitor(process, Pid),
    Role = default_role(Chan),
    Mode = default_mode(Role),
    Member = #member{pid=Pid,nick=Nick,ref=URef,role=Role,mode=Mode},
    NewChan = Chan#chan{members=[Member|Chan#chan.members]},
    {complex_reply,
     {joined, Chan#chan.topic, Chan#chan.mode,
      [{N,R} || #member{nick=N,role=R} <- members(NewChan)]},
     {joined, Nick, Mode},
     NewChan};

handle_chan_msg(_, non_member, _, Chan) ->
    {reply, {error, non_member}, Chan};

handle_chan_msg(part, M = #member{nick=Nick, ref=Mref}, _, Chan) ->
    erlang:demonitor(Mref, [flush]),
    NewChan = Chan#chan{members=lists:delete(M, Chan#chan.members)},
    {reply_all, {parted, Nick}, NewChan};

handle_chan_msg(topic, _, _, Chan) ->
    {reply, Chan#chan.topic, Chan};
handle_chan_msg({topic, Text}, Member, _From, Chan) ->
    NewTopic = #topic{text=Text,author=Member,ts=erlang:now()},
    {reply_all, NewTopic, Chan#chan{topic=NewTopic}};

handle_chan_msg({msg, Text}, _Member, _From, C = #chan{}) ->
    {reply_all, {msg, Text}, C}.

%%--------------------------------------------------------------------
%% @private
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
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

find_member(M, #chan{members=Members}) ->
    find_member(M, Members);

find_member(_, []) ->
    non_member;
find_member({Pid, Nick}, [M = #member{pid=Pid,nick=Nick}|_]) -> M;
find_member(Pid, [M = #member{pid=Pid}|_]) when is_pid(Pid) -> M;
find_member(Pid, [Pid|_]) when is_pid(Pid) -> Pid;
find_member(Ref, [M = #member{ref=Ref}|_]) when is_reference(Ref) -> M;
find_member(Member, [_|Rest]) ->
    find_member(Member, Rest).

default_role(#chan{members=[]}) -> op;
default_role(#chan{members=_}) -> user.

default_mode(op) -> "@";
default_mode(user) -> "".

members(#chan{members=M}) -> M;
members(M) when is_list(M) -> M.

broadcast(Msg, Members) ->
    lists:foreach(fun (Member) ->
                          %%?INFO("Broadcasting ~p to ~p", [Msg, Member]),
                          send(Msg, Member)
                  end,
                  members(Members)).

chan_msg(Msg, Ref, C = #chan{}) when is_reference(Ref) ->
    gen_irc:msg(channel, Ref, C#chan.name, Msg).

broadcast_chan_msg(Msg, Ref, C) ->
    %%?INFO("Broadcasting ~p/~p to channel", [Msg, Ref]),
    broadcast(chan_msg(Msg, Ref, C), C).

send(Msg, #member{pid=To}) when is_pid(To); is_atom(To) ->
    To ! Msg;
send(Msg, {To, _Nick}) when is_pid(To); is_atom(To) ->
    To ! Msg;
send(Msg, To) when is_pid(To); is_atom(To) ->
    To ! Msg.    

find_member_test() ->
    Members1 = [],
    Member1Ref = make_ref(),
    Member1Pid = list_to_pid("<0.21.21>"),
    Member1 = #member{pid=Member1Pid,
                      ref=Member1Ref,
                      nick="Foo",
                      role=op,
                      mode="@"},
    Member2Ref = make_ref(),
    Member2Pid = list_to_pid("<0.21.22>"),
    Member2 = #member{pid=Member2Pid,
                      ref=Member2Ref,
                      nick="Bar",
                      role=user,
                      mode=""},
    Members2 = [Member1],
    Members3 = [Member1, Member2],
    ?assertMatch(non_member,
                 find_member({Member1Pid,"Foo"},Members1)),
    ?assertMatch(#member{ref=R} when R =:= Member1Ref,
                 find_member({Member1Pid,"Foo"},Members2)),
    ?assertMatch(#member{ref=R} when R =:= Member1Ref,
                 find_member(Member1Ref, Members2)),
    ?assertMatch(#member{ref=R} when R =:= Member2Ref,
                 find_member({Member2Pid,"Bar"},Members3)),
    ?assertMatch(#member{ref=R} when R =:= Member2Ref,
                 find_member(Member2Ref, Members3)).
