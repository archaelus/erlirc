%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(gen_irc).

%% API
-export([msg/4
         ,msg/3]).

%%====================================================================
%% API
%%====================================================================

msg(Type, Ref, Msg) ->
    msg2(Type, Ref, self(), Msg).
                            
msg(Type, Ref, {Pid, Name}, Msg)
  when is_pid(Pid) orelse is_atom(Pid), is_list(Name) ->
    msg2(Type, Ref, {Pid, Name}, Msg);
msg(Type, Ref, Name, Msg) when is_list(Name)->
    msg2(Type, Ref, {self(), Name}, Msg).

%%====================================================================
%% Internal functions
%%====================================================================

msg2(Type, Ref, From, Msg)
  when Type =:= server orelse Type =:= user orelse Type =:= channel,
       is_reference(Ref) ->
    {irc, Type, Ref, From, Msg}.

%%====================================================================
%% Message Examples
%%====================================================================

%% From = {username, Pid} | {channelname, Pid} | Pid

%% {irc, user, Ref, From, UserMsg}
%% UserMsg = {nick, "Nick"}
%% {irc, channel, Ref, From, ChanMsg}
%% ChanMsg = join | part | topic | {topic, Text} | {msg, Text}
