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
         ,msg/5]).

%%====================================================================
%% API
%%====================================================================

msg(Type, Ref, {Pid, Name}, Msg) ->
    msg(Type, Ref, Pid, Name, Msg);
msg(Type, Ref, Name, Msg) ->
    msg(Type, Ref, self(), Name, Msg).

msg(Type, Ref, Pid, Name, Msg)
  when Type =:= server orelse Type =:= user orelse Type =:= channel,
       is_reference(Ref),
       is_pid(Pid) orelse is_atom(Pid) ->
    {irc, Type, Ref, {Pid, Name}, Msg}.

%%====================================================================
%% Internal functions
%%====================================================================
