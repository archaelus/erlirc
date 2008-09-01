%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_channel_tests).

-include_lib("eunit.hrl").
-include_lib("logging.hrl").

channel_test() ->
    {ok, Pid} = irc_channel:start_link("example", "example"),
    Self = self(),
    Ref1 = make_ref(),
    Pid ! {irc, channel, Ref1, {Self, "test"}, join},
    receive
        {irc, channel, Ref1, {Pid, "example"},
         {joined, undefined, _, [{"test", operator}]}} ->
            ?assert(true);
        Other ->
            ?ERR("Got ~p", [Other]),
            ?assert(false)
    after 1000 ->
            ?assert(false)
    end,
    Ref2 = make_ref(),
    Pid ! {irc, channel, Ref2, {Self, "test"}, join},
    receive
        {irc, channel, Ref2, {Pid, "example"},
         {error, already_joined}} ->
            ?assert(true);
        Other2 ->
            ?ERR("Got ~p", [Other2]),
            ?assert(false)
    after 1000 ->
            ?assert(false)
    end,
    Ref3 = make_ref(),
    Pid ! {irc, channel, Ref3, Self, {msg, "Hi."}},
    receive
        {irc, channel, Ref3, {Pid, "example"},
         {msg, "Hi."}} ->
            ?assert(true);
        Other3 ->
            ?ERR("Got ~p", [Other3]),
            ?assert(false)
    after 1000 ->
            ?assert(false)
    end,
    irc_channel:shutdown(Pid).
