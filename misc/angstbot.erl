-module(angstbot).
-compile(export_all).
-include("gen_irc_data.hrl").

start() -> start("irc.perl.org", "angstbot").
start(Server, Nick) -> spawn(?MODULE, init, [Server,Nick]).
init(Server, Nick) ->
    {ok, IRC} = gen_irc:connect(Server),
    gen_irc:send(IRC, {nick,Nick}),
    gen_irc:send(IRC, {user,"julian","Julian Fondren"}),
    spawn(?MODULE, monitor, [self()]),
    put(angst,angst:load()),
    angst(),
    loop(IRC).

monitor(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Angstbot crashed: ~p\n", [Reason])
    end.

angst() ->
    A = angst:fetch(),
    As = [A|get(angst)],
    put(angst,As),
    angst:save(As),
    A.

last() -> hd(get(angst)).

command(IRC,_,#channel{name=C},"angst") -> gen_irc:send(IRC,{privmsg,C,angst()});
command(IRC,_,#channel{name=C},"last angst") -> gen_irc:send(IRC,{privmsg,C,last()});
command(IRC,#user{nick=K},_,"angst") -> gen_irc:send(IRC,{privmsg,K,angst()});
command(IRC,#user{nick=K},_,"last angst") -> gen_irc:send(IRC,{privmsg,K,last()});
command(_,_,_,_) -> 'no such command'.

purl(IRC,_,C,"couldn't get the headlines: http://cou.ch/teenopendiary.cgi"++_) ->
    gen_irc:send(IRC,{privmsg,C,last()}), angst();
purl(IRC,M,C,Message) ->
    case regexp:match(Message, ": that gave some error$") of
        {match,_,_} -> gen_irc:send(IRC,{privmsg,C,last()}), angst();
        nomatch -> io:format("Angst: ~p\n", [M])
    end.
            
loop(IRC) ->
    io:format("meep.\n"),
    receive
        {irc,_,{From,privmsg,[To="angstbot",C]}} -> command(IRC,From,To,C), loop(IRC);
        {irc,_,{From,privmsg,[To,"angstbot: "++C]}} -> command(IRC,From,To,C), loop(IRC);
        {irc,_,{_,privmsg,[_, "angst"++_]}} -> angst(), loop(IRC);
        {irc,_,{_,privmsg,[_, "purl: angst"++_]}} -> angst(), loop(IRC);
        {irc,_,{_,privmsg,[_, "purl, angst"++_]}} -> angst(), loop(IRC);
        {irc,_,M={#user{nick="purl"},privmsg,[#channel{name=C}, Message]}} ->
            purl(IRC,M,C,Message), loop(IRC);
        {irc,_,M} -> io:format("Angst: ~p\n", [M]), loop(IRC);
        {command,C} -> gen_irc:send(IRC,C), loop(IRC);
        {angst,C,W} -> gen_irc:send(IRC,{privmsg,C,W ++ ": " ++ angst()}), loop(IRC);
        last -> io:format("~p\n", [last()]);
        cached -> io:format("~p\n", [get(angst)]), loop(IRC);
        refresh -> ?MODULE:loop(IRC);
        close -> ok;
        X -> io:format("Unhandled message: ~p\n", [X]), loop(IRC)
    end.
