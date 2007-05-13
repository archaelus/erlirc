-module(gen_irc).
-version("0.1").
-author("Julian Fondren <ayrnieu@gmail.com>").
-copyright("Copyright (C) 2006 Julian Fondren").
-license("Artistic | GPL, as with Perl.").
-export([connect/1, connect/2, recv/1, recv/2, send/2]).
-export([init/3]).
-compile(export_all).
-include("gen_irc_data.hrl").

% ----------------------------------------------------------------------
connect(Server, Options) ->
    {S,P} = case Server of {_,_} -> Server; _ -> {Server, 6667} end,
    case gen_tcp:connect(S,P,[{packet,line},{active,false}]) of
        {ok, Sock} ->
            {ok, case boolopt(Options,active,true) of
                     true -> spawn(?MODULE, init, [Sock, self(), Options]),
                             Sock;
                     false -> Sock
                 end};
        E -> {error, {gen_tcp, E}}
    end.
connect(Server) -> connect(Server,[]).

% ----------------------------------------------------------------------
init(S, P, O) -> loop(boolopt(O,keepalive,true), S, P).
loop(false, S, P) ->
    case recv(S) of
        {error, E} -> P ! {irc_error, self(), E};
        {ok, L} -> P ! {irc, L}, ?MODULE:loop(false, S, P)
    end;
loop(true, S, P) ->
    case recv(S) of
        {error, E} -> P ! {irc_error, self(), E};
        {ok, {ping, [X]}} -> case send(S,{pong,X}) of
                                 ok -> ?MODULE:loop(true, S, P);
                                 {error, E} -> P ! {irc_error, self(), E}
                             end;
        {ok, L} -> P ! {irc, self(), L}, ?MODULE:loop(true, S, P)
    end.

% ----------------------------------------------------------------------
recv(Sock) -> recv(Sock,infinity).
recv(Sock,Timeout) ->
    case gen_tcp:recv(Sock,0,Timeout) of
        E={error, closed} -> E;
        E={error, timeout} -> E;
        {error, E} -> {error, {gen_tcp, E}};
        {ok, L} -> {ok, filter(parse(chomp(L)))}
    end.

send(IRC, {raw, Text}) ->
    case gen_tcp:send(IRC, [Text, "\r\n"]) of
        {error, E} -> {error, {gen_tcp, E}};
        ok -> ok
    end;
send(IRC, {user, U, N}) -> send(IRC, {user, U, 8, "*", N});
send(IRC, {ctcp, W, C, M}) -> send(IRC, {privmsg, W, [1,C,$\s,M,1]});
send(IRC,M) when tuple(M) ->
    [C|Args] = tuple_to_list(M),
    send(IRC, {raw, [atom_to_list(C), parametize(Args)]});
send(IRC,C) -> send(IRC,{raw,atom_to_list(C)}).

parametize([]) -> "";
parametize([X]) -> [" :", X];
parametize([H|T]) -> [" ", H|parametize(T)].

% ----------------------------------------------------------------------
% [prefix] command parameters*
% prefix indicated by leading $: , without gap.
%  servers use $: to indicate true orgin of message
%  without prefix, message assumed to come from received connection
%  clients only have one valid prefix: the nickname associated with them
%  servers silently ignore invalidly prefixed messages
% command is a valid IRC command or a three digit digit
% BNF indicates that params end on a :-prefixed param, which param
%  consumes the entire rest of the message.
% server can also reply to commands with a 'numeric reply'
% (clients can talk to a list of users?!)

filter({P,privmsg,[T,[1|X]]}) ->
    {Type,M} = case lists:splitwith(fun ($\s) -> false; (_) -> true end,
                                    lists:delete(1,X)) of
                   {T0,[$\s|M0]} -> {T0,M0};
                   R={_,[]} -> R
               end,
    {P,{ctcp,list_to_atom(lowercase(Type))},[T,M]};
filter(X) -> X.

-define(userre, "[^!@]+|^[#&].+").
split_user(S) ->
    case matches(S, regexp:matches(S, ?userre)) of
        [K,N,H] -> #user{nick=K, name=N, host=H};
        [C=[$#|_]] -> #channel{name=C};
        [C=[$&|_]] -> #channel{name=C};
        [S] -> S
    end.

parse(Text) ->
    case split(Text) of
        [[$:|P],C,T,M] -> {split_user(P),parse_command(C),[split_user(T),M]};
        [[$:|P],C|R] -> {split_user(P), parse_command(C), R};
        [C|R] -> {parse_command(C), R}
    end.

parse_command(S) ->
    case string:to_integer(S) of
        {error,no_integer} -> list_to_atom(lowercase(S));
        {N,[]} -> try reply(N)
                  catch
                      error:_ -> S
                  end
    end.

-define(ircre, "[^ ]+|:.*$").
split([$:|L]) ->
    [H|T] = split(L),
    [[$:|H]|T];
split(L) -> matches(L,regexp:matches(L, ?ircre)).

matches(L,{match,M=[_|_]}) -> matches(L,M);
matches(_,[]) -> [];
matches(L,[{N,M}|T]) -> [case lists:sublist(L, N, M) of
                             [$:|S] -> S;
                             S -> S
                         end|matches(L,T)].

reply(N) when N >= 400, N =< 600 -> {error, gen_irc_codes:error(N)};
reply(N) when N >= 200, N < 400 -> {reply, gen_irc_codes:reply(N)}.

% ----------------------------------------------------------------------
boolopt(Os, O, Default) ->
    case lists:keysearch(O,1,Os) of
        {value, {_, B}} -> B;
        false -> Default
    end.

chomp(S) -> lists:reverse(chomp_(lists:reverse(S))).
chomp_([$\n|T]) -> chomp_(T);    
chomp_([$\r|T]) -> chomp_(T);
chomp_(S) -> S.

lowercase([C|T]) when C =< $Z, C >= $A -> [C-$A+$a|lowercase(T)];
lowercase([C|T]) -> [C|lowercase(T)];
lowercase([]) -> [].
