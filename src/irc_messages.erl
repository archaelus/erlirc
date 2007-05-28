%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_messages).

-include_lib("irc.hrl").
-include_lib("logging.hrl").
-include_lib("eunit.hrl").


%% API
-export([parse_args/1,
         parse_line/1,
         to_list/1,
         now_to_unix_ts/1,
         encode_ctcp_delims/1,
         decode_ctcp_delims/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () ->
%% @doc 
%% @end 

parse_line(Line) ->
    parse_args(irc_parser:parse_line(Line)).

%%--------------------------------------------------------------------
parse_args(Cmd) when is_record(Cmd, irc_cmd) ->
    parse_args(Cmd#irc_cmd.name, Cmd#irc_cmd.args, Cmd).

parse_args(error, ":" ++ Args, Cmd) ->
    parse_error(Cmd, lists:split(string:chr(Args, $:), Args));
parse_args(notice, Args, Cmd) ->
    parse_notice(Cmd, string:tokens(Args, ":"));

parse_args(needmoreparams, Args, Cmd) ->
    parse_error(needmoreparams, string:tokens(Args, ":"), Cmd);

parse_args(pass, ":" ++ Pass, Cmd) ->
    Cmd#irc_cmd{args=[{pass, Pass}]};

parse_args(server, Args, Cmd) ->
    parse_server(Cmd, Args);

parse_args(nick, Args, Cmd) ->
    parse_nick(Cmd, Args);

parse_args(end_of_burst, _Args, Cmd) ->
    Cmd#irc_cmd{args = []};
parse_args(burst, Args, Cmd) ->
    parse_burst(Cmd, string:tokens(Args, " "));

parse_args(PingPong, [$:|Token], Cmd) when PingPong == ping; PingPong == pong ->
    Cmd#irc_cmd{args=[{token , Token}]};

parse_args(privmsg, Args, Cmd) ->
    {Target, Msg} = irc_parser:split($:, Args),
    parse_privmsg(Cmd#irc_cmd{target=string:strip(Target, right, $\s)},
                  Msg);

parse_args(join, [$:|Chans], Cmd) ->
    Cmd#irc_cmd{args=[{channels, string:tokens(Chans, ",")}]};

parse_args(Name, Args, Cmd) when Name == welcome;
                                 Name == yourhost;
                                 Name == created;
                                 Name == myinfo;
                                 Name == isupport;
                                 Name == luserclient;
                                 Name == luserop;
                                 Name == luserchannels;
                                 Name == luserme;
                                 Name == luserconns;
                                 Name == motdstart;
                                 Name == motd;
                                 Name == endofmotd;
                                 Name == n_global;
                                 Name == n_local ->
    {Target, Msg} = irc_parser:split(Args),
    Cmd#irc_cmd{target=Target,
                args=[{message, Msg}]}.


%%--------------------------------------------------------------------

parse_privmsg(Cmd, Msg) ->
    case lists:member(1, Msg) of
        true -> parse_ctcp_msg(Cmd, Msg);
        false -> Cmd#irc_cmd{args=[{message, Msg}]}
    end.

parse_ctcp_msg(Cmd, Msg) ->
    {Ctcp, NonCtcp} = lists:partition(fun ({ctcp, _}) -> true; (_) -> false end,
                                      decode_ctcp_delims(Msg)),
    Cmd#irc_cmd{args=[{message,irc_parser:join(" ", lists:append([M||{_,M}<-NonCtcp]))}],
                ctcp=[parse_ctcp_cmd(C)||{_,C} <- Ctcp]}.

parse_ctcp_cmd(Msg) ->
    case irc_parser:split(Msg) of
        {Cmd,[]} ->
            #ctcp_cmd{name=irc_commands:ctcp_from_list(Cmd),
                      args=[]};
        {Cmd, Arg} ->
            Name = irc_commands:ctcp_from_list(Cmd),
            parse_ctcp_cmd(Name, #ctcp_cmd{name=Name}, Arg)
    end.

parse_ctcp_cmd(action, C, Msg) ->
    C#ctcp_cmd{args=[{action, Msg}]};
parse_ctcp_cmd(finger, C, [$:|Msg]) ->
    C#ctcp_cmd{args=[{info, Msg}]};
parse_ctcp_cmd(version, C, Msg) ->
    case string:tokens(Msg, ":") of
        {Client, Version, Environment} ->
            C#ctcp_cmd{args=[{client, Client},
                             {version, Version},
                             {environment, Environment}]};
        _ ->
            C#ctcp_cmd{args=[{unparsed, Msg}]}
    end;
parse_ctcp_cmd(userinfo, C, [$:|Info]) ->
    C#ctcp_cmd{args=[{info, Info}]};
parse_ctcp_cmd(ping, C, Ts) ->
    C#ctcp_cmd{args=[{token, Ts}]};
parse_ctcp_cmd(Cmd, _C, _) ->
    throw({not_implemented, ctcp, Cmd}).

ctcp_to_list(#ctcp_cmd{name=version, args=A}) ->
    Client = proplists:get_value(client, A, "erlirc"),
    Version = proplists:get_value(client, A, "0.0.1"),
    Environment = proplists:get_value(client, A, "erlang"),
    io_lib:format("VERSION ~s:~s:~s", [Client, Version, Environment]);
ctcp_to_list(#ctcp_cmd{name=action, args=[{action, A}]}) ->
    "ACTION " ++ A;
ctcp_to_list(C) ->
    throw({not_implemented, ctcp_to_list, C}).

decode_ctcp_delims(Msg) ->
    decode_ctcp_delims(non_ctcp, Msg, [], []).


decode_ctcp_delims(State, This, ThisPart, Parts) when This == [1]; This == [] ->
    lists:reverse(tag_part(State, ThisPart, Parts));
decode_ctcp_delims(non_ctcp, [1|Rest], ThisPart, Parts) ->
    decode_ctcp_delims(ctcp, Rest, [], tag_part(non_ctcp, ThisPart, Parts));
decode_ctcp_delims(ctcp, [1|Rest], ThisPart, Parts) ->
    decode_ctcp_delims(non_ctcp, Rest, [], tag_part(ctcp, ThisPart, Parts));
decode_ctcp_delims(State, [This|Rest], ThisPart, Parts) ->
    decode_ctcp_delims(State, Rest, [This|ThisPart], Parts).

tag_part(_Tag, [], Parts) ->
    Parts;
tag_part(Tag, Part, Parts) ->
    [{Tag, lists:reverse(Part)}|Parts].

encode_ctcp_delims(Parts) ->
    encode_ctcp_delims(non_ctcp, Parts, []).

encode_ctcp_delims(ctcp, [], Acc) ->
    lists:reverse([1|Acc]);
encode_ctcp_delims(non_ctcp, [], Acc) ->
    lists:reverse(Acc);
encode_ctcp_delims(ctcp, [{ctcp, Part}|Parts], Acc) ->
    encode_ctcp_delims(ctcp, Parts, lists:reverse(Part) ++ [1,1|Acc]);
encode_ctcp_delims(non_ctcp, [{non_ctcp, Part}|Parts], Acc) ->
    encode_ctcp_delims(non_ctcp, Parts, lists:reverse(Part) ++ Acc);
encode_ctcp_delims(_State, [{OtherState, Part}|Parts], Acc) ->
    encode_ctcp_delims(OtherState, Parts, lists:reverse(Part) ++ [1|Acc]).

%%--------------------------------------------------------------------
parse_error(Cmd, {Reason, Text}) ->
    Cmd#irc_cmd{args = [{error, string:strip(Reason, right, $:)},
                    {text, string:strip(Text, both, $\s)}]};
parse_error(Cmd, [Reason | Text]) ->
    Cmd#irc_cmd{args = [{error, Reason}, {text, lists:append(Text)}]}.

parse_error(Code, [Info, Text], Cmd) ->
    case string:tokens(Info, " ") of
        [Target, Command] ->
            Cmd#irc_cmd{name=error,
                        target=Target,
                        args=[{code, Code},
                              {command, Command},
                              {text, Text}]}
    end.


%%--------------------------------------------------------------------
parse_notice(Cmd, [Target, Text]) ->
    Cmd#irc_cmd{args = [{text, Text}], target=Target};
parse_notice(Cmd, [Target | Text]) ->
    Cmd#irc_cmd{target = string:strip(Target, right, $\s),
                args = [{text, irc_parser:join($:, Text)}]}.

%%--------------------------------------------------------------------
parse_server(Cmd, Args) when is_list(Args) ->
    parse_server(Cmd, irc_parser:split($:, Args));
parse_server(Cmd, {Args,Description}) ->
    parse_server(Cmd, string:tokens(Args, " "), Description).

parse_server(Cmd, [Name, HopCount, BootTS, LinkTS, Proto, [A,B|MaxClient], Flags], Description) ->
    Cmd#irc_cmd{target=#p10server{numeric=irc_numerics:p10b64_to_int([A,B]),
                              name=Name,
                              hopcount=HopCount,
                              boot_ts=BootTS,
                              link_ts=LinkTS,
                              protocol=Proto,
                              max_client=MaxClient,
                              flags=Flags,
                              description=Description}}.


%%--------------------------------------------------------------------
parse_nick(Cmd, Args) when is_list(Args) ->
    parse_nick(Cmd, split_one_prefix_many_space(Args));
parse_nick(Cmd, {[Nick,_Something,NickTS,UserName,HostName,"+" ++ UMode,AuthName,Numeric], Description}) ->
    Cmd#irc_cmd{target=#user{numeric=Numeric,
                             nick=Nick,
                             nick_ts=NickTS,
                             user=UserName,
                             host=HostName,
                             authname=AuthName,
                             mode=UMode,
                             description=Description}};
parse_nick(Cmd, {[Nick,_Something,NickTS,UserName,HostName,AuthName,Numeric], Description}) ->
    Cmd#irc_cmd{target=#user{numeric=Numeric,
                             nick=Nick,
                             nick_ts=NickTS,
                             user=UserName,
                             host=HostName,
                             authname=AuthName,
                             mode="",
                             description=Description}}.

%%--------------------------------------------------------------------
parse_burst(Cmd, [Name, ChanTS, "+" ++ ChanMode, UserData]) ->
    parse_burst_userdata(Cmd#irc_cmd{target=#chan{name=Name,
                                              chan_ts=ChanTS,
                                              mode=ChanMode}},
                         string:tokens(UserData, ":"));
parse_burst(Cmd, [Name, ChanTS, UserData]) ->
    parse_burst_userdata(Cmd#irc_cmd{target=#chan{name=Name,
                                                  chan_ts=ChanTS,
                                                  mode=""}},
                         string:tokens(UserData, ":")).

parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, ["o," ++ Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    OldOps = Chan#chan.ops,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{ops=lists:umerge(UserNumerics,
                                                                       OldOps)}},
                         Rest);
parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, ["v," ++ Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    OldVoices = Chan#chan.voices,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{voices=lists:umerge(UserNumerics,
                                                                          OldVoices)}},
                         Rest);
parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, ["ov," ++ Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    OldOps = Chan#chan.ops,
    OldVoices = Chan#chan.voices,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{voices=lists:umerge(UserNumerics,
                                                                          OldVoices),
                                                      ops=lists:umerge(UserNumerics,
                                                                       OldOps)}},
                         Rest);
parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, [Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    OldUsers = Chan#chan.users,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{users=lists:umerge(UserNumerics,
                                                                         OldUsers)}},
                         Rest);
parse_burst_userdata(Cmd, []) ->
    Cmd.

%%--------------------------------------------------------------------
to_list(Cmd) when is_record(Cmd, irc_cmd) ->
    to_list(Cmd#irc_cmd.name, Cmd#irc_cmd.args, Cmd) ++ "\r\n".

to_list(pass, Args, _Cmd) when length(Args) >= 3 ->
    Pass = proplists:get_value(password, Args),
    Ver = proplists:get_value(version, Args),
    Flags = proplists:get_value(flags, Args),
    "PASS " ++ Pass ++ " " ++ Ver ++ " " ++ Flags;
to_list(pass, [{pass, Pass}], _Cmd) ->
    "PASS " ++ Pass;

to_list(server, Args, _Cmd) when length(Args) == 4 ->
    Name = proplists:get_value(servername, Args),
    Hopcount = proplists:get_value(hopcount, Args),
    Token = proplists:get_value(token, Args),
    Info = proplists:get_value(info, Args),
    "SERVER " ++ Name ++
        " " ++ integer_to_list(Hopcount) ++
        " " ++ integer_to_list(Token) ++
        " :" ++ Info;
to_list(server, Args, _Cmd) when length(Args) == 8 ->
    Name = proplists:get_value(servername, Args),
    Hopcount = proplists:get_value(hopcount, Args),
    BootTS = proplists:get_value(boot_time, Args),
    LinkTS = proplists:get_value(link_time, Args),
    Protocol = proplists:get_value(protocol, Args),
    ServerNumeric = proplists:get_value(numeric, Args),
    MaxClient = proplists:get_value(max_client, Args),
    Info = proplists:get_value(info, Args),
    "SERVER " ++ Name ++ " " ++
        integer_to_list(Hopcount) ++ " " ++
        now_to_unix_ts_list(BootTS) ++ " " ++
        now_to_unix_ts_list(LinkTS) ++ " " ++
        Protocol ++ " " ++
        if is_list(ServerNumeric) -> ServerNumeric;
           is_integer(ServerNumeric) -> irc_numerics:int_to_p10b64(ServerNumeric, 2) end ++
        if is_list(MaxClient) -> MaxClient;
           is_integer(MaxClient) -> irc_numerics:int_to_p10b64(MaxClient, 3) end ++
        " 0 :" ++ Info;

to_list(pong, [{token, Token}], _Cmd) ->
    "PONG :" ++ Token;

to_list(nick, [{name, Name}], _Cmd) ->
    "NICK " ++ Name;

to_list(quit, _, _Cmd) ->
    "QUIT";

to_list(join, Args, _cmd) ->
    Chans = proplists:get_value(channels, Args, []),
    Cs = case lists:partition(fun is_tuple/1, Chans) of
             {[], NonKeyed} ->
                 irc_parser:join(",", NonKeyed);
             {Keyed, NonKeyed} ->
                 irc_parser:join(",", [C || {C,_} <- Keyed] ++ NonKeyed) ++ " " ++
                     irc_parser:join(",", [K || {_C, K} <- Keyed])
         end,
    lists:flatten(["JOIN ", Cs]);

to_list(Name, [{message, M}],
        #irc_cmd{target=T, ctcp = undefined}) when Name == notice;
                                                   Name == privmsg ->
    lists:flatten([irc_commands:to_list(Name), nick(T), " ", M]);

to_list(Name, Args, 
        #irc_cmd{target=T, ctcp = Ctcp}) when Name == notice;
                                              Name == privmsg ->
    M = proplists:get_value(message, Args, ""),
    CtcpParts = [{non_ctcp, M}|
                 [{ctcp, ctcp_to_list(C)}||C<-Ctcp]],
    lists:flatten([irc_commands:to_list(Name),
                   " ", nick(T), " ", encode_ctcp_delims(CtcpParts)]);

to_list(user, Args, _Cmd) ->
    Name = proplists:get_value(user_name, Args),
    Mode = proplists:get_value(mode, Args, "+w"),
    RealName = proplists:get_value(real_name, Args, "Erlang Hacker"),
    lists:flatten(["USER ", Name, " ", Mode, " * :", RealName]).

%%====================================================================
%% Internal functions
%%====================================================================

now_to_unix_ts(Tm) ->
    calendar:datetime_to_gregorian_seconds(Tm) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

now_to_unix_ts_list(Tm) ->
    integer_to_list(now_to_unix_ts(Tm)).


split_one_prefix_many_space(Str) ->
    {SpaceSepArgs,PrefArg} = irc_parser:split($:, Str),
    {string:tokens(SpaceSepArgs, " "), PrefArg}.

nick(N) when is_atom(N) ->
    atom_to_list(N);
nick(#user{}) ->
    throw({not_implemented, nick, "#user"});
nick(N) when is_list(N) ->
    N.

parse_error_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("ERROR :Closing Link: erl.irc by ve.irc.dollyfish.net.nz (No C:line)\r\n")).

parse_line_1_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("SERVER ve.irc.dollyfish.net.nz 1 1164352162 1171089421 J10 ACAP] +h :ircd on ve\r\n")).
parse_line_2_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC S scorch.irc.dollyfish.net.nz 2 0 1164963563 P10 ABAP] +h :DollyNET ircd at irc.dollyfish.net.nz\r\n")).
parse_line_3_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AB N Ned 2 1166709690 ~martyn 202-61-3-148.cable5.acsdata.co.nz +oiwg DKPQOU ABABc :Unknown\r\n")).
parse_line_4_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AB S x2.irc.dollyfish.net.nz 3 0 1164965565 P10 A0]]] +s :X2 Channel Service\r\n")).
parse_line_5_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC B #wow 1167179822 ACAE[\r\n")).
parse_line_6_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC B #blah 1164352204 +tn ABAFT,ACAKJ,ABAFQ,ACAJ9,ABAE7,ABAEp,ACAJH,ABAEf,ABABs:o,ABABc,A0AAA\r\n")).
parse_line_7_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC EB \r\n")).
parse_line_8_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC N shinsterw 1 1167197569 sian leibniz.catalyst.net.nz DKTvAH ACAE[ :shinster\r\n")).

parse_line_9_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":freenode-connect!freenode@freenode/bot/connect PRIVMSG nemerling :^AVERSION^A")).

to_list_join_test() ->
    ?assertMatch("JOIN #c1,#c2\r\n",
                 to_list(#irc_cmd{name=join,
                                  args=[{channels,
                                         ["#c1", "#c2"]}]})).
decode_ctcp_delims_test() ->
    ?assertMatch([{ctcp, "VERSION"}],
                 decode_ctcp_delims([1] ++ "VERSION" ++ [1])), 
    ?assertMatch([{non_ctcp, "This is a "}, {ctcp, "VERSION"}, {non_ctcp, "test."}],
                 decode_ctcp_delims("This is a " ++ [1] ++ "VERSION" ++ [1] ++ "test.")).

encode_ctcp_delims_test() ->
    ?assertMatch([1,$V,$E,$R,$S,$I,$O,$N,1],
                 encode_ctcp_delims([{ctcp, "VERSION"}])), 
    ?assertMatch("This is a " ++ [1] ++ "VERSION" ++ [1] ++ "test.",
                 encode_ctcp_delims([{non_ctcp, "This is a "}, {ctcp, "VERSION"}, {non_ctcp, "test."}])).
