%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_messages_tests).

-include_lib("irc.hrl").
-include_lib("logging.hrl").
-include_lib("eunit.hrl").

-import(irc_messages, [parse_line/1
                       ,to_list/1
                       ,decode_ctcp_delims/1
                       ,encode_ctcp_delims/1
                       ,unix_ts_to_datetime/1]).

irc_error_test() ->
    ?assertMatch(#irc_cmd{name=error},
                 parse_line("ERROR :Closing Link: erl.irc by ve.irc.dollyfish.net.nz (No C:line)\r\n")).

irc_server_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("SERVER ve.irc.dollyfish.net.nz 1 1164352162 1171089421 J10 ACAP] +h :ircd on ve\r\n")).
burst_server_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC S scorch.irc.dollyfish.net.nz 2 0 1164963563 P10 ABAP] +h :DollyNET ircd at irc.dollyfish.net.nz\r\n")).
burst_nick_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AB N Ned 2 1166709690 ~martyn 202-61-3-148.cable5.acsdata.co.nz +oiwg DKPQOU ABABc :Unknown\r\n")).
burst_service_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AB S x2.irc.dollyfish.net.nz 3 0 1164965565 P10 A0]]] +s :X2 Channel Service\r\n")).
burst_chang_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC B #wow 1167179822 ACAE[\r\n")).
burst_chan_2_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC B #blah 1164352204 +tn ABAFT,ACAKJ,ABAFQ,ACAJ9,ABAE7,ABAEp,ACAJH,ABAEf,ABABs:o,ABABc,A0AAA\r\n")).
burst_end_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC EB \r\n")).
burst_nick_2_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC N shinsterw 1 1167197569 sian leibniz.catalyst.net.nz DKTvAH ACAE[ :shinster\r\n")).

ctcp_version_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":freenode-connect!freenode@freenode/bot/connect PRIVMSG nemerling :^AVERSION^A")).
namreply_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":ve.irc.dollyfish.net.nz 353 nembot = #dullbots :nembot @nem\r\n")).

endofnames_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":ve.irc.dollyfish.net.nz 366 nembot #dullbots :End of /NAMES list.\r\n")).

topic_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":ve.irc.dollyfish.net.nz 332 nermerlin #dullbots :Foo.\r\n")).

topicinfo_test() ->
    X = parse_line(":ve.irc.dollyfish.net.nz 333 nermerlin #dullbots nem 1180326256\r\n"),
    ?assertMatch(#irc_cmd{},X),
    ?assertMatch(topicinfo, X#irc_cmd.name),
    ?assertMatch("#dullbots",
                 proplists:get_value(channel, X#irc_cmd.args)),
    ?assertMatch("nem",
                 proplists:get_value(topic_set_by, X#irc_cmd.args)),
    TS = unix_ts_to_datetime(1180326256),
    ?assertMatch(TS,
                 proplists:get_value(topic_set_at, X#irc_cmd.args)).

to_list_topicinfo_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":ve.irc.dollyfish.net.nz 333 nermerlin #dullbots nem 1180326256\r\n")).

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

nick_reply_test() ->
    ?assertMatch(#irc_cmd{name=nick, args=[{name, "nemor"}]},
                 parse_line(":nemerlng!nemerlng@121-73-3-252.cable.telstraclear.net NICK :nemor\r\n")).

user_test() ->
    Cmd = parse_line("USER nem nem localhost :Geoff Cant\r\n"),
    ?assertMatch(#irc_cmd{name=user}, Cmd),
    ?assertMatch("nem", proplists:get_value(user_name,Cmd#irc_cmd.args)),
    ?assertMatch("Geoff Cant", proplists:get_value(real_name,Cmd#irc_cmd.args)).

user_to_list_test() ->
    ?assertMatch("nem!nem@localhost", to_list(#user{nick="nem",name="nem",host="localhost"})).

pingpong_test() ->
    ?assertMatch(#irc_cmd{name=ping,args=[{servers, {"localhost", []}}]},
                 parse_line("PING localhost\r\n")),
    ?assertMatch(#irc_cmd{name=ping,args=[{servers, {"localhost", "foobar"}}]},
                 parse_line("PING localhost foobar\r\n")),
    ?assertMatch(#irc_cmd{name=pong,args=[{servers, {"localhost", []}}]},
                 parse_line("PONG localhost\r\n")),
    ?assertMatch(#irc_cmd{name=pong,args=[{servers, {"localhost", "foobar"}}]},
                 parse_line("PONG localhost foobar\r\n")).

pingpong_gen_test() ->
    ?assertMatch("PING localhost\r\n",
                 to_list(parse_line("PING localhost\r\n"))),
    ?assertMatch("PING localhost foobar\r\n",
                 to_list(parse_line("PING localhost foobar\r\n"))),
    ?assertMatch("PONG localhost\r\n",
                 to_list(parse_line("PONG localhost\r\n"))),
    ?assertMatch("PONG localhost foobar\r\n",
                 to_list(parse_line("PONG localhost foobar\r\n"))).
    

quit_test() ->
    ?assertMatch(#irc_cmd{name=quit},
                 parse_line("QUIT\r\n")),
    ?assertMatch(#irc_cmd{name=quit, args=[{message, "Foo"}]},
                 parse_line("QUIT :Foo\r\n")),
    ?assertMatch("QUIT\r\n",
                 to_list(parse_line("QUIT\r\n"))),
    ?assertMatch("QUIT :Foo\r\n",
                 to_list(parse_line("QUIT :Foo\r\n"))),
    ?assertMatch("ERROR :Foo\r\n",
                 to_list((parse_line("QUIT :Foo\r\n"))#irc_cmd{name=error})).

nomotd_to_list_test() ->
    ?assertMatch(":localhost 422 nem :NOMOTD\r\n",
                 to_list(#irc_cmd{source=#irc_server{host="localhost"},
                                  target=#user{nick="nem"},
                                  name=nomotd,
                                  args=[]})),
    ?assertMatch(":localhost 422 nem :No MOTD\r\n",
                 to_list(#irc_cmd{source=#irc_server{host="localhost"},
                                  target=#user{nick="nem"},
                                  name=nomotd,
                                  args=[{message, "No MOTD"}]})).
    

%numreply_test() ->
%    ?assertMatch(Num when Num > 0, string:str(to_list(#irc_cmd{name=notregistered,}))).
