%%% File    : irc_parser.erl
%%% Author  : Geoff Cant <nem@lisp.geek.nz>
%%% Description : 
%%% Created : 27 Mar 2006 by Geoff Cant <nem@lisp.geek.nz>

-module(irc_parser).

-include_lib("irc.hrl").
-include_lib("logging.hrl").
-include_lib("eunit.hrl").

-compile(export_all).

-define(IS_DIGIT(Var), $0 =< Var, Var =< $9).

parse_line(Line) ->
    parse_line(Line, #cmd{raw=Line}).

parse_line([A,B,C,D,E,$\s|Rest], Cmd) ->
    case irc_numeric:p10b64_to_int([A,B,C,D,E]) of 
        Num when is_integer(Num) ->
            parse_command_part(Rest, Cmd#cmd{source=#user{numeric=Num}});
        {error, Reason} ->
            ?ERR("Need to fix this code.", []),
            erlang:error(Reason)
    end;
parse_line([A,B,$\s|Rest], Cmd) when ((A == $[) or (A == $]) or
                                      (($0 =< A) and (A =< $9)) or
                                      (($a =< A) and (A =< $z)) or
                                      (($A =< A) and (A =< $Z))),
                                     ((B == $[) or (B == $]) or
                                      (($0 =< B) and (B =< $9)) or
                                      (($a =< B) and (B =< $z)) or
                                      (($A =< B) and (B =< $Z))) ->
    parse_command_part(Rest, Cmd#cmd{source=#p10server{numeric=irc_numerics:p10b64_to_int([A,B])}});
parse_line([$:|Rest], Cmd) ->
    parse_prefix_part(Rest, Cmd);
parse_line(Line, Cmd) ->
    parse_command_part(Line, Cmd).

parse_prefix_part(Line, Cmd) ->
    {Prefix, Rest} = split(Line),
    CmdWithUser = parse_prefix(Prefix, Cmd#cmd{source=#user{}}),
    parse_command_part(Rest, CmdWithUser).

parse_prefix(Prefix, #cmd{source=User} = Cmd) ->
    case string:tokens(Prefix, "@") of
        [Nick] -> 
            case lists:member($., Prefix) of
                true ->
                    Cmd#cmd{source=#server{host=Nick}};
                false ->
                    Cmd#cmd{source=User#user{nick=Nick}}
            end;
        [Nick, HostSpec] -> 
            case string:tokens(HostSpec, "!") of
                [User, Host] ->
                    Cmd#cmd{source=User#user{nick=Nick, user=User, host=Host}};
                [Host] ->
                    Cmd#cmd{source=User#user{nick=Nick, host=Host}}
            end
    end.

parse_command_part([D1,D2,D3,$\s|Rest], Cmd) when ?IS_DIGIT(D1),
                                              ?IS_DIGIT(D2),
                                              ?IS_DIGIT(D3) ->
    Cmd#cmd{name=irc_numerics:numeric_to_atom([D1,D2,D3]),
            args=nonl(Rest)};
parse_command_part([D1,D2,D3|Rest], Cmd) when ?IS_DIGIT(D1),
                                              ?IS_DIGIT(D2),
                                              ?IS_DIGIT(D3) ->
    Cmd#cmd{name=irc_numerics:numeric_to_atom([D1,D2,D3]),
            args=nonl(Rest)};
parse_command_part(Rest, Cmd) ->
    {CommandName, Args} = split(Rest),
    Cmd#cmd{name=irc_commands:from_list(CommandName),
            args=nonl(Args)}.

split(Line) ->
    split($\s, Line).

split(Char, Line) when is_integer(Char) ->
    split(fun (X) when X == Char -> false; (_) -> true end, Line);
split(Fun, Line) when is_function(Fun) ->
    {First, Second} = lists:splitwith(Fun, Line),
    {First, tl(Second)}.
    
split_test() ->
    ?assertMatch({"this", "is a test"}, split("this is a test")).

is_digit(D) when ?IS_DIGIT(D) ->
    true;
is_digit(_) -> false.

nonl([$\r,$\n]) -> [];
nonl([$\n]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].
