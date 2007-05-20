%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_commands).

%% API
-export([from_list/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () ->
%% @doc 
%% @end 

from_list("AC") -> account;
from_list("ACCOUNT") -> account;
from_list("AD") -> admin;
from_list("ADMIN") -> admin;
from_list("LL") -> asll;
from_list("ASLL") -> asll;
from_list("A") -> away;
from_list("AWAY") -> away;
from_list("B") -> burst;
from_list("BURST") -> burst;
from_list("CM") -> clearmode;
from_list("CLEARMODE") -> clearmode;
from_list("CLOSE") -> close;
from_list("CN") -> cnotice;
from_list("CNOTICE") -> cnotice;
from_list("CO") -> connect;
from_list("CONNECT") -> connect;
from_list("CP") -> cprivmsg;
from_list("CPRIVMSG") -> cprivmsg;
from_list("C") -> create;
from_list("CREATE") -> create;
from_list("DE") -> destruct;
from_list("DESTRUCT") -> destruct;
from_list("DS") -> desynch;
from_list("DESYNCH") -> desynch;
from_list("DIE") -> die;
from_list("DNS") -> dns;
from_list("EB") -> end_of_burst;
from_list("EA") -> end_of_burst_ack;
from_list("ACK") -> ack;
from_list("Y") -> error;
from_list("ERROR") -> error;
from_list("GET") -> get;
from_list("GL") -> gline;
from_list("GLINE") -> gline;
from_list("HASH") -> hash;
from_list("HELP") -> help;
from_list("F") -> info;
from_list("INFO") -> info;
from_list("I") -> invite;
from_list("INVITE") -> invite;
from_list("ISON") -> ison;
from_list("J") -> join;
from_list("JOIN") -> join;
from_list("JU") -> jupe;
from_list("JUPE") -> jupe;
from_list("K") -> kick;
from_list("KICK") -> kick;
from_list("D") -> kill;
from_list("KILL") -> kill;
from_list("LI") -> links;
from_list("LINKS") -> links;
from_list("LIST") -> list;
from_list("LU") -> lusers;
from_list("LUSERS") -> lusers;
from_list("MAP") -> map;
from_list("M") -> mode;
from_list("MODE") -> mode;
from_list("MO") -> motd;
from_list("MOTD") -> motd;
from_list("E") -> names;
from_list("NAMES") -> names;
from_list("N") -> nick;
from_list("NICK") -> nick;
from_list("O") -> notice;
from_list("NOTICE") -> notice;
from_list("OPER") -> oper;
from_list("OM") -> opmode;
from_list("OPMODE") -> opmode;
from_list("L") -> part;
from_list("PART") -> part;
from_list("PA") -> pass;
from_list("PASS") -> pass;
from_list("G") -> ping;
from_list("PING") -> ping;
from_list("Z") -> pong;
from_list("PONG") -> pong;
from_list("POST") -> post;
from_list("P") -> privmsg;
from_list("PRIVMSG") -> privmsg;
from_list("PRIVS") -> privs;
from_list("PROTO") -> proto;
from_list("Q") -> quit;
from_list("QUIT") -> quit;
from_list("REHASH") -> rehash;
from_list("RESET") -> reset;
from_list("RESTART") -> restart;
from_list("RI") -> rping;
from_list("RPING") -> rping;
from_list("RO") -> rpong;
from_list("RPONG") -> rpong;
from_list("S") -> server;
from_list("SERVER") -> server;
from_list("SET") -> set;
from_list("SE") -> settime;
from_list("SETTIME") -> settime;
from_list("U") -> silence;
from_list("SILENCE") -> silence;
from_list("SQ") -> squit;
from_list("SQUIT") -> squit;
from_list("R") -> stats;
from_list("STATS") -> stats;
from_list("TI") -> time;
from_list("TIME") -> time;
from_list("T") -> topic;
from_list("TOPIC") -> topic;
from_list("TR") -> trace;
from_list("TRACE") -> trace;
from_list("UP") -> uping;
from_list("UPING") -> uping;
from_list("USER") -> user;
from_list("USERHOST") -> userhost;
from_list("USERIP") -> userip;
from_list("V") -> version;
from_list("VERSION") -> version;
from_list("WC") -> wallchops;
from_list("WALLCHOPS") -> wallchops;
from_list("WA") -> wallops;
from_list("WALLOPS") -> wallops;
from_list("WU") -> wallusers;
from_list("WALLUSERS") -> wallusers;
from_list("WV") -> wallvoices;
from_list("WALLVOICES") -> wallvoices;
from_list("H") -> who;
from_list("WHO") -> who;
from_list("W") -> whois;
from_list("WHOIS") -> whois;
from_list("X") -> whowas;
from_list("WHOWAS") -> whowas;
from_list("SN") -> svsnick;
from_list("SVSNICK") -> svsnick;
from_list("SJ") -> svsjoin;
from_list("SVSJOIN") -> svsjoin;
from_list("SERVICE") -> service;
from_list("NJOIN") -> njoin;
from_list("SERVLIST") -> servlist;
from_list("SQUERY") -> squery;
from_list(_) -> invalid_command.

%%====================================================================
%% Internal functions
%%====================================================================
