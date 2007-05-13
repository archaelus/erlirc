-module(gen_irc_codes).
-author("Julian Fondren <ayrnieu@gmail.com>").
-copyright("Copyright (C) 2006 Julian Fondren").
-license('Artistic').
-export([error/1, reply/1]).

% 6.1 error replies
error(401) -> 'no such nick';
error(402) -> 'no such server';
error(403) -> 'no such channel';
error(404) -> 'cannot send to channel'; % ERR_CANNOTSENDTOCHAN
error(405) -> 'too many channels';
error(406) -> 'was no such nick';
error(407) -> 'too many target';
error(409) -> 'no origin';
error(411) -> 'no recipient';
error(412) -> 'no text to send';
error(413) -> 'no toplevel';
error(414) -> 'wild toplevel';
error(421) -> 'unknown command';
error(422) -> 'no motd';
error(423) -> 'no admin info';
error(424) -> 'file error';
error(431) -> 'no nickname given';
error(432) -> 'erroneous nickname';
error(433) -> 'nickname in use';
error(436) -> 'nick collision';
error(441) -> 'user not in channel';
error(442) -> 'not on channel';
error(443) -> 'user on channel';
error(444) -> 'no login';
error(445) -> 'summon disabled';
error(446) -> 'users disabled';
error(451) -> 'not registered';
error(461) -> 'need more params';
error(462) -> 'already registered';
error(463) -> 'no perm for host';
error(464) -> 'password mismatch'; % ERR_PASSWDMISMATCH
error(465) -> 'you\'re banned, creep'; % ERR_YOUREBANNEDCREEP
error(467) -> 'key set';
error(471) -> 'channel is full';
error(472) -> 'unknown mode';
error(473) -> 'invite only channel'; % ERR_INVITEONLYCHAN
error(474) -> 'banned from channel'; % ERR_BANNEDFROMCHAN
error(475) -> 'bad channel key';
error(481) -> 'no privileges';
error(482) -> 'chanop privileges needed'; % ERR_CHANOPPRIVSNEEDED
error(483) -> 'can\'t kill server'; % ERR_CANTKILLSERVER
error(491) -> 'no oper host';
error(501) -> 'unknown usermode flag'; % ERR_UMODEUNKNOWNFLAG
error(502) -> 'users don\'t match'; % ERR_USERSDONTMATCH

% 6.3 reserved numerics (some of these under reply/1 below)
error(466) -> 'you will be banned';
error(492) -> 'no service host';
error(476) -> 'bad chan mask'. % ERR_BADCHANMASK

% 6.2 command responses
reply(300) -> none;
reply(302) -> userhost;
reply(303) -> 'is on';
reply(301) -> away;
reply(305) -> unaway;
reply(306) -> 'now away';
reply(311) -> {whois, user};
reply(312) -> {whois, server};
reply(313) -> {whois, operator};
reply(317) -> {whois, idle};
reply(318) -> {whois, 'end'}; % RPL_ENDOFWHOIS
reply(319) -> {whois, channels};
reply(314) -> {whowas, user};
reply(369) -> {whowas, 'end'}; % RPL_ENDOFWHOWAS
reply(321) -> {list, start};
reply(322) -> {list, item}; % RPL_LIST
reply(323) -> {list, 'end'};
reply(324) -> 'channel mode is';
reply(331) -> 'no topic';
reply(332) -> topic;
reply(341) -> inviting;
reply(342) -> summoning;
reply(351) -> version;
reply(352) -> {who, item}; % RPL_WHOREPLY
reply(315) -> {who, 'end'}; % RPL_ENDOFWHO
reply(353) -> {names, item}; % RPL_NAMREPLY
reply(366) -> {names, 'end'}; % RPL_ENDOFNAMES
reply(364) -> {links, item}; % RPL_LINKS
reply(365) -> {links, 'end'}; % RPL_ENDOFLINKS
reply(367) -> {banlist, item}; % RPL_BANLIST
reply(368) -> {banlist, 'end'}; % RPL_ENDOFBANLIST
reply(371) -> {info, item}; % RPL_INFO
reply(374) -> {info, 'end'}; % RPL_ENDOFINFO
reply(375) -> {motd, start}; % RPL_MOTDSTART
reply(372) -> {motd, item}; % RPL_MOTD
reply(376) -> {motd, 'end'}; % RPL_ENDOFMOTD
reply(381) -> 'you\'re oper'; % RPL_YOUREOPER ... I don't like this either.
reply(382) -> rehashing;
reply(391) -> time;
reply(392) -> {users, start};
reply(393) -> {users, item}; % RPL_USERS
reply(394) -> {users, 'end'}; % RPL_ENDOFUSERS
reply(395) -> 'no users';
reply(200) -> {trace, link};
reply(201) -> {trace, connecting};
reply(202) -> {trace, handshake};
reply(203) -> {trace, unknown};
reply(204) -> {trace, operator};
reply(205) -> {trace, user};
reply(206) -> {trace, server};
reply(208) -> {trace, 'new type'};
reply(261) -> {trace, log};
reply(211) -> {stats, 'link info'};
reply(212) -> {stats, commands};
reply(213) -> {stats, 'c line'};
reply(214) -> {stats, 'n line'};
reply(215) -> {stats, 'i line'};
reply(216) -> {stats, 'k line'};
reply(218) -> {stats, 'y line'};
reply(219) -> {stats, 'end'}; % RPL_ENDOFSTATS
reply(241) -> {stats, 'l line'};
reply(242) -> {stats, uptime};
reply(243) -> {stats, 'o line'};
reply(244) -> {stats, 'h line'};
reply(221) -> 'user mode is'; % RPL_UMODEIS
reply(251) -> {luser, client}; % ... 'luser'?
reply(252) -> {luser, op};
reply(253) -> {luser, unknown};
reply(254) -> {luser, channels};
reply(255) -> {luser, me};
reply(256) -> {admin, me};
reply(257) -> {admin, loc1}; % city, state, country (locality?)
reply(258) -> {admin, loc2}; % university, deparntment (subordination?)
reply(259) -> {admin, email};

% 6.3 reserved numerics (some of these under error/1 above)
reply(209) -> {trace, class};
reply(231) -> {services, info};
reply(233) -> {services, item}; % RPL_SERVICE
reply(235) -> {'server list', 'end'}; % RPL_SERVLISTEND
reply(316) -> {who, 'is chanop'}; % RPL_WHOISCHANOP
reply(362) -> closing; % {closing, item} ?
reply(373) -> {info, start};
reply(217) -> {stats, 'q line'};
reply(232) -> {services, 'end'}; % RPL_ENDOFSERVICES
reply(234) -> {'server list', item}; % RPL_SERVLIST
reply(361) -> 'kill done';
reply(363) -> 'close end'; % {closing, 'end'} ?
reply(384) -> 'my port is'.
