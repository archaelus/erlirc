%%% File    : irc.hrl
%%% Author  : Geoff Cant <nem@erlang.geek.nz>
%%% Purpose : Define some irc related data structures
%%% Created : 11 Feb 2007 by Geoff Cant <nem@erlang.geek.nz>

-author('nem@erlang.geek.nz').

-define(ERLIRC_VERSION, "erlirc-0.1").

-record(user, {nick = "missingnick",
               nick_ts,
               name = "missingname",
               realname = "missingrealname",
               host = "missinghost",
               mode,
               description,
               info,
               net}).

-record(irc_cmd, {raw,
                  source,
                  target,
                  name,
                  args = [],
                  ctcp}).

-record(p10user, {numeric,
                  nick,
                  nick_ts,
                  user,
                  host,
                  authname,
                  mode,
                  description}).

-record(p10server, {numeric,
                    name,
                    hopcount,
                    boot_ts,
                    link_ts,
                    protocol,
                    max_client,
                    flags,
                    description}).

-record(irc_server, {host,
                     net}).

-record(topic, {text,
                topic_ts,
                author}).

-record(chan, {numeric,
               name,
               chan_ts,
               mode,
               topic = #topic{},
               type = public,
               members = [],
               info}).


-record(ctcp_cmd, {name,
                   args = []}).

-record(gircd_state, {nicks,
                      channels}).
