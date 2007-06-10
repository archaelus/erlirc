%%% File    : irc.hrl
%%% Author  : Geoff Cant <nem@erlang.geek.nz>
%%% Purpose : Define some irc related data structures
%%% Created : 11 Feb 2007 by Geoff Cant <nem@erlang.geek.nz>

-author('nem@erlang.geek.nz').

-record(user, {numeric,
               nick,
               nick_ts,
               user,
               host,
               authname,
               mode,
               description}).

-record(irc_cmd, {raw,
                  source,
                  target,
                  name,
                  args = [],
                  ctcp}).

-record(p10server, {numeric,
                    name,
                    hopcount,
                    boot_ts,
                    link_ts,
                    protocol,
                    max_client,
                    flags,
                    description}).

-record(server, {host}).

-record(chan, {numeric,
               name,
               chan_ts,
               mode,
               topic,
               type = public,
               members = []}).

-record(ctcp_cmd, {name,
                   args = []}).
