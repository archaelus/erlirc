%%%----------------------------------------------------------------------
%%% File    : config.hrl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Define some config element as erlang structures
%%% Created : 19 Feb 2002 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-author('dim@tuxfamily.org').

-define(arg_conffile, conf).
-define(arg_logfile,  log).

-record(config,	{name,                          % the name of the bot
		 controler=[],                  % the nick of the one wich
						% controls the bot from irc

		 dict={"localhost", "2628", "wn"},
		 servers=[],
		 behaviours=[]
		}).

-record(server, {host,
		 port,
		 passwd,
		 channels = []
		}).

-record(channel, {name,
		  botname,
		  behaviours = []
		 }).
		  

%% The behavior as found in the config file
%% With some patterns and some exclude patterns
-record(cfg_behaviour, {name,
			action,

			from,
			to,
			op,
			option,
			pattern,

			exl_from,
			exl_to,
			exl_op,
			exl_option,
			exl_pattern,

		       data = []
		      }).


