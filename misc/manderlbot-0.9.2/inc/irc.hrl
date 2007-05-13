%%% File    : irc.hrl
%%% Author  : Dimitri Fontaine <tapoueh@free.fr>
%%% Purpose : Define some irc related data structures
%%% Created : 12 Nov 2002 by Dimitri Fontaine <dim@tuxfamily.org>

-author('dim@tuxfamily.org').

%% --- #tuxfamily ~fontaine dim.net1.nerim.net irc.localnet dim H :0 Dimitri

-record(user, {login,
	       from,
	       nick,
	       name}).



