%%% File    : erlirc_logging.hrl
%%% Author  : Geoff Cant <nem@lisp.geek.nz>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(erlirc_logging).
-define(erlirc_logging, true).

-define(INFO(Format, Args),
        erlirc_log:info(?MODULE, ?LINE, self(), Format, Args)).
-define(WARN(Format, Args),
        erlirc_log:warn(?MODULE, ?LINE, self(), Format, Args)).
-define(ERR(Format, Args),
        erlirc_log:error(?MODULE, ?LINE, self(), Format, Args)).

-endif. %erlirc_logging
