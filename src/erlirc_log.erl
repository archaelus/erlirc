%%% File    : erlirc_log.erl
%%% Author  : Geoff Cant <geoff@catalyst.net.nz>
%%% Description : Logging API for ERLIRC.
%%% Created :  5 Jun 2006 by Geoff Cant <geoff@catalyst.net.nz>


-module(erlirc_log).

-export([error/5, warn/5, info/5]).

error(Module, LineNo, Pid, Format, Data) ->
    error_logger:error_msg(lists:append("(~p ~p:~p) ", Format), [Pid, Module, LineNo|Data]).
%    erlirc_logger:log(error, Module, LineNo, Pid, Format, Data).

warn(Module, LineNo, Pid, Format, Data) ->
    error_logger:warning_msg(lists:append("(~p ~p:~p) ", Format), [Pid, Module, LineNo|Data]).
%    erlirc_logger:log(warning, Module, LineNo, Pid, Format, Data).

%info(Module, _, _, _, _) when Module == erlirc_esme_connection; Module == erlirc_http_connection; Module == erlirc_debug_connection; Module == erlirc_shortcode -> 
%    ok;
info(Module, LineNo, Pid, Format, Data) ->
    error_logger:info_msg(lists:append("(~p ~p:~p) ", Format), [Pid, Module, LineNo|Data]).
%    erlirc_logger:log(info, Module, LineNo, Pid, Format, Data).
