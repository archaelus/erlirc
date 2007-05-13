%% Debugging macro
%% If the macro debug is defined before including this file,
%% the debug print-outs are enabled.

-define(debug, true).

-ifdef(debug).
-define(dbg(Fmt, Args), ok=io:format("~p: " ++ Fmt ++ "~n", [?LINE|Args])).
-define(trace, ok=io:format("<<~p:~p>>~n", [?MODULE, ?LINE])).
-else.
-define(dbg(Fmt, Args), no_debug).
-define(trace, no_debug).
-endif.
