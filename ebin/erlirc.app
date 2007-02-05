{application, erlirc,
 [{description, "Erlang IRC Library"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, sasl]},
  {mod, {irc_app, []}},
  {modules, [irc_app, irc_sup]},
  {registered, [irc_sup]}
 ]}.
