{application, erlirc,
 [{description, "Erlang IRC Library"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, sasl]},
  {mod, {irc_app, []}},
  {modules, [erlirc_log
             ,gen_irc
             ,gen_leader
             ,gproc
             ,irc_app
             ,irc_channel
             ,irc_channel_tests
             ,irc_cmd
             ,irc_commands
             ,irc_connection
             ,irc_messages
             ,irc_messages_tests
             ,irc_numerics
             ,irc_parser
             ,irc_s2c_fsm
             ,irc_server
             ,irc_sup
             ,irc_user
             ,tcp_server
            ]},
  {registered, [irc_sup]}
 ]}.
