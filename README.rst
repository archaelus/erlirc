=====================================================
ErlIRC - An Internet Relay Chat framework  for Erlang
=====================================================

ErlIRC is a framework for writing IRC clients, servers, proxies and
bots in Erlang.

Example Use
===========

Run ``erl -name erlirc -pa ebin`` and run the following commands::

  (erlirc@bete.ran)1> irc_app:start().
  <SASL reports snipped>
  (erlirc@bete.ran)2> {ok, Server} = irc_sup:start_server("localnet","localhost").
  <SASL reports snipped>
  {ok,<0.54.0>}
  (erlirc@bete.ran)3> irc_server:listen(Server, 16667).
  {ok,<0.56.0>}

Then from another bash console, telnet to the new server and send it
IRC commands::

  telnet localhost 16667
  NICK test
  USER test test localhost :Test User
  JOIN :#test