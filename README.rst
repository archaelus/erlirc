=====================================================
ErlIRC - An Internet Relay Chat framework  for Erlang
=====================================================

ErlIRC is a framework for writing IRC clients, servers, proxies and
bots in Erlang.

Building ErlIRC
===============

* Download and install `Rebar3 <https://github.com/rebar/rebar3>`_.
* ``rebar3 compile``

Example Use
===========

IRC Server
----------

Run ``erl -name server -env ERL_LIBS _build/default/lib`` (or ``rebar3
shell``) and run the following commands in the erlang shell::

  (server@bete.ran)1> irc_app:start().
  <SASL reports snipped>
  (server@bete.ran)2> {ok, Server} = irc_sup:start_server("localnet","localhost").
  <SASL reports snipped>
  {ok,<0.54.0>}
  (server@bete.ran)3> irc_server:listen(Server, 16667).
  {ok,<0.56.0>}

Then from another bash console, telnet to the new server and send it
IRC commands::

  $ telnet localhost 16667
  NICK test
  USER test test localhost :Test User
  JOIN :#test

IRC Client
----------

Run ``erl -name client -env ERL_LIBS _build/default/lib`` and run the following commands in
the erlang shell::

  (client@bete.ran)1> {ok, C} = irc_client_fsm:start_link("erlirc","localhost", 16667).
  {ok,<0.61.0>}
  =INFO REPORT==== 7-Nov-2008::17:54:26 ===
  (<0.61.0> irc_client_fsm:149) :localhost 001 erlirc :Welcome to the Internet Relay Network erlirc!erlirc@127.0.0.1
  =INFO REPORT==== 7-Nov-2008::17:54:26 ===
  (<0.61.0> irc_client_fsm:157) :localhost 002 erlirc :Your host is localhost, running version erlirc-0.1
  =INFO REPORT==== 7-Nov-2008::17:54:26 ===
  (<0.61.0> irc_client_fsm:157) :localhost 003 erlirc :This server was created 2008-11-07T16:54:26+00
  =INFO REPORT==== 7-Nov-2008::17:54:26 ===
  (<0.61.0> irc_client_fsm:157) :localhost 004 erlirc localhost erlirc-0.1 aios biklImnoPstv
  =INFO REPORT==== 7-Nov-2008::17:54:26 ===
  (<0.61.0> irc_client_fsm:153) :localhost 422 erlirc :NOMOTD
  

Note that the erlirc server and client don't quite agree enough at the
moment to successfully join channels.
