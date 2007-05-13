# Author: Chris "BinGOs" Williams
# Derived from code by Dennis Taylor
#
# This module may be used, modified, and distributed under the same
# terms as Perl itself. Please see the license that came with your Perl
# distribution for details.
#

package POE::Component::IRC::Service::P10;

use strict;
use POE qw( Wheel::SocketFactory Wheel::ReadWrite Driver::SysRW
            Filter::Line Filter::Stream );
use POE::Filter::IRC::P10;
use POE::Filter::CTCP::P10;
use Carp;
use Socket;
use Sys::Hostname;
use Time::HiRes qw (gettimeofday);
use vars qw($VERSION);

$VERSION = '0.9';

my %cmd2token = ('ACCOUNT' => 'AC', 'ADMIN' => 'AD', 'ASLL' => 'LL', 'AWAY' => 'A', 'BURST' => 'B', 'CLEARMODE' => 'CM', 'CLOSE' => 'CLOSE', 'CNOTICE' => 'CN', 'CONNECT' => 'CO', 'CPRIVMSG' => 'CP', 'CREATE' => 'C', 'DESTRUCT' => 'DE', 'DESYNCH' => 'DS', 'DIE' => 'DIE', 'DNS' => 'DNS', 'END_OF_BURST' => 'EB', 'EOB_ACK'  => 'EA', 'ERROR' => 'Y', 'GET' => 'GET', 'GLINE' => 'GL', 'HASH' => 'HASH', 'HELP' => 'HELP', 'INFO' => 'F', 'INVITE' => 'I', 'ISON' => 'ISON', 'JOIN' => 'J', 'JUPE' => 'JU', 'KICK' => 'K', 'KILL' => 'D', 'LINKS' => 'LI', 'LIST' => 'LIST', 'LUSERS' => 'LU', 'MAP' => 'MAP', 'MODE' => 'M', 'MOTD' => 'MO', 'NAMES' => 'E', 'NICK' => 'N', 'NOTICE' => 'O', 'OPER' => 'OPER', 'OPMODE' => 'OM', 'PART' => 'L', 'PASS' => 'PA', 'PING' => 'G', 'PONG' => 'Z', 'POST' => 'POST', 'PRIVMSG' => 'P', 'PRIVS' => 'PRIVS', 'PROTO' => 'PROTO', 'QUIT' => 'Q', 'REHASH' => 'REHASH', 'RESET' => 'RESET', 'RESTART' => 'RESTART', 'RPING' => 'RI', 'RPONG' => 'RO', 'SERVER' => 'S', 'SET' => 'SET', 'SETTIME' => 'SE', 'SILENCE' => 'U', 'SQUIT' => 'SQ', 'STATS' => 'R', 'TIME' => 'TI', 'TOPIC' => 'T', 'TRACE' => 'TR', 'UPING' => 'UP', 'USER' => 'USER', 'USERHOST' => 'USERHOST', 'USERIP' => 'USERIP', 'VERSION' => 'V', 'WALLCHOPS' => 'WC', 'WALLOPS' => 'WA', 'WALLUSERS' => 'WU', 'WALLVOICES' => 'WV', 'WHO' => 'H', 'WHOIS' => 'W', 'WHOWAS' => 'X');

use constant PCI_REFCOUNT_TAG => "P::C::I registered";

use constant CMD_PRI => 0;
use constant CMD_SUB => 1;

my %irc_commands =
   ('quit'      => \&oneoptarg_client,
    'nick'      => \&onlyonearg_client,
    'invite'    => \&onlytwoargs_client,
    'kill'      => \&onlytwoargs,
    'account'   => \&onlytwoargs,
    'clearmode' => \&onlytwoargs,
    'opmode'    => \&spacesep,
    'gline'     => \&spacesep,
    'jupe'      => \&spacesep,
    'privmsg'   => \&privandnotice,
    'notice'    => \&privandnotice,
    'stats'     => \&spacesep_client,
    'links'     => \&spacesep_client,
    'mode'      => \&spacesep_client,
    'part'      => \&commasep_client,
    'ctcp'      => \&ctcp,
    'ctcpreply' => \&ctcp,
  );

# Create a new IRC Service

sub new {
  my ($package,$alias,$hash) = splice @_, 0, 3;
  my ($package_events);

  unless ($alias and $hash) {
        croak "Not enough parameters to POE::Component::IRC::Service::P10::new()";
  }

  unless (ref $hash eq 'HASH') {
        croak "Second argument to POE::Component::IRC::Service::P10::new() must be a hash reference";
  }
  
  $hash->{EventMode} = 1 unless ( defined ( $hash->{EventMode} ) and $hash->{EventMode} == 0 );

  $hash->{Reconnect} = 0 unless ( defined ( $hash->{Reconnect} ) and $hash->{Reconnect} == 1 );

  $hash->{Debug} = 0 unless ( defined ( $hash->{Debug} ) and $hash->{Debug} == 1 );

  if ( $hash->{EventMode} == 1 ) {
    $package_events = [qw( _start
                           _stop
                           _parseline
		           _sock_up
		           _sock_down
		           _sock_failed
			   autoping
                           addnick
                           connect
                           topic
			   irc_p10_disconnected
			   irc_p10_socketerr
		           irc_p10_stats
			   irc_p10_version
		           irc_p10_server_link
			   irc_p10_server
			   irc_p10_squit
			   irc_p10_burst
		           irc_p10_end_of_burst
			   irc_p10_eob_ack
                           irc_p10_ping
		           irc_p10_quit
		           irc_p10_kill
			   irc_p10_nick
			   irc_p10_whois
			   irc_p10_account
			   irc_p10_create
			   irc_p10_join
			   irc_p10_part
			   irc_p10_kick
			   irc_p10_mode
			   irc_p10_opmode
			   irc_p10_clearmode
                           kick
			   join
		           register
		           sl_server
		           sl_client
                           shutdown
		           squit
		           unregister)];
  } else {
    $package_events = [qw( _start
                           _stop
                           _parseline
		           _sock_up
		           _sock_down
		           _sock_failed
			   autoping
                           addnick
                           connect
                           topic
			   irc_p10_disconnected
			   irc_p10_socketerr
		           irc_p10_stats
			   irc_p10_version
		           irc_p10_server_link
			   irc_p10_server
			   irc_p10_squit
		           irc_p10_end_of_burst
			   irc_p10_eob_ack
                           irc_p10_ping
		           irc_p10_quit
		           irc_p10_kill
			   irc_p10_nick
			   irc_p10_whois
			   irc_p10_account
			   irc_p10_mode
                           kick
			   join
		           register
		           sl_server
		           sl_client
                           shutdown
		           squit
		           unregister)];
  }

  # Create our object 
  my ($self) = { };
  bless ($self);

  # Parse the passed hash reference
  unless ($hash->{'ServerNumeric'} and $hash->{'ServerName'} and $hash->{'RemoteServer'} and $hash->{'Password'} and $hash->{'ServerPort'}) {
	croak "You must specify ServerNumeric, ServerName, RemoteServer, Password and ServerPort in your hash reference.";
  }

  # Is numeric in proper format ie. P10 Base64 if not convert it
  if ($hash->{'ServerNumeric'} =~ /^[0-9]+?$/) {
      if ($hash->{'ServerNumeric'} < 0 or $hash->{'ServerNumeric'} > 4095) {
        die "ServerNumeric must be either 0-4095 or a valid P10 Base64 numeric\n";
      } else {
        $hash->{'ServerNumeric'} = dectobase64($hash->{'ServerNumeric'});
      }
  }

  if ($hash->{'ServerNumeric'} !~ /^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\[\]]{2}$/) {
        die "ServerNumeric must be either 0-4095 or a valid P10 Base64 numeric\n";
  }  

  $hash->{ServerDesc} = "*** POE::Component::IRC::Service ***" unless defined ($hash->{ServerDesc});
  $hash->{Version} = "POE-Component-IRC-Service-P10-$VERSION" unless defined ($hash->{Version});

  my @event_map = map {($_, $irc_commands{$_})} keys %irc_commands;

  $hash->{'PingFreq'} = 90 unless ( defined ( $hash->{'PingFreq'} ) );

  POE::Session->create( inline_states => { @event_map },
			package_states => [
                        $package => $package_events, ],
                     	args => [ $alias, @_ ],
			heap => { State => $self, 
				  servernum => $hash->{'ServerNumeric'},
				  servername => $hash->{'ServerName'},
				  serverdesc => $hash->{'ServerDesc'},
				  remoteserver => $hash->{'RemoteServer'},
				  serverport => $hash->{'ServerPort'},
				  password => $hash->{'Password'},
				  localaddr => $hash->{'LocalAddr'},
				  pingfreq => $hash->{'PingFreq'},
				  eventmode => $hash->{'EventMode'},
				  reconnect => $hash->{'Reconnect'},
				  debug => $hash->{'Debug'},
				  his_servername => $hash->{'HIS_SERVERNAME'},
				  his_serverinfo => $hash->{'HIS_SERVERINFO'},
				  version => $hash->{'Version'}, },
		      );
   return $self;
}

# Register and unregister to receive events

sub register {
  my ($kernel, $heap, $session, $sender, @events) =
    @_[KERNEL, HEAP, SESSION, SENDER, ARG0 .. $#_];

  die "Not enough arguments" unless @events;

  # FIXME: What "special" event names go here? (ie, "errors")
  # basic, dcc (implies ctcp), ctcp, oper ...what other categories?
  foreach (@events) {
    $_ = "irc_p10_" . $_ unless /^_/;
    $heap->{events}->{$_}->{$sender} = $sender;
    $heap->{sessions}->{$sender}->{'ref'} = $sender;
    unless ($heap->{sessions}->{$sender}->{refcnt}++ or $session == $sender) {
      $kernel->refcount_increment($sender->ID(), PCI_REFCOUNT_TAG);
    }
  }
}

sub unregister {
  my ($kernel, $heap, $session, $sender, @events) =
    @_[KERNEL,  HEAP, SESSION,  SENDER,  ARG0 .. $#_];

  die "Not enough arguments" unless @events;

  foreach (@events) {
    delete $heap->{events}->{$_}->{$sender};
    if (--$heap->{sessions}->{$sender}->{refcnt} <= 0) {
      delete $heap->{sessions}->{$sender};
      unless ($session == $sender) {
        $kernel->refcount_decrement($sender->ID(), PCI_REFCOUNT_TAG);
      }
    }
  }
}

# Session starts or stops

sub _start {
  my ($kernel, $session, $heap, $alias) = @_[KERNEL, SESSION, HEAP, ARG0];
  my @options = @_[ARG1 .. $#_];

  $session->option( @options ) if @options;
  $kernel->alias_set($alias);
  $kernel->yield( 'register', qw(stats server_link server squit burst end_of_burst eob_ack ping quit kill nick whois account create join part kick mode opmode clearmode version disconnected socketerr) );
  $heap->{irc_filter} = POE::Filter::IRC::P10->new();
  $heap->{ctcp_filter} = POE::Filter::CTCP::P10->new();
  $heap->{irc_filter}->debug(1) if ( $heap->{debug} );
  $heap->{nextnick} = 0;
  $heap->{connected} = 0;
  $heap->{serverlink} = "";
  $heap->{starttime} = time();
}

sub _stop {
  my ($kernel, $heap, $quitmsg) = @_[KERNEL, HEAP, ARG0];

  if ($heap->{connected}) {
    $kernel->call( $_[SESSION], 'shutdown', $quitmsg );
  }
}

# Disconnect the IRC Service from IRC Network

sub squit {
  my ($kernel, $heap) = @_[KERNEL,HEAP];

  # Don't give a f**k about any parameters passed

  if ( $heap->{'socket'} ) {
    delete ( $heap->{'socket'} );
  }
#  $kernel->call( $_[SESSION], 'shutdown' );
}

# Connect to IRC Network

sub connect {
  my ($kernel, $heap, $session) = @_[KERNEL, HEAP, SESSION];

  if ($heap->{'socket'}) {
        $kernel->call ($session, 'squit');
  }

  $heap->{socketfactory} = POE::Wheel::SocketFactory->new(
                                        SocketDomain => AF_INET,
                                        SocketType => SOCK_STREAM,
                                        SocketProtocol => 'tcp',
                                        RemoteAddress => $heap->{'remoteserver'},
                                        RemotePort => $heap->{'serverport'},
                                        SuccessEvent => '_sock_up',
                                        FailureEvent => '_sock_failed',
                                        ( $heap->{localaddr} ? (BindAddress => $heap->{localaddr}) : () ),
  );

}

# Internal function called when a socket is closed.
sub _sock_down {
  my ($kernel, $heap) = @_[KERNEL, HEAP];

  # Destroy the RW wheel for the socket.
  delete $heap->{'socket'};
  $heap->{connected} = 0;

  # post a 'irc_disconnected' to each session that cares
  foreach (keys %{$heap->{sessions}}) {
    $kernel->post( $heap->{sessions}->{$_}->{'ref'},
                   'irc_p10_disconnected', $heap->{server} );
  }
}

sub _sock_up {
  my ($kernel,$heap,$session,$socket) = @_[KERNEL,HEAP,SESSION,ARG0];
  $heap->{connecttime} = time();
  $heap->{State}->_burst_create();

  delete $heap->{socketfactory};

  $heap->{localaddr} = (unpack_sockaddr_in( getsockname $socket))[1];

  $heap->{'socket'} = new POE::Wheel::ReadWrite
  (
        Handle => $socket,
        Driver => POE::Driver::SysRW->new(),
        Filter => POE::Filter::Line->new(),
        InputEvent => '_parseline',
        ErrorEvent => '_sock_down',
   );

  if ($heap->{'socket'}) {
        $heap->{connected} = 1;
  } else {
        _send_event ( $kernel, $heap, 'irc_p10_socketerr', "Couldn't create ReadWrite wheel for IRC socket" );
  }

  foreach (keys %{$heap->{sessions}}) {
        $kernel->post( $heap->{sessions}->{$_}->{'ref'}, 'irc_p10_connected', $heap->{remoteserver}, $heap->{servernum} );
  }

  $heap->{socket}->put("PASS $heap->{password}\n");
  $heap->{socket}->put("SERVER $heap->{servername} 1 $heap->{starttime} $heap->{connecttime} J10 $heap->{servernum}A]] 0 :$heap->{serverdesc}\n");
  # Sit back and wait for the uplink to finish its burst
}

sub _sock_failed {
  my ($kernel, $heap, $op, $errno, $errstr) = @_[KERNEL, HEAP, ARG0..ARG2];

  _send_event( $kernel, $heap, 'irc_p10_socketerr', "$op error $errno: $errstr" );
}

# Parse each line from received at the socket

# Parse a message from the IRC server and generate the appropriate
# event(s) for listening sessions.
sub _parseline {
  my ($kernel, $session, $heap, $line) = @_[KERNEL, SESSION, HEAP, ARG0];
  my (@events, @cooked);

  # Feed the proper Filter object the raw IRC text and get the
  # "cooked" events back for sending, then deliver each event. We
  # handle CTCPs separately from normal IRC messages here, to avoid
  # silly module dependencies later.

  @cooked = ($line =~ tr/\001// ? @{$heap->{ctcp_filter}->get( [$line] )}
             : @{$heap->{irc_filter}->get( [$line] )} );

  foreach my $ev (@cooked) {
    $ev->{name} = 'irc_p10_' . $ev->{name};
    _send_event( $kernel, $heap, $session, $ev->{name}, @{$ev->{args}} );
  }
}


# Sends an event to all interested sessions. This is a separate sub
# because I do it so much, but it's not an actual POE event because it
# doesn't need to be one and I don't need the overhead.
# BinGOs: Added a hack to try and improve performance for IRC Service
#         use a CALL instead of a POST if the session ID is our own
sub _send_event  {
  my ($kernel, $heap, $session, $event, @args) = @_;
  my %sessions;

  foreach (values %{$heap->{events}->{'irc_p10_all'}},
           values %{$heap->{events}->{$event}}) {
    $sessions{$_} = $_;
  }
  # Make sure our session gets notified of any requested events before any other bugger
  $kernel->call( $session, $event, @args ) if ( defined ($sessions{$session}) );
  foreach (values %sessions) {
    $kernel->post( $_, $event, @args ) unless ( $_ eq $session );
  }
}

sub addnick {
  my ($kernel, $heap, $session, $args) = @_[KERNEL, HEAP, SESSION, ARG0];
  my $connecttime = time();

  if ($args) {
    my %arg;
    if (ref $args eq 'HASH') {
      %arg = %$args;
    } else {
      die "First argument to addnick() should be a hash reference";
    }

    # Gentlemen, lets get down to business
    # Mandatory fields we must must must have these, damnit
    my $nickname = $arg{'NickName'} if exists $arg{'NickName'};
    my $username = $arg{'UserName'} if exists $arg{'UserName'};
    my $hostname = $arg{'HostName'} if exists $arg{'HostName'};
    my $authname = $arg{'AuthName'} if exists $arg{'AuthName'};
    my $umode = $arg{'Umode'} if exists $arg{'Umode'};
    my $description = $arg{'Description'} if exists $arg{'Description'};
    my $localaddr = $arg{'LocalAddr'} if exists $arg{'LocalAddr'};

    unless (defined $nickname) {
	die "You must specify at least a NickName to addnick";
    }

    # Check if NickName already exists. Issue a KILL if it does and call addnick again with a delay
    if (not $heap->{State}->nick_numeric($nickname) ) {

      my $numeric = dectobase64($heap->{nextnick},3);
      $heap->{nextnick}++;

      # Default everything else

      my $cmd = "N $nickname 1 $connecttime ";
      $cmd .= lc $nickname . " " unless (defined $username);
      $cmd .= "$username " if (defined $username);
      $cmd .= "$heap->{servername} " unless (defined $hostname);
      $cmd .= "$hostname " if (defined $hostname);
      $umode = "+odk" unless (defined $umode);
      $umode = "+" . $umode unless ($umode =~ /^\+/ or not defined $umode);
      $umode .= "r" unless (not defined $authname or $umode =~ /r/);
      $authname = $nickname unless (defined $authname or $umode !~ /r/);
      $umode .= " $authname" unless (not defined $authname or $umode !~ /r/);
      $description = $heap->{serverdesc} unless (defined $description);
      $localaddr = "127.0.0.1" unless (defined $localaddr);

      $localaddr = inttobase64($localaddr);

      $cmd .= "$umode $localaddr $heap->{servernum}$numeric :$description";

      $kernel->yield ( 'sl_server', $cmd );

    } else {
      # Issue the KILL ... bye bye sucker!
      my ($target) = $heap->{State}->nick_numeric($nickname);
      $kernel->yield ( 'sl_server', $cmd2token{'KILL'} . " $target :" . $heap->{servername} . " (Using a Services nickname)" );
      $kernel->delay ( 'addnick' => 5 => \%arg );
    }

  } else {
      die "First argument to addnick() should be a hash or array reference";
  }

}

# Deal with STATS requests

sub irc_p10_stats {
  my ($kernel,$heap,$who,$what) = @_[KERNEL,HEAP,ARG0,ARG1];

  my ($first,undef) = split(/ :/,$what,2);
  SWITCH: {
    if ($first eq "u") {
	my ($string) = timestring($heap->{starttime});
	my ($clients) = $heap->{State}->server_clients($heap->{servernum});
	$kernel->yield( 'sl_server', "242 $who :$string" );
	$kernel->yield( 'sl_server', "250 $who :Highest connection count: " . ( $clients + 1 ) . " ($clients clients)" );
	$kernel->yield( 'sl_server', "219 $who $first :End of /STATS report" );
	last SWITCH;
    }
  }
}

# Remove client if we get a quit for it

sub irc_p10_quit {
  my ($heap, $who) = @_[HEAP,ARG0];

  $heap->{State}->_nick_del($who);
}

# Or if it is killed

sub irc_p10_kill {
  my ($heap, $who, $what) = @_[HEAP,ARG0,ARG1];

  $heap->{State}->_nick_del(substr($what,0,5));
}

# Server related handlers
# Who is our uplink ... needed so that we don't respond to every single EOB

sub irc_p10_server_link {
  my ($heap, @args) = @_[HEAP,ARG0 .. $#_];

  $heap->{serverlink} = substr($args[5],0,2);
  $heap->{State}->{serverlink} = $heap->{serverlink};
  $heap->{State}->_server_add($heap->{serverlink},$args[0],$args[1],$heap->{servernum});
}

sub irc_p10_server {
  my ($heap, $what, $args) = @_[HEAP,ARG0,ARG1];

  my ($first,$second) = split(/ :/,$args);
  my (@args) = split(/ /,$first);
  $heap->{State}->_server_add(substr($args[5],0,2),$args[0],$args[1],$what);
}

sub irc_p10_squit {
  my ($heap, $what, $args) = @_[HEAP,ARG0,ARG1];

  my ($first,$second) = split(/ :/,$args);
  my (@args) = split(/ /,$first);
  my ($who) = $heap->{State}->server_numeric($args[0]);
  if ($who eq $heap->{servernum}) {
    $_[KERNEL]->delay( 'autoping' => undef );
    $who = $heap->{serverlink};
  }
  $heap->{State}->_server_del($who);
}

sub irc_p10_disconnected {
  my ($kernel,$heap) = @_[KERNEL,HEAP];

  if ( $heap->{Reconnect} ) {
    $kernel->yield( 'connect' );
  }
}

sub irc_p10_socketerr {
  my ($kernel,$heap) = @_[KERNEL,HEAP];

  if ( $heap->{Reconnect} ) {
    $kernel->yield( 'connect' );
  }
}

sub irc_p10_version {
  my ($kernel, $heap, $who) = @_[KERNEL,HEAP,ARG0];

  $kernel->yield( 'sl_server', "351 $who $heap->{version}. $heap->{servername} :" );
}

# Generate an automatic pong in response to IRC Server's ping

sub irc_p10_ping {
  my ($kernel, $heap, $arg) = @_[KERNEL, HEAP, ARG0];

  $kernel->yield( 'sl_server', "$cmd2token{PONG} $arg") unless ( $arg eq $heap->{servernum} );
}

# Generate an automatic end_of_burst_ack in response to IRC Server's end_of_burst. It's only polite of course :)

sub irc_p10_end_of_burst {
  my ($kernel, $heap, $arg) = @_[KERNEL, HEAP, ARG0];

  if ($arg eq $heap->{serverlink}) {
    $kernel->yield( 'sl_server', "$cmd2token{EOB_ACK}");
    # Now we burst our shit to the uplink
    foreach ($heap->{State}->_burst_info()) {
        $kernel->yield( 'sl_server', $_ );
    }
    $kernel->yield( 'sl_server', "$cmd2token{END_OF_BURST}" );
    $heap->{State}->_burst_destroy();
  }
}

sub irc_p10_eob_ack {
  my ($kernel,$heap) = @_[KERNEL,HEAP];

  # Lazy, but we can assume that it is our uplink that ACK'ed
  # Start PINGing uplink server

  $kernel->yield( 'autoping' );
}

sub autoping {
  my ($kernel,$heap) = @_[KERNEL,HEAP];

  if ( $heap->{'socket'} ) {
    my ($uplink) = $heap->{State}->server_name($heap->{serverlink});
    my ($seconds,$micro) = gettimeofday();

    $kernel->yield( 'sl_server', "G !$seconds.$micro $uplink $seconds.$micro" );
    $kernel->delay( 'autoping' => $heap->{pingfreq} );
  }
}

sub irc_p10_whois {
  my ($kernel,$heap,$who,$what) = @_[KERNEL,HEAP,ARG0,ARG1];
  my ($target) = ( split /:/,$what )[1];

  my ($numeric) = $heap->{State}->nick_numeric($target);
  my (@info) = $heap->{State}->nick_info($numeric);

  $kernel->yield( 'sl_server' => "311 $who $info[0] " . $info[1] . " $info[2] * :$info[3]" );
  if ( defined ( $heap->{his_servername} ) and defined ( $heap->{his_serverinfo} ) and not $heap->{State}->is_operator($who) ) {
    $kernel->yield( 'sl_server' => "312 $who $info[0] $heap->{his_servername} :$heap->{his_serverinfo}" );
  } else {
    $kernel->yield( 'sl_server' => "312 $who $info[0] $heap->{servername} :$heap->{serverdesc}" );
  }
  $kernel->yield( 'sl_server' => "313 $who $info[0] :is an IRC Operator" ) if ( $info[4] =~ /o/ );
  $kernel->yield( 'sl_server' => "330 $who $info[0] $info[5] :is authed as" ) if ( defined ($info[5]) );
  $kernel->yield( 'sl_server' => "317 $who $info[0] 0 $heap->{starttime} :seconds idle, signon time" );
  $kernel->yield( 'sl_server' => "318 $who $info[0] :End of /WHOIS list." );
}

# Track New Nicks and Nick Changes

sub irc_p10_nick {
  my ($kernel, $heap, $who) = @_[KERNEL,HEAP,ARG0];

  # New nick on the network or a burst
  if ($who =~ /^.{2}$/) {
    my ($oper,$authname);
    my ($first,$second) = split(/ :/,$_[ARG1],2);
    my (@args) = split(/ /,$first);
    if ($args[5] =~ /^\+([a-zA-Z]+)/) {
       $oper = $1;
       if ($args[5] =~ /r/) {
          $authname = $args[6];
       }
    }
    $heap->{State}->_nick_add($args[$#args],$args[0],$args[3],$args[4],$args[$#args-1],$args[2],$oper,$authname,$second);

  # Or a user changed their nick
  } else {
    my ($first,undef) = split(/ /,$_[ARG1]);
    $heap->{State}->_nick_change($who,$first);
  }

}

sub irc_p10_account {
  my ($kernel, $heap, $what) = @_[KERNEL,HEAP,ARG1];

  my ($who,$account) = split(/ /,$what);
  # Strip any leading colons
  $account =~ s/^://;
  $heap->{State}->_nick_account($who,$account);
}

sub irc_p10_burst {
  my ($kernel,$heap,$who,$what) = @_[KERNEL,HEAP,ARG0,ARG1];

  $heap->{State}->_channel_burst($who,$what);
}

sub irc_p10_create {
  my ($kernel,$heap,$who,$what) = @_[KERNEL,HEAP,ARG0,ARG1];

  my ($channel,$timestamp) = split(/ /,$what);
  $heap->{State}->_channel_join($channel,$who,$timestamp);
}

sub irc_p10_join {
  my ($kernel,$heap,$who,$what) = @_[KERNEL,HEAP,ARG0,ARG1];

  my ($channel,$timestamp) = split(/ /,$what);
  $heap->{State}->_channel_join($channel,$who,$timestamp);
}

sub irc_p10_part {
  my ($kernel,$heap,$who,$what) = @_[KERNEL,HEAP,ARG0,ARG1];

  my ($channel,undef) = split(/ :/,$what);
  $heap->{State}->_channel_part($channel,$who);
}

sub irc_p10_kick {
  my ($kernel,$heap,$who,$channel,$victim) = @_[KERNEL,HEAP,ARG0,ARG1,ARG2];

  $heap->{State}->_channel_part($channel,$victim);
}

sub irc_p10_mode {
  my ($kernel,$heap,$who,$what,$mode) = @_[KERNEL,HEAP,ARG0,ARG1,ARG2];

  # Assume that if it isnt a channel its a umode change
  if ($what !~ /^#/) {
    $heap->{State}->_nick_umode($who,$mode);
  } else {
    # Okay its a channel mode
    if ( $heap->{eventmode} ) {
      my ($args) = join(" ",@_[ARG3..$#_]);
      $mode .= " " . $args if ( defined ($args) );
      $heap->{State}->_channel_mode($what,$mode,$who);
    }
  }
}

sub irc_p10_opmode {
  my ($kernel,$heap,$who,$what,$mode) = @_[KERNEL,HEAP,ARG0,ARG1,ARG2];

  # Assume that if it isnt a channel its a umode change
  if ($what !~ /^#/) {
    $heap->{State}->_nick_umode($who,$mode);
  } else {
    # Okay its a channel mode
    my ($args) = join(" ",@_[ARG3..$#_]);
    $mode .= " " . $args if ( defined ($args) );
    $heap->{State}->_channel_mode($what,$mode,$who);
  }
}

sub irc_p10_clearmode {
  my ($kernel,$heap,$who,$what) = @_[KERNEL,HEAP,ARG0,ARG1];

  my ($channel,$flags) = split(/ /,$what);
  $heap->{State}->_channel_clearmode($channel,$flags);
}

# Our event handlers for events sent to us

# The handler for commands which have N arguments, separated by commas.
sub commasep {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $args = join ',', @_[ARG0 .. $#_];

  $state = $cmd2token{uc( $state )};
  $state .= " $args" if defined $args;
  $kernel->yield( 'sl_server', $state );
}

# The handler for commands which have N arguments, separated by commas. Client hacked.
sub commasep_client {
  my ($kernel, $state, $numeric) = @_[KERNEL, STATE, ARG0];
  my $args = join ',', @_[ARG1 .. $#_];

  $state = $cmd2token{uc( $state )};
  $state .= " $args" if defined $args;
  $kernel->yield( 'sl_client', "$numeric $state" );
}

# Send a CTCP query or reply, with the same syntax as a PRIVMSG event.
sub ctcp {
  my ($kernel, $state, $heap, $numeric, $to) = @_[KERNEL, STATE, HEAP, ARG0, ARG1];
  my $message = join ' ', @_[ARG2 .. $#_];

  unless (defined $numeric and defined $to and defined $message) {
    die "The POE::Component::IRC event \"$state\" requires three arguments";
  }

  # CTCP-quote the message text.
  ($message) = @{$heap->{ctcp_filter}->put([ $message ])};

  # Should we send this as a CTCP request or reply?
  $state = $state eq 'ctcpreply' ? 'notice' : 'privmsg';

  $kernel->yield( $state, $numeric, $to, $message );
}

# Tell the IRC server to forcibly remove a user from a channel.
sub kick {
  my ($kernel, $numeric, $chan, $nick) = @_[KERNEL, ARG0, ARG1, ARG2];
  my $message = join '', @_[ARG3 .. $#_];

  unless (defined $numeric and defined $chan and defined $nick) {
    die "The POE::Component::IRC event \"kick\" requires at least three arguments";
  }

  $nick .= " :$message" if defined $message;
  $kernel->yield('sl_client', "$numeric K $chan $nick" );
}

# Join a channel. Do a CREATE or JOIN appropriately
sub join {
  my ($kernel,$heap,$numeric,$channel,$modes) = @_[KERNEL,HEAP,ARG0,ARG1,ARG2];

  unless (defined $numeric and defined $channel) {
    die "The POE::Component::IRC event \"join\" requires at least two arguments";
  }
  
  my ($timestamp) = time();
  if ( $heap->{State}->channel_exists($channel) ) {
    # Join channel
    $kernel->yield( 'sl_client' => "$numeric J $channel $timestamp" );
  } else {
    # Create channel
    $kernel->yield( 'sl_client' => "$numeric C $channel $timestamp" );
  }
}

# The handler for all IRC commands that take no arguments.
sub noargs {
  my ($kernel, $state, $arg) = @_[KERNEL, STATE, ARG0];

  if (defined $arg) {
    die "The POE::Component::IRC event \"$state\" takes no arguments";
  }
  $kernel->yield( 'sl_server', $cmd2token{uc( $state )} );
}

# The handler for all IRC commands that take no arguments. Client hacked.
sub noargs_client {
  my ($kernel, $state, $numeric, $arg) = @_[KERNEL, STATE, ARG0, ARG1];

  unless (defined $numeric) {
    die "The POE::Component::IRC event \"$state\" requires at least one argument";
  }

  if (defined $arg) {
    die "The POE::Component::IRC event \"$state\" takes no arguments";
  }
  $kernel->yield( 'sl_client', "$numeric " . $cmd2token{uc( $state )} );
}

# The handler for commands that take one required and two optional arguments.
sub oneandtwoopt {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $arg = join '', @_[ARG0 .. $#_];

  $state = $cmd2token{uc( $state )};
  if (defined $arg) {
    $arg = ':' . $arg if $arg =~ /\s/;
    $state .= " $arg";
  }
  $kernel->yield( 'sl_server', $state );
}

# The handler for commands that take one required and two optional arguments. Client hacked.
sub oneandtwoopt_client {
  my ($kernel, $state, $numeric) = @_[KERNEL, STATE, ARG0];
  my $arg = join '', @_[ARG1 .. $#_];

  unless (defined $numeric) {
    die "The POE::Component::IRC event \"$state\" requires at least one argument";
  }

  $state = $cmd2token{uc( $state )};
  if (defined $arg) {
    $arg = ':' . $arg if $arg =~ /\s/;
    $state .= " $arg";
  }
  $kernel->yield( 'sl_client', "$numeric $state" );
}

# The handler for commands that take at least one optional argument.
sub oneoptarg {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $arg = join '', @_[ARG0 .. $#_] if defined $_[ARG0];

  $state = $cmd2token{uc( $state )};
  if (defined $arg) {
    $arg = ':' . $arg if $arg =~ /\s/;
    $state .= " $arg";
  }
  $kernel->yield( 'sl_server', $state );
}

# The handler for commands that take at least one optional argument. Client hacked.
sub oneoptarg_client {
  my ($kernel, $state, $numeric) = @_[KERNEL, STATE, ARG0];
  my $arg = join '', @_[ARG1 .. $#_] if defined $_[ARG1];

  unless (defined $numeric) {
    die "The POE::Component::IRC event \"$state\" requires at least one argument";
  }

  $state = $cmd2token{uc( $state )};
  if (defined $arg) {
    $arg = ':' . $arg if $arg =~ /\s/;
    $state .= " $arg";
  }
  $kernel->yield( 'sl_client', "$numeric $state" );
}

# The handler for commands which take one required and one optional argument.
sub oneortwo {
  my ($kernel, $state, $one) = @_[KERNEL, STATE, ARG0];
  my $two = join '', @_[ARG1 .. $#_];

  unless (defined $one) {
    die "The POE::Component::IRC event \"$state\" requires at least one argument";
  }

  $state = $cmd2token{uc( $state )} . " $one";
  $state .= " $two" if defined $two;
  $kernel->yield( 'sl_server', $state );
}

# The handler for commands which take one required and one optional argument. Client hacked.
sub oneortwo_client {
  my ($kernel, $state, $numeric, $one) = @_[KERNEL, STATE, ARG0, ARG1];
  my $two = join '', @_[ARG2 .. $#_];

  unless (defined $numeric and defined $one) {
    die "The POE::Component::IRC event \"$state\" requires at least two argument";
  }

  $state = $cmd2token{uc( $state )} . " $one";
  $state .= " $two" if defined $two;
  $kernel->yield( 'sl_client', "$numeric $state" );
}

# Handler for commands that take exactly one argument.
sub onlyonearg {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $arg = join '', @_[ARG0 .. $#_];

  unless (defined $arg) {
    die "The POE::Component::IRC event \"$state\" requires one argument";
  }

  $state = $cmd2token{uc( $state )};
  $arg = ':' . $arg if $arg =~ /\s/;
  $state .= " $arg";
  $kernel->yield( 'sl_server', $state );
}

# Handler for commands that take exactly one argument. Client hacked.
sub onlyonearg_client {
  my ($kernel, $state, $numeric) = @_[KERNEL, STATE, ARG0];
  my $arg = join '', @_[ARG1 .. $#_];

  unless (defined $numeric and defined $arg) {
    die "The POE::Component::IRC::Service::P10 event \"$state\" requires two argument";
  }

  $state = $cmd2token{uc( $state )};
  $arg = ':' . $arg if $arg =~ /\s/;
  $state .= " $arg";
  $kernel->yield( 'sl_client', "$numeric $state" );
}

# Handler for commands that take exactly two arguments.
sub onlytwoargs {
  my ($heap, $kernel, $state, $one) = @_[HEAP, KERNEL, STATE, ARG0];
  my ($two) = join '', @_[ARG1 .. $#_];

  unless (defined $one and defined $two) {
    die "The POE::Component::IRC::Service::P10 event \"$state\" requires two arguments";
  }

  $state = $cmd2token{uc( $state )};
  $two = ':' . $two if $two =~ /\s/;
  $kernel->yield( 'sl_server', "$state $one $two" );
}

# Handler for commands that take exactly two arguments. Client hacked.
sub onlytwoargs_client {
  my ($heap, $kernel, $state, $numeric, $one) = @_[HEAP, KERNEL, STATE, ARG0, ARG1];
  my ($two) = join '', @_[ARG2 .. $#_];

  unless (defined $numeric and defined $one and defined $two) {
    die "The POE::Component::IRC::Service::P10 event \"$state\" requires three arguments";
  }

  $state = $cmd2token{uc( $state )};
  $two = ':' . $two if $two =~ /\s/;
  $kernel->yield( 'sl_client', "$numeric $state $two" );
}

# Handler for privmsg or notice events.
sub privandnotice {
  my ($kernel, $state, $numeric, $to) = @_[KERNEL, STATE, ARG0, ARG1];
  my $message = join ' ', @_[ARG2 .. $#_];

  unless (defined $numeric and defined $to and defined $message) {
    die "The POE::Component::IRC event \"$state\" requires three arguments";
  }

  if (ref $to eq 'ARRAY') {
    $to = join ',', @$to;
  }

  $state = $cmd2token{uc( $state )};
  $state .= " $to :$message";
  $kernel->yield( 'sl_client', "$numeric $state" );
}

# Tell the IRC session to go away.
sub shutdown {
  my ($kernel, $heap) = @_[KERNEL, HEAP];

  foreach ($kernel->alias_list( $_[SESSION] )) {
    $kernel->alias_remove( $_ );
  }

  foreach (qw(socket sock socketfactory dcc wheelmap)) {
    delete $heap->{$_};
  }
}

# The handler for commands which have N arguments, separated by spaces.
sub spacesep {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $args = join ' ', @_[ARG0 .. $#_];

  $state = $cmd2token{uc( $state )};
  $state .= " $args" if defined $args;
  $kernel->yield( 'sl_server', $state );
}

# The handler for commands which have N arguments, separated by spaces. Client hacked.
sub spacesep_client {
  my ($kernel, $state, $numeric) = @_[KERNEL, STATE, ARG0];
  my $args = join ' ', @_[ARG1 .. $#_];

  $state = $cmd2token{uc( $state )};
  $state .= " $args" if defined $args;
  $kernel->yield( 'sl_server', "$numeric $state" );
}

# Dish out server initiated commands

sub sl_server {
  my ($kernel, $heap, $cmd) = @_[KERNEL, HEAP, ARG0];

  # TODO: need to categorise events so that we don't send crap to the uplink
  # eg. if we send events to our clients 
  # TBH I don't suppose the uplink cares that much :o)

  $heap->{socket}->put("$heap->{servernum} $cmd\n") if ( $heap->{'socket'} );
  $kernel->yield('_parseline',"$heap->{servernum} $cmd");
}

# Dish out client (whichever is specified) initiated commands

sub sl_client {
  my ($kernel, $heap, $cmd) = @_[KERNEL, HEAP, ARG0];

  $heap->{socket}->put("$cmd\n") if ($heap->{'socket'});
  $kernel->yield('_parseline',$cmd);
}

# Set or query the current topic on a channel.
sub topic {
  my ($kernel,$heap, $numeric, $chan) = @_[KERNEL,HEAP, ARG0, ARG1];
  my $topic = join '', @_[ARG2 .. $#_];

  $chan .= " :$topic" if length $topic;
  $kernel->yield('sl_client',"$numeric T $chan");
}

# Base64 P10 Stylee functions

# Convert decimal to Base64 optionally provide the length of the Base64 returned
sub dectobase64 {
  my ($number) = shift || 0;
  my ($output) = shift || 2;
  my ($numeric) = "";

  if ($number == 0) {
    for (my $i = length($numeric); $i < $output; $i++) {
      $numeric = "A" . $numeric;
    }
    return $numeric;
  }

  my ($b64chars) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789[]";
  my (@d2b64) = split(//,$b64chars);

  my (@convert); my ($g); my ($r);

  LOOP: while (1) {
    $g = $number / 64;
    $r = $number % 64;
    if ($g >= 64) {
        $number = $g;
        push(@convert,$r);
    } else {
        push(@convert,$r);
        push(@convert,int $g);
        last LOOP;
    }
  }
  foreach (reverse @convert) {
    $numeric .= $d2b64[$_];
  }
  for (my $i = length($numeric); $i < $output; $i++) {
    $numeric = "A" . $numeric;
  }
  return $numeric;
}

# Convert from Base64 to decimal
sub base64todec {
  my ($numeric) = shift || return undef;

  my ($b64chars) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789[]";
  my (@d2b64) = split(//,$b64chars);
  my (%b642d) = ();
  for (my $i = 0; $i <= $#d2b64; $i++) {
    $b642d{$d2b64[$i]} = $i;
  }

  my (@numeric) = reverse split(//,$numeric);
  my ($number) = 0;

  for (my $i=0; $i <= $#numeric; $i++) {
        $number += (64**$i) * $b642d{$numeric[$i]};
  }
  return $number;
}

# Convoluted method to convert from IP quad to Base64 /me *sighs*
sub inttobase64 {
  my ($quad) = shift || return undef;

  return dectobase64(hex(int2hex(dotq2int($quad))));
}

# The following two functions are taken from :-
# http://www.math.ucla.edu/~jimc/jvtun
# Copyright © 2003 by James F. Carter.  2003-08-02, Perl-5.8.0

sub dotq2int {
    my @dotq = split /[.\/]/, $_[0];
    push(@dotq, 32) if @dotq == 4;
    my($ip) = unpack("N", pack("C4", splice(@dotq, 0, 4)));
    my($mask) = (@dotq > 1) ? unpack("N", pack("C4", @dotq)) :
        $dotq[0] ? ~((1 << (32-$dotq[0]))-1) : 0;

    ($ip, $mask);
}

sub int2hex {
    sprintf("%08X", $_[0]);
}

# Our own little function to return a proper uppercase nickname or channel name IRC stylee
# See the RFC for the details

sub u_irc {
  my ($value) = shift || return undef;

  $value =~ tr/a-z{}|/A-Z[]\\/;
  return $value;
}


# Return a correctly formatted string for STATS u requests

sub timestring {
      my ($timeval) = shift || return 0;
      my $uptime = time() - $timeval;
  
      my $days = int $uptime / 86400;
      my $remain = $uptime % 86400;
      my $hours = int $remain / 3600;
      $remain %= 3600;
      my $mins = int $remain / 60;
      $remain %= 60;
      return sprintf("Server Up %d days, %2.2d:%2.2d:%2.2d",$days,$hours,$mins,$remain);
}

sub retOpflags {
  my ($opflags) = shift || return undef;
  my (@opflags) = ();
  my ($action) = "";

  for (my $i = 0; $i < length($opflags); $i++) {
    my $char = substr($opflags,$i,1);
    if ($char eq "+" or $char eq "-") {
       $action = $char;
    } else {
       push (@opflags,"$action$char");
    }
  }
  return @opflags;
}

# Object Methods
# Private methods begin with _

sub _server_add {
  my ($self) = shift;
  
  my ($server) = { Numeric => $_[0],
		   Name    => $_[1],
		   Hops    => $_[2],
		   Link    => $_[3] 
		 };

  $self->{servers_numeric}->{ $server->{Numeric} } = $server;
  $self->{servers_name}->{ $server->{Name} } = $server;
  return 1;
}

sub _server_del {
  my ($self) = shift;
  my ($server) = shift || return 0;

  $self->{servers_numeric}->{$server}->{ToDelete} = 1;
  foreach ( keys %{ $self->{servers_numeric} } ) {
    if ( $server eq $self->{servers_numeric}->{$_}->{Link} and not defined ( $self->{servers_numeric}->{$server}->{ToDelete}) ) {
       $self->_server_del($self->{servers_numeric}->{$_}->{Link});
    }
  }
  my (@numerics) = grep /^$server/, keys %{ $self->{bynumeric} };
  foreach (@numerics) {
    $self->_nick_del($_);
  }
  my ($servername) = $self->{servers_numeric}->{$server}->{Name};
  delete ( $self->{servers_numeric}->{$server} );
  delete ( $self->{servers_name}->{$servername} );
  return 1;
}

sub _nick_add {
  my ($self) = shift;
  my ($numeric) = $_[0] || return 0;
  my ($nickname) = $_[1] || return 0;
  my ($username) = $_[2] || return 0;
  my ($hostname) = $_[3] || return 0;
  my ($ipaddr) = $_[4] || return 0;
  my ($timestamp) = $_[5] || time();
  my ($umode) = $_[6] || undef;
  my ($authname) = $_[7] || undef;
  my ($ircname) = $_[8] || undef;

  # Does the nickname already exist in our state, ie. one of our clients
  # If so kludge the timestamp on ours so it is older and they will get KILLed mwuahahahaha :o)
  if ( defined ( $self->{bynickname}->{ u_irc ($nickname) } ) ) {
    my ($kludge) = $timestamp - 30;
    $self->{bynickname}->{ u_irc ( $nickname ) }->{TimeStamp} = $kludge;
    if ( defined ( $self->{burst_nicks}->{ $self->{bynickname}->{ u_irc ( $nickname ) }->{Numeric} } ) ) {
      $self->{burst_nicks}->{ $self->{bynickname}->{ u_irc ( $nickname ) }->{Numeric} }->{TimeStamp} = $kludge;
    }
  }

  if ( not defined ( $self->{bynumeric}->{$numeric} ) ) {
    my ($record) = { Numeric => $numeric,
  		     NickName => $nickname,
		     UserName => $username,
		     HostName => $hostname,
		     IRCName => $ircname,
		     IPAddr => $ipaddr,
		     TimeStamp => $timestamp,
		     UMode => $umode,
		     AuthName => $authname };
    $self->{bynumeric}->{ $record->{Numeric} } = $record;
    $self->{bynickname}->{ u_irc ( $record->{NickName} ) } = $record;
  }
		   
  return 1;
}

sub _nick_del {
  my ($self) = shift;
  my ($numeric) = shift || return 0;

  foreach ( keys %{ $self->{bynumeric}->{$numeric}->{Channels} } ) {
    delete ( $self->{channels}->{$_}->{Members}->{$numeric} );
    if ( scalar ( keys % { $self->{channels}->{$_}->{Members} } ) == 0 ) {
	delete ( $self->{channels}->{$_} );
    }
  }
  my ($nickname) = u_irc ( $self->{bynumeric}->{$numeric}->{NickName} );
  delete ( $self->{bynumeric}->{$numeric} );
  delete ( $self->{bynickname}->{$nickname} );
  return 1;
}

sub _nick_change {
  my ($self) = shift;
  my ($numeric) = shift || return 0;
  my ($newnick) = shift || return 0;

  my ($currentnick) = u_irc( $self->{bynumeric}->{$numeric}->{NickName} );
  $self->{bynumeric}->{$numeric}->{NickName} = $newnick;
  $self->{bynumeric}->{$numeric}->{TimeStamp} = time();
  my ($record) = $self->{bynumeric}->{$numeric};
  delete $self->{bynickname}->{$currentnick};
  $self->{bynickname}->{ u_irc( $record->{NickName} ) } = $record;
  return 1;
}

sub _nick_account {
  my ($self) = shift;
  my ($numeric) = $_[0] || return 0;
  my ($account) = $_[1] || return 0;

  $self->{bynumeric}->{$numeric}->{AuthName} = $account;
  $self->_nick_umode($numeric,"+r");
}

sub _nick_umode {
  my ($self) = shift;
  my ($numeric) = $_[0] || return 0;
  my ($umode) = $_[1] || return 0;

  my ($currentumode) = $self->{bynumeric}->{$numeric}->{UMode};
  foreach (retOpflags($umode)) {
    SWITCH: {
      if (/^\+(.+)/) {
	if ( not defined ($currentumode) ) {
	  $currentumode = $1;
	} else {
	  $currentumode .= $1;
	  $currentumode = join("",sort(split(//,$currentumode)));
	}
	last SWITCH;
      }
      if (/^-(.+)/) {
	if ( defined ($currentumode) ) {
	  $currentumode =~ s/$1//g;
	}
	last SWITCH;
      }
    }
  }
  if ( defined ($currentumode) and $currentumode ) {
    $self->{bynumeric}->{$numeric}->{UMode} = $currentumode;
  } else {
    delete ( $self->{bynumeric}->{$numeric}->{UMode} );
  }
  return 1;
}

sub _channel_join {
  my ($self) = shift;
  my ($channel) = $_[0] || return 0;
  my ($numeric) = $_[1] || return 0;
  my ($timestamp) = $_[2];
  my ($usermode) = 0;
  my ($channelname) = $channel;
  $channel = u_irc ( $channel );
  
  if (not exists $self->{channels}->{$channel}) {
    $self->{channels}->{$channel}->{Channel} = $channelname;
    $self->{channels}->{$channel}->{TimeStamp} = $timestamp;
    $usermode = 2;
  }
  $self->{channels}->{$channel}->{Members}->{$numeric} = $usermode;
  $self->{bynumeric}->{$numeric}->{Channels}->{$channel} = $usermode;
  return 1;
}

sub _channel_part {
  my ($self) = shift;
  my ($channel) = $_[0] || return 0;
  my ($numeric) = $_[1] || return 0;
  my ($channelname) = $channel;
  $channel = u_irc ( $channel );

  delete ( $self->{channels}->{$channel}->{Members}->{$numeric} );
  if ( scalar ( keys % { $self->{channels}->{$_}->{Members} } ) == 0 ) {
    delete ( $self->{channels}->{$_} );
  }
  delete ( $self->{bynumeric}->{$numeric}->{Channels}->{$channel} );
  return 1;
}

sub _channel_topic {
  my ($self) = shift;
  my ($channel) = u_irc( $_[0] ) || return 0;
  my ($topic) = $_[1] || return 0;
  my ($set_by) = $_[2] || return 0;
  my ($timestamp) = $_[3] || return 0;

  $self->{channels}->{$channel}->{Topic} = $topic;
  $self->{channels}->{$channel}->{Set_By} = $set_by;
  $self->{channels}->{$channel}->{TopicTS} = $timestamp;
  return 1;
}

sub _channel_untopic {
  my ($self) = shift;
  my ($channel) = u_irc( $_[0] ) || return 0;

  delete ( $self->{channels}->{$channel}->{Topic} );
  delete ( $self->{channels}->{$channel}->{Set_By} );
  delete ( $self->{channels}->{$channel}->{TopicTS} );
  return 1;
}

sub _channel_mode {
  my ($self) = shift;
  my ($channel) = u_irc( $_[0] ) || return 0;
  my ($string) = $_[1] || return 0;
  my ($who) = $_[2] || return 0; # This is either a server or client numeric only used for bans tbh

  my ($modes,@args) = split(/ /,$string);
  my (@modes) = retOpflags($modes);
  my ($currentmode) = $self->{channels}->{$channel}->{Mode};
  foreach (@modes) {
    my ($argument) = shift(@args) if (/\+[ovbkl]/);
    my ($argument) = shift(@args) if (/-[ovb]/);
    SWITCH: {
      if (/b/) {
	$self->_channel_ban($channel,$_,$argument,$who);
	last SWITCH;
      }
      if (/l/) {
	if (/^\+(.+)/) {
	  $self->{channels}->{$channel}->{ChanLimit} = $argument;
	  $currentmode .= $1;
	} else {
	  delete ( $self->{channels}->{$channel}->{ChanLimit} );
	  $currentmode =~ s/$1//g;
	}
	last SWITCH;
      }
      if (/k/) {
	if (/^\+(.+)/) {
	  $self->{channels}->{$channel}->{ChanKey} = $argument;
	  $currentmode .= $1;
	} else {
	  delete ( $self->{channels}->{$channel}->{ChanKey} );
	  $currentmode =~ s/$1//g;
	}
	last SWITCH;
      }
      if (/[ov]/) {
	my ($value) = 0;
	if (/\+o/) { $value = 2; }
	if (/-o/) { $value = -2; }
	if (/\+v/) { $value = 1; }
	if (/-v/) { $value = -1; }
        $self->{channels}->{$channel}->{Members}->{$argument} += $value;
        $self->{bynumeric}->{$argument}->{Channels}->{$channel} += $value;
	last SWITCH;
      }
      if (/^\+(.+)/) {
	$currentmode .= $1;
	last SWITCH;
      }
      if (/^-(.+)/) {
	$currentmode =~ s/$1//g;
	last SWITCH;
      }
    }
  }
  $self->{channels}->{$channel}->{Mode} = join("",sort(split(//,$currentmode)));
  return 1;
}

sub _channel_clearmode {
  my ($self) = shift;
  my ($channel) = u_irc( $_[0] ) || return 0;
  my ($modes) = $_[1] || return 0;

  my ($currentmodes) = $self->{channels}->{$channel}->{Mode};
  foreach (split(//,$modes)) {
    $currentmodes =~ s/$_//g;
  }
  $self->{channels}->{$channel}->{Mode} = $currentmodes;
  delete ( $self->{channels}->{$channel}->{Bans} ) if ( $modes =~ /b/ );
  foreach ( %{ $self->{channels}->{$channel}->{Members} } ) {
    if ( $modes =~ /o/ and $self->{channels}->{$channel}->{Members}->{$_} > 1 ) {
	$self->{channels}->{$channel}->{Members}->{$_} -= 2;
        $self->{bynumeric}->{$_}->{Channels}->{$channel} = $self->{channels}->{$channel}->{Members}->{$_};
    }
    if ( $modes =~ /v/ and $self->{channels}->{$channel}->{Members}->{$_} > 0 ) {
	$self->{channels}->{$channel}->{Members}->{$_} -= 1;
        $self->{bynumeric}->{$_}->{Channels}->{$channel} = $self->{channels}->{$channel}->{Members}->{$_};
    }
  }
  return 1;
}

sub _channel_ban {
  my ($self) = shift;
  my ($channel) = u_irc( $_[0] ) || return 0;
  my ($operation) = $_[1] || return 0;
  my ($banmask) = $_[2] || return 0;
  my ($who) = $_[3] || return 0;
 
  if ($operation eq "+b") { 
    $self->{channels}->{$channel}->{Bans}->{$banmask}->{Time} = time();
    $self->{channels}->{$channel}->{Bans}->{$banmask}->{Who} = $who;
  } else {
    delete ( $self->{channels}->{$channel}->{Bans}->{$banmask} );
  }
  return 1;
}

sub _channel_burst {
  my ($self) = shift;
  my ($who) = shift;
  my ($args) = shift;

  my ($first,$second) = split(/:%/,$args);
  my (@args) = split(/ /,$first);
  push(@args,":%$second") if ( defined ($second) );
  my ($channelname,$timestamp) = @args[0..1];
  my ($channel) = u_irc ( $channelname );
  # Kludge channel timestamp here
  if ( exists $self->{channels}->{$channel} ) {
    if ( $timestamp < $self->{channels}->{$channel}->{TimeStamp} ) {
	$self->{channels}->{$channel}->{TimeStamp} = $timestamp;
	$self->{burst_channels}->{$channel}->{TimeStamp} = $timestamp;
    }
  } else {
    $self->{channels}->{$channel}->{Channel} = $channelname;
    $self->{channels}->{$channel}->{TimeStamp} = $timestamp;
  }
  my ($channelmodes) = "";
  for (my $i = 2; $i <= $#args; $i++) {
    SWITCH: {
	if ($args[$i] =~ /^\+(.+)/) {
	  $channelmodes = $1;
	  $self->{channels}->{$channel}->{Mode} = $channelmodes;
          my ($l) = index ( $1, "l" );
          my ($k) = index ( $1, "k" );
	  SWITCH2: {
            if ( $l > $k and $k != -1 ) {
	      $i++;
              $self->{channels}->{$channel}->{ChanKey} = $args[$i];
	      $i++;
              $self->{channels}->{$channel}->{ChanLimit} = $args[$i];
              last SWITCH2;
            }
            if ( $l > $k and $k == -1 ) {
	      $i++;
              $self->{channels}->{$channel}->{ChanLimit} = $args[$i];
              last SWITCH2;
            }
            if ( $k > $l and $l != -1 ) {
	      $i++;
              $self->{channels}->{$channel}->{ChanLimit} = $args[$i];
	      $i++;
              $self->{channels}->{$channel}->{ChanKey} = $args[$i];
              last SWITCH2;
            }
            if ( $k > $l and $l == -1 ) {
	      $i++;
              $self->{channels}->{$channel}->{ChanKey} = $args[$i];
              last SWITCH2;
            }
	  }
	  last SWITCH;
	}
	if ($args[$i] =~ /^:%(.+)$/) {
	  foreach (split(/ /,$1)) {
	    $self->_channel_ban($channel,"+b",$_,$who);
	  }
	  last SWITCH;
	}
	# Hey, it must be a list of nicks then :o)
	my ($lastmodes);
	foreach (split(/,/,$args[$i])) {
	  my ($numeric,$modes) = split(/:/);
	  if (defined ($modes)) {
		$lastmodes = $modes;
	  }
	  # Add nick here
	  my ($value) = 0;
	  SWITCH2: {
	    if ( $lastmodes eq "ov" ) {
		$value = 3;
		last SWITCH2;
	    }
	    if ( $lastmodes eq "o" ) {
		$value = 2;
		last SWITCH2;
	    }
	    if ( $lastmodes eq "v" ) {
		$value = 1;
		last SWITCH2;
	    }
          }
	  $self->{channels}->{$channel}->{Members}->{$numeric} = $value;
	  $self->{bynumeric}->{$numeric}->{Channels}->{$channel} = $value;
	}
    }
  }
}

sub _burst_create {
  my ($self) = shift;

  # First a list of nicks
  foreach ( keys %{ $self->{bynumeric} } ) {
    $self->{burst_nicks}->{$_}->{Numeric} = $self->{bynumeric}->{$_}->{Numeric};
    $self->{burst_nicks}->{$_}->{NickName} = $self->{bynumeric}->{$_}->{NickName};
    $self->{burst_nicks}->{$_}->{UserName} = $self->{bynumeric}->{$_}->{UserName};
    $self->{burst_nicks}->{$_}->{HostName} = $self->{bynumeric}->{$_}->{HostName};
    $self->{burst_nicks}->{$_}->{IPAddr} = $self->{bynumeric}->{$_}->{IPAddr};
    $self->{burst_nicks}->{$_}->{UMode} = $self->{bynumeric}->{$_}->{UMode};
    $self->{burst_nicks}->{$_}->{AuthName} = $self->{bynumeric}->{$_}->{AuthName};
    $self->{burst_nicks}->{$_}->{IRCName} = $self->{bynumeric}->{$_}->{IRCName};
    $self->{burst_nicks}->{$_}->{TimeStamp} = $self->{bynumeric}->{$_}->{TimeStamp};
  }
  # And now a list of channels
  foreach ( keys %{ $self->{channels} } ) {
     $self->{burst_channels}->{$_}->{Channel} = $self->{channels}->{$_}->{Channel};
     $self->{burst_channels}->{$_}->{TimeStamp} = $self->{channels}->{$_}->{TimeStamp};
     $self->{burst_channels}->{$_}->{Mode} = $self->{channels}->{$_}->{Mode} if ( exists $self->{channels}->{$_}->{Mode} );
     $self->{burst_channels}->{$_}->{ChanKey} = $self->{channels}->{$_}->{ChanKey} if ( exists $self->{channels}->{$_}->{ChanKey} );
     $self->{burst_channels}->{$_}->{ChanLimit} = $self->{channels}->{$_}->{ChanLimit} if ( exists $self->{channels}->{$_}->{ChanLimit} );
     foreach my $ban ( keys %{ $self->{channels}->{$_}->{Bans} } ) {
	push(@{ $self->{burst_channels}->{$_}->{Bans} },$ban);
     }
     foreach my $user ( keys %{ $self->{channels}->{$_}->{Members} } ) {
	$self->{burst_channels}->{$_}->{Members}->{$user} = $self->{channels}->{$_}->{Members}->{$user};
     }
  }
  return 1;
}

sub _burst_info {
  my ($self) = shift;
  my (@burst);
  my (@mode) = ( "","v","o","ov" );

  # Return an array of correctly formatted lines suitable for spewing at our uplink
  # As per P10 protocol nicks come first
  foreach ( keys %{ $self->{burst_nicks} } ) {
    my ($burstline) = "N " . $self->{burst_nicks}->{$_}->{NickName} . " ";
    $burstline .= "1 " . $self->{burst_nicks}->{$_}->{TimeStamp} . " ";
    $burstline .= $self->{burst_nicks}->{$_}->{UserName} . " " . $self->{burst_nicks}->{$_}->{HostName} . " ";
    $burstline .= "+" . $self->{burst_nicks}->{$_}->{UMode} . " " if ( defined ($self->{burst_nicks}->{$_}->{UMode}) );
    $burstline .= $self->{burst_nicks}->{$_}->{AuthName} . " " if ( defined ($self->{burst_nicks}->{$_}->{AuthName}) );
    $burstline .= $self->{burst_nicks}->{$_}->{IPAddr} . " " . $_ . " :";
    $burstline .= $self->{burst_nicks}->{$_}->{IRCName} if ( defined ($self->{burst_nicks}->{$_}->{IRCName}) );
    push(@burst,$burstline);
  }
  # Followed by channels
  foreach ( keys %{ $self->{burst_channels} } ) {
    my ($burstline) = "B " . $self->{burst_channels}->{$_}->{Channel} . " " . $self->{burst_channels}->{$_}->{TimeStamp} . " ";
    $burstline .= "+" . $self->{burst_channels}->{$_}->{Mode} . " " if ( defined ($self->{burst_channels}->{$_}->{Mode}) );
    $burstline .= $self->{burst_channels}->{$_}->{ChanKey} . " " if ( defined ($self->{burst_channels}->{$_}->{ChanKey}) );
    $burstline .= $self->{burst_channels}->{$_}->{ChanLimit} . " " if ( defined ($self->{burst_channels}->{$_}->{ChanLimit}) );
    my ($lastmode) = 0; my (@users);
    foreach my $member ( sort { $self->{burst_channels}->{$_}->{Members}->{$a} <=> $self->{burst_channels}->{$_}->{Members}->{$b} } keys %{ $self->{burst_channels}->{$_}->{Members} } ) {
      my ($user) = $member;
      if ($self->{burst_channels}->{$_}->{Members}->{$member} > $lastmode) {
	$user .= ":" . $mode[$self->{burst_channels}->{$_}->{Members}->{$member}];
      }
      push(@users,$user);
    }
    $burstline .= join(",",@users) . " ";
    my ($bans) = join(" ",@{ $self->{burst_channels}->{$_}->{Bans} }) if ( defined ($self->{burst_channels}->{$_}->{Bans}) );
    $burstline .= ":%" . $bans if ( defined ($bans) );
    push(@burst,$burstline);
  }
  return @burst;
}

sub _burst_destroy {
  my ($self) = shift;

  delete ( $self->{burst_nicks} );
  delete ( $self->{burst_channels} );
  return 1;
}

sub _dump_state {
  my ($self) = shift;
  my (@mode) = ( '','+','@','@+' );
  my (@results);

  # servers
  foreach ( keys %{ $self->{servers_numeric} } ) {
    push ( @results, $_ . " " . $self->{servers_numeric}->{$_}->{Name} . " " . $self->{servers_numeric}->{$_}->{Hops} . " " . $self->{servers_numeric}->{$_}->{Link} );
  }
  # users
  foreach ( keys %{ $self->{bynumeric} } ) {
    push ( @results, $_ . " " . $self->{bynumeric}->{$_}->{NickName} );
  }
  # channels
  foreach ( keys %{ $self->{channels} } ) {
    my ($line) = $_ . " ";
    foreach my $member ( keys %{ $self->{channels}->{$_}->{Members} } ) {
      $line .= $mode[$self->{channels}->{$_}->{Members}->{$member}] . $self->{bynumeric}->{$member}->{NickName};
    }
    push ( @results, $line );
  }
  return @results;
}

# Public Methods

sub cmd_token {
  my ($self) = shift;
  my ($command) = uc ( $_[0] ) || return undef;

  return $cmd2token{$command};
}

sub server_numeric {
  my ($self) = shift;
  my ($name) = shift || return undef;

  return $self->{servers_name}->{$name}->{Numeric};
}

sub server_name {
  my ($self) = shift;
  my ($numeric) = shift || return undef;

  return $self->{servers_numeric}->{$numeric}->{Name};
}

sub server_clients {
  my ($self) = shift;
  my ($server) = shift || return 0;

  my (@numerics) = grep /^$server/, keys %{ $self->{bynumeric} };

  return scalar( @numerics );
}

sub server_link {
  my ($self) = shift;

  return $self->{serverlink};
}

sub nick_info {
  my ($self) = shift;
  my ($numeric) = shift || return undef;
  my (@returnvalues);
  
  $returnvalues[0] = $self->{bynumeric}->{$numeric}->{NickName};
  $returnvalues[1] = $self->{bynumeric}->{$numeric}->{UserName};
  $returnvalues[2] = $self->{bynumeric}->{$numeric}->{HostName};
  $returnvalues[3] = $self->{bynumeric}->{$numeric}->{IRCName};
  $returnvalues[4] = $self->{bynumeric}->{$numeric}->{UMode};
  $returnvalues[5] = $self->{bynumeric}->{$numeric}->{AuthName};

  return @returnvalues;
}

sub nick_numeric {
  my ($self) = shift;
  my ($nickname) = u_irc( $_[0] ) || return undef;

  return $self->{bynickname}->{$nickname}->{Numeric};
}

sub nick_name {
  my ($self) = shift;
  my ($numeric) = $_[0] || return undef;

  return $self->{bynumeric}->{$numeric}->{NickName};
}

sub nick_channels {
  my ($self) = shift;
  my ($numeric) = shift || return undef;
  my (@returnvalues);

  foreach ( keys %{ $self->{bynumeric}->{$numeric}->{Channels} } ) {
    SWITCH: {
	if ( $self->{bynumeric}->{$numeric}->{Channels}->{$_} == 1 ) {
	  push(@returnvalues,"+$_");
	  last SWITCH;
	}
	if ( $self->{bynumeric}->{$numeric}->{Channels}->{$_} >= 2 ) {
	  push(@returnvalues,"\@$_");
	  last SWITCH;
	}
	push(@returnvalues,"$_");
    }
  }
  return @returnvalues;
}

sub channel_exists {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return undef;

  if ( defined ( $self->{channels}->{$channel} ) ) {
    return $self->{channels}->{$channel}->{TimeStamp};
  }
  return undef;
}

sub is_channel_operator {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return 0;
  my ($numeric) = $_[1] || return 0;

  if ( defined ( $self->{channels}->{$channel}->{Members}->{$numeric} ) and $self->{channels}->{$channel}->{Members}->{$numeric} >= 2 ) {
    return 1;
  } else {
    return 0;
  }
}

sub has_channel_voice {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return 0;
  my ($numeric) = $_[1] || return 0;

  if ( defined ( $self->{channels}->{$channel}->{Members}->{$numeric} ) and $self->{channels}->{$channel}->{Members}->{$numeric} == 2 or $self->{channels}->{$channel}->{Members}->{$numeric} == 3 ) {
    return 1;
  } else {
    return 0;
  }
}

sub is_operator {
  my ($self) = shift;
  my ($numeric) = $_[0] || return 0;

  if ( defined ( $self->{bynumeric}->{$numeric}->{UMode} ) and $self->{bynumeric}->{$numeric}->{UMode} =~ /o/ ) {
    return 1;
  } else {
    return 0;
  }
}

sub has_account_set {
  my ($self) = shift;
  my ($numeric) = $_[0] || return undef;

  return $self->{bynumeric}->{$numeric}->{AuthName}
}

sub channel_banlist {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return undef;

  return keys %{ $self->{channels}->{$channel}->{Bans} };
} 

sub nick_long_form {
  my ($self) = shift;
  my ($numeric) = $_[0] || return undef;

  if ( defined ( $self->{bynumeric}->{$numeric} ) ) {
    return $self->{bynumeric}->{$numeric}->{NickName} . "!" . $self->{bynumeric}->{$numeric}->{UserName} . "\@" . $self->{bynumeric}->{$numeric}->{HostName};
  } else {
    return undef;
  }
}

sub channels_list {
  my ($self) = shift;
  my (@channels);

  foreach ( keys %{ $self->{channels} } ) {
    push (@channels,$self->{channels}->{$_}->{Channel});
  }
  return @channels;
}

sub irc_network_stats {
  my ($self) = shift;
  my (@results);

  # Number of servers
  push (@results,scalar( keys %{ $self->{servers_numeric} } ));
  # Number of users
  push (@results,scalar( keys %{ $self->{bynumeric} } ));
  # Number of channels
  push (@results,scalar( keys %{ $self->{channels} }));
  return @results;
}

sub channel_users {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return undef;
  
  return keys %{ $self->{channels}->{$channel}->{Members} };
}

sub network_users {
  my ($self) = shift;

  return keys %{ $self->{bynumeric} };
}

sub channel_mode {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return undef;

  return $self->{channels}->{$channel}->{Mode};
}

sub channel_limit {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return undef;

  return $self->{channels}->{$channel}->{ChanLimit};
}

sub channel_key {
  my ($self) = shift;
  my ($channel) = u_irc ( $_[0] ) || return undef;

  return $self->{channels}->{$channel}->{ChanKey};
}

sub decimal_to_base64 {
  my ($self) = shift;
  my ($number) = $_[0] || return undef;
  my ($output) = $_[1] || 2;

  if ( $number =~ /^[0-9]*$/ ) {
    return dectobase64($number,$output);
  } else {
    return undef;
  }
}

sub base64_to_decimal {
  my ($self) = shift;
  my ($base64) = $_[0] || return undef;

  if ( $base64 =~ /^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\[\]]*$/ ) {
    return base64todec($base64);
  } else {
    return undef;
  }
}

1;
__END__

=head1 NAME

POE::Component::IRC::Service::P10 - a fully event-driven IRC services module for P10 compliant networks.

=head1 SYNOPSIS

  use POE::Component::IRC::Service::P10;

  # Do this when you create your sessions. 'IRC-Service' is just a
  # kernel alias to christen the new IRC connection with. Second argument
  # must be a hash reference with your options.

  my (%hash) =  { ServerNumeric     => 3,
		  ServerName        => 'services.lamenet.org',
		  ServerDesc	    => 'Services for LameNET',
		  RemoteServer	    => 'hub.lamenet.org',
		  ServerPort        => 4400,
		  Password          => 'password',
		  EventMode	    => 0              # Default is 1
		  Reconnect	    => 1	      # Default is 0
		  Version	    => 'LameServ0.1a' # Defaults to module name and version
                };

  my ($object) = POE::Component::IRC::Service::P10->new('IRC-Service', \%hash ) or die "Oh noooo! $!";

  # Do stuff like this from within your sessions. This line tells the
  # connection named "IRC-Service" to send your session the following
  # events when they happen.

  $kernel->post('IRC-Service', 'register', qw(connected msg public nick server));

  # You can guess what this line does.

  $kernel->post('IRC-Service', 'connect');

  # Add a services identity to the network

  $kernel->post('IRC-Service' => 'addnick',
		{ NickName    => 'Lame',
		  Umode	      => '+odk',
		  Description => 'Lame Services Bot', } );

  # Determine if a user is an IRCop

  if ( $object->is_operator( $numeric ) ) {
    $kernel->post( 'IRC-Service' => privmsg => $botnumeric => $numeric => "Access granted." );
  }

=head1 DESCRIPTION

POE::Component::IRC::Service::P10 is a POE component which
acts as an easily controllable IRC Services server for your other POE
components and sessions. You create an IRC Services component and tell it what
events your session cares about and where to connect to, and it sends
back interesting IRC events when they happen. You make the client do
things by sending it events.

The component implements the basic functionality of an IRC server and will track 
state changes ( such as new users, channels, etc. ) on the IRC network. To access 
this state the component returns an object when created and there are a hat-full of
methods to query the state from your POE sessions. This also means that it is not necessary
to use 'connect' to link to your uplink server before creating nicks and joining channels, as
the component will track all these changes and during connection to the IRC network will "burst"
in the necessary information. See the events section below for more information about this.

[Note that this module requires a lot of familiarity with the details of the
IRC protocol. I'd advise you to read up on the gory details of RFC 1459 
E<lt>http://cs-pub.bu.edu/pub/irc/support/rfc1459.txtE<gt> before starting.
Some knowledge of the P10 IRC Server-to-Server protocol would also be advisable. 
Consult E<lt>http://www.xs4all.nl/~carlo17/irc/P10.htmlE<gt> for details.]

In order to write a POE program with POE::Component::IRC::Service::P10:
Create your session(s) and an
alias for a new POE::Component::IRC::Service::P10 server. (Conceptually, it helps if
you think of them as little IRC servers.) In your session's _start
handler, send the IRC service a 'register' event to tell it which IRC
events you want to receive from it. Send it a 'connect' event at some
point to tell it to join the IRC network, and it should start sending you
interesting events every once in a while. Use the 'addnick' event to add
an IRC client to your "server". The IRC Service accepts two different sets of
events, server and client. Server events are commands that are issued by (heh) 
the server and client events are commands issued by clients. 

  # Example of a client command:

  $kernel->post( 'IRC-Service', 'join', 'ADAAA' , '#LameNET' );

  # Example of a server command:

  $kernel->post( 'IRC-Service', 'opmode', '#LameNET', '+o', 'ADAAA' );

Basically, client commands require a source nickname for the command, eg. 
it doesn't make sense for a server to "join" a channel.

The long version is the rest of this document.

=head1 METHODS

=over

=item new

Takes two arguments: a name (kernel alias) which this new connection
will be known by; a hash reference with the configuration for your new
shiny component. The hash reference must contain the following mandatory 
items: "ServerNumeric", an unique number between 0-4095 or Base64 equivalent
(read the P10 specification to clue up on this); "ServerName", the name your IRC Service will
be called; "RemoteServer", the DNS or
IP address of your uplink/hub server; "ServerPort", the port to connect to on your uplink/hub
server; "Password", the password required to link to uplink/hub server. The following are optional: 
"ServerDesc", a brief description of your IRC Service;
"LocalAddr", which local IP address on a multihomed box to connect as; "PingFreq", the frequency in seconds
that we should send PINGs to our uplink server; "EventMode", this specifies what events the component should
track, setting to 0 will only track user or nick events, whilst setting to 1 will track all events. Default is 1.
If you aren't interested in channel events ( say you are writing a NickServ ) set this to 0 and save some overhead :o)
"Version", specify a software version that will get returned to users who use /VERSION, defaults to the name of the module;
"Reconnect", determines whether the module automatically reconnects to the IRC network on disconnects and socket errors, default is 0 ie. don't reconnect. There are also two additional settings "HIS_SERVERNAME" and "HIS_SERVERINFO", which provide
the "name" of the server shown to users on a /WHOIS of another user and the server description, no defaults.

Returns an object which can be used to access the IRC network state with the following methods:

=item cmd_token

One argument, an IRC command. Returns the appropriate tokenised command.

=item decimal_to_base64

One argument, a number, and an optional argument, the length of BASE64 number to return, defaults to 2. 
Returns the appropriate BASE64 representation of that number.

=item base64_to_decimal

One argument, a BASE64 representation. Returns the corresponding decimal number.

=item server_numeric

Takes one argument, a server name. Returns the corresponding server numeric.

=item server_name

Takes one argument, a server numeric. Returns the corresponding server name.

=item server_clients

Takes one argument, a server numeric. Returns a list of the client numerics.

=item nick_info

Takes on argument, a client numeric. Returns an array consisting of [0] NickName, [1] UserName, [2] HostName,
[3] IRCName, [4] UMode and [5] AuthName ( if applicable ).

=item nick_long_form

Takes one argument, a client numeric. Returns <nickname>!<username>@<hostname>

=item nick_numeric

Takes one argument, a nickname. Returns the corresponding client numeric.

=item nick_name

Takes one argument, a client numeric. Returns the corresponding nickname.

=item nick_channels

Takes one argument, a client numeric. Returns an array of channels that the client is joined, preceeded with '@' or '+' for ops or voice.

=item channel_exists

Takes one argument, a channel name ( with # prefix ). Returns the timestamp of the channel if it exists.

=item is_channel_operator

Takes two arguments, channel name and a client numeric. Returns 1 if that user is a channel operator on that channel, or 0 otherwise.

=item has_channel_voice

Takes two arguments, channel name and a client numeric. Returns 1 if that user has voice on that channel, or 0 otherwise.

=item is_operator

Takes one argument, a client numeric. Returns 1 if that user has umode +o or 0 otherwise.

=item has_account_set

Takes one argument, a client numeric. Returns the authname/account if set, undef otherwise.

=item channel_banlist

Takes one argument, a channel name. Returns a list of the current ban masks set on that channel.

=item channels_list

No arguments. Returns a list of all the channels on the IRC network.

=item irc_network_stats

No arguments. Returns an array made up of [0] number of servers on the network, [1] number of channels on network, [2] number of clients on the network.

=item channel_users

Takes one argument, a channel name. Returns a list of client numerics, who are joined to that channel.

=item channel_mode

Takes one argument, a channel name. Returns the current channel mode.

=item channel_limit

Takes one argument, a channel name. Returns the current channel limit +l if set.

=item channel_key

Takes one argument, a channel name. Returns the current channel key +k if set.

=back

=head1 INPUT

How to talk to your new IRC Services component... here's the events we'll accept.

=head2 Important Commands

=over

=item connect

Takes no arguments. This event tells the IRC Services client to connect to a
new/different hub and join an IRC network. If it has a connection already open, it'll close
it gracefully before reconnecting. You will receieve an irc_p10_connected event, but it is safer
to wait for an irc_p10_eob_ack from the module before assuming that the connection is properly made.

=item addnick

Takes one argument: a hash reference of attributes for the new service client 
(see the L<SYNOPSIS> section of this doc for an example). This event adds a new 
client to the IRC Service server. Multiple clients are allowed. Expect to receive
an appropriate irc_p10_nick event for the new client, from which you can derive the 
clients numeric token. Possible attributes for the new client are "NickName", (duh) 
the nickname this client will appear as on the IRC network (only required attribute); 
"UserName", the user part of ident@host (default is nick); 
"HostName", the host part of ident@host (default is the name of the server);
"AuthName", the network "account" this client will use (see P10 specification for details);
"Umode", the user modes this client will have (defaults to +odk);
"Description", equivalent to the IRCName (default server description);

=item register

Takes N arguments: a list of event names that your session wants to
listen for, minus the "irc_p10_" prefix. So, for instance, if you just
want a bot that keeps track of which people are on a channel, you'll
need to listen for CREATEs, JOINs, PARTs, QUITs, and KICKs to people on the
channel you're in. You'd tell POE::Component::IRC::Service::P10 that you want those
events by saying this:

  $kernel->post( 'IRC-Service', 'register', qw(join part quit kick) );

Then, whenever people enter or leave a channel your bot is on (forcibly
or not), your session will receive events with names like "irc_p10_join",
"irc_p10__kick", etc., which you can use to update a list of people on the
channel.

Registering for C<'all'> will cause it to send all IRC-related events to
you; this is the easiest way to handle it.

=item unregister

Takes N arguments: a list of event names which you I<don't> want to
receive. If you've previously done a 'register' for a particular event
which you no longer care about, this event will tell the IRC
connection to stop sending them to you. (If you haven't, it just
ignores you. No big deal.)

=back

=head2 Server initiated commands

These are commands that come from the IRC Service itself and not from clients.

=over

=item account

Sets a user's account name. Takes two arguments, the target user's numeric token and
the account name. If a user's account name field is set, the ircd knows the user is "logged in",
and can have a "is logged in as" or "is authed as" line in the whois reply.

=item clearmode

Clears the specified channel modes. Takes two arguments, the channel to clear and a string of
chars representing the modes to clear. Eg. "o" would clear all people with +o on a channel, "b" would
clear all bans, etc.

=item gline

Sets or removes a GLINE to the IRC network. A GLINE prevents matching users from connecting to the
network. Implemented as if the IRC Service is a U: lined server, so ircd must be configured 
accordingly. Takes four arguments, the target for the gline which can be * (for all servers) or
a server numeric; the mask to gline [!][-|+]<mask> the presence of the ! prefix means "force", 
+ means add/activate, - means remove/deactivate; the duration of the gline, ie. the time to expire the
gline in seconds since epoch ( ie. output from time() ); the reason for the gline.
Mask may be a user@host mask, or a channel name. In the later case (mask starts with a # or &) it is a "BADCHAN". 
A BADCHAN prevents users from joining a channel with the same name.

=item jupe

A jupe prevents servers from joining the network. Takes five arguments, the target for the jupe,
either * for all servers or a server numeric; what to jupe [!][-|+]<server>, ! is force, + activate jupe, 
- deactivate jupe; the duration of the jupe, ie. the time to expire the jupe in seconds since epoch; the
last modification timestamp, ie. the output of time(); the reason for the jupe.

=item kill

Server kill :) Takes two arguments, the client numeric of the victim; the reason for the kill.
If the numeric specified matches one of the IRC Service's internal clients, that client will
be automatically removed.

=item opmode

Opmode forces channel mode changes, resistance is futile. Changes are not bounced or denied.
Takes at least one argument: the mode changes to effect, as a single string (e.g.,
"+sm-p+o"), and any number of optional operands to the mode changes
(nicks, hostmasks, channel keys, whatever.) Or just pass them all as one
big string and it'll still work, whatever. Note, nicks must be specified as numerics.

$kernel->post( 'IRC-Service' => opmode => "#LameNET" => "+o" => "ZaAAA" );

=item squit

This will disconnect the IRC Service from its uplink/hub server. Expect to receive an
"irc_p10_disconnected" event. Takes no arguments.

=item shutdown

This will shutdown the IRC Service.

=item sl_server

Send a raw server command. Exercise extreme caution. Takes one argument, a string 
representing the raw command that the server will send. The module prepends the
appropriate server numeric for you, so don't worry about that. Note, IRC commands must be 
specified as tokenised equivalents as per P10 specification.

$kernel->post( 'IRC-Service' => sl_server => "OM #LameNET +o ZaAAA" );

=back

=head2 Client initiated commands

These are commands that come from clients on the IRC Service.

=over

=item ctcp and ctcpreply

Sends a CTCP query or response to the nick(s) or channel(s) which you
specify. Takes 3 arguments: the numeric of the client who is sending the command;
the numeric or channel to send a message to
(use an array reference here to specify multiple recipients), and the
plain text of the message to send (the CTCP quoting will be handled
for you).

=item invite

Invites another user onto an invite-only channel. Takes 3 arguments:
the numeric of the inviting client, the nick of the user you wish to admit, 
and the name of the channel to invite them to. Note that the invitee is a 
nickname and not a numeric.

=item join

Tells a specified client to join a single channel of your choice. Takes
at least two args: the numeric of the client that you want to join, 
the channel name (required) and the channel key
(optional, for password-protected channels).

=item mode

Request a mode change on a particular channel or user. Takes at least
two arguments: the mode changing client's numeric, 
the mode changes to effect, as a single string (e.g.,
"+sm-p+o"), and any number of optional operands to the mode changes
(numerics, hostmasks, channel keys, whatever.) Or just pass them all as one
big string and it'll still work, whatever. Nicks should be specified as 
numerics.

=item nick

Allows you to change a client's nickname. Takes two arguments: the 
numeric of the client who wishes to change nickname and the
new username that you'd like to be known as.

=item notice

Sends a NOTICE message to the nick(s) or channel(s) which you
specify. Takes 3 arguments: the numeric of the issuing client, 
the nick or channel to send a notice to
(use an array reference here to specify multiple recipients), and the
text of the notice to send.

=item part

Tell a client to leave the channels which you pass to it. Takes
any number of arguments: the numeric of the client followed by the 
channel names to depart from.

=item privmsg

Sends a public or private message to the nick(s) or channel(s) which
you specify. Takes 3 arguments: the numeric of the issuing client, 
the numeric or channel to send a message
to (use an array reference here to specify multiple recipients), and
the text of the message to send.

=item quit

Tells the IRC service to remove a client. Takes one argument: 
the numeric of the client to disconnect; and one optional argument:
some clever, witty string that other users in your channels will see
as you leave. The IRC Service will automatically remove the client from
its internal list of clients.

=item sl_client

Send a raw client command. Exercise extreme caution. Takes one argument, a string 
representing the raw command that the server will send. Unlike "sl_server" you must specify 
the full raw command prefixed with the appropriate client numeric. Note, IRC commands must be 
specified as tokenised equivalents as per P10 specification.

$kernel->post( 'IRC-Service' => sl_client => "AbZAd OM #LameNET +o ZaAAA" );

=item stats

Returns some information about a server. Kinda complicated and not
terribly commonly used, so look it up in the RFC if you're
curious. Takes as many arguments as you please, but the first argument
must be the numeric of a client. Target servers must be specified by numeric token.

=back

=head1 OUTPUT

The events you will receive (or can ask to receive) from your running
IRC component. Note that all incoming event names your session will
receive are prefixed by "irc_p10_", to inhibit event namespace pollution
( and Dennis had already taken irc_ :p ).

If you wish, you can ask the client to send you every event it
generates. Simply register for the event name "all". This is a lot
easier than writing a huge list of things you specifically want to
listen for.

The module will deal with automatically irc_p10_stats, irc_p10_version, irc_p10_whois and 
irc_p10_ping. The module will also track all events relating to server joins, nicks and channels.

=over

=item Miscellaneous events

Events such as join, part, etc. should be same as POE::Component::IRC. See that documentation for details.

=item All numeric events (see RFC 1459)

Most messages from IRC servers are identified only by three-digit
numeric codes with undescriptive constant names like RPL_UMODEIS and
ERR_NOTOPLEVEL. (Actually, the list of codes in the RFC is kind of
out-of-date... the list in the back of Net::IRC::Event.pm is more
complete, and different IRC networks have different and incompatible
lists. Ack!) As an example, say you wanted to handle event 376
(RPL_ENDOFMOTD, which signals the end of the MOTD message). You'd
register for '376', and listen for 'irc_p10_376' events. Simple, no? ARG0
is the numeric of the server which sent the message. ARG1 is the text of
the message.

=back

=head1 AUTHOR

Chris Williams, E<lt>chris@bingosnet.co.uk<gt>

Based on a hell of lot of POE::Component::IRC written by
Dennis Taylor, E<lt>dennis@funkplanet.comE<gt>

=head1 MAD PROPS

Greatest of debts to Dennis Taylor, E<lt>dennis@funkplanet.comE<gt> for
letting me "salvage" POE::Component::IRC to write this module.

The wonderful people in #poe @ irc.perl.org for invaluable assistance and putting up with my
n00b questions.

And to ^kosh and FozzySon and others from #jeditips @ irc.quakenet.org for allowing me to 
inflict my coding on them :)

=head1 SEE ALSO

RFC 1459, http://www.irchelp.org/, http://poe.perl.org/,
http://www.xs4all.nl/~beware3/irc/bewarep10.html

=cut

