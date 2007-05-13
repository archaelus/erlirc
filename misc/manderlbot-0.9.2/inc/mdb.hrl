%mdb.hrl --------------------------
%% Parameters to start a bot
-record(params, {server="",
		 port=0,
		 password="",
		 channel="",
		 nickserv_password="",
		 nickname="",
		 realname=""}).

%% ----------------------
%% Used to keep track of 
%% who is connecting to
%% the channel
-record(spy, {nickname="",
	      user="",
	      last_phrase_date={},
	      last_phrase="",
	      join_date={},
	      quit_date={},
	      quit_reason=""}).

%% ----------------------
%% Used by mdb_bot.erl

%% Parsed incoming IRC data
-define(nodata, '_').
-record(data, {body           = ?nodata,
               header_from    = ?nodata,
               header_op      = ?nodata,
               header_to      = ?nodata,
	       header_options = ?nodata}).

-define(TIME, 2000).
-define(RNDTIME, 3000).

%% Bot process state
-record(state, {bot_pid = "",
		channel = "",
		nickname = "",
		passwd = "",
		controler = "",
	        socket = "",
		buffer = <<>>,
		behaviours = [],
	        bot_state={},
		date={},
		host = "",
		port = "",
		joined = false,
		mode = unmuted % will either be muted or unmuted atom
	       }).

%% Behaviour description
-record(behaviour, {id="",
                    pattern,
		    exclude_pattern,
		    function,
		    data}).

%% ----------------------
%% Used by mdb_srv.erl

%% Use to keep trace of the networks that we can connect to
-record(network, {network_id=0,
		  network_name="",
                  server="",
		  ip_port=0}).

%% Use to keep track of running bots
-record(bot, {bot_id,
	      network_id,
	      password,
	      channel,
	      nickserv_password,
              nickname,
	      real_name,
	      botpid,
	      %% socket,
	      date}).

%% State of the server
-record(srv_state, {networks=[],
		bots=[]}).

-record(search_state, {requests = []}).

-record(search_param, {server,
				port,
				type}).
