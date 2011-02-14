%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> puts together the mechanism
%% to connect to an XMPP server, using various authentication
%% mechanisms and network layers.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>
%%
%% TODO: Rewrite the following text into a comprehensive documentation.
%%   explain priority matching of iq query reply. Illustration with echo_client.
%%   Illustration with an Erlang/OTP example.
%%
%% TODO: - manage timeouts
%%       - Callback should not be module, but anonymous or named
%%       functions
%%       - Do function callback need to have priority ?
%%
%% Currently thinking about a design purely based on sending back messages to
%% the client process: Would allow selection of the order of packets (whic
%% seems more powerful)
%% Sending a packet is async and return the packet id
%% If this is an IQ, the next reply can be a blocking receive on an IQ result
%% with the same refid.
%% It could be a generic receive, getting packets in order.
%% TODO: Add unregister account ?

%%
%%
%% Initial support for sasl authentication is up and running.
%% For now, only PLAIN is supported.
%% Example:
%%    S = exmpp_session:start_link({1,0}),  %{1,0} is the stream version. You must supply {1,0} to be able to perform sasl authentication.
%%    JID = exmpp_jid:make("user1", "localhost", "user1"),
%%    exmpp_session:auth_basic_digest(S, JID, "user1"),   %% it is still called auth_basic_digest.. I must change that.
%%    {ok, StreamID, Features} = exmpp_session:connect_TCP(S, "localhost", 5222),
%%    io:format("StreamID ~p Features:  \n", [StreamID]),
%%    ok = exmpp_session:login(S, "PLAIN"),   %specify "PLAIN" as sasl login method
%%     ...
%%

-module(exmpp_session).
-behaviour(gen_fsm).

%% XMPP Session API:
-export([start/0, start_link/0, start/1, start_link/1,start_debug/0, stop/1]).
-export([auth_basic/3, auth_basic_digest/3,
	 connect_SSL/3, connect_SSL/4,
	 connect_TCP/3, connect_TCP/4,
	 connect_BOSH/4,
	 register_account/2, register_account/3,
	 login/1, login/2,
	 send_packet/2,
	 set_controlling_process/2]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_info/3,
	 handle_event/3,
	 handle_sync_event/4,
	 terminate/3]).

%% States
-export([setup/3, wait_for_stream/2, wait_for_stream/3,
	 stream_opened/2, stream_opened/3,
	 wait_for_sasl_response/2,
	 wait_for_stream_features/2,
	 wait_for_bind_response/2,
	 wait_for_session_response/2,
	 stream_error/2, stream_error/3,
	 stream_closed/2, stream_closed/3,
	 wait_for_legacy_auth_method/2,
	 wait_for_auth_result/2,
	 wait_for_register_result/2,
	 logged_in/2, logged_in/3
	]).

-include("exmpp.hrl").
-include("exmpp_client.hrl").

-record(state, {
	  auth_method = undefined,
      stream_version,
      authenticated = false,
	  domain,
	  client_pid,
	  connection = exmpp_tcp,
	  connection_ref,
	  stream_ref,
	  stream_id = false, %% XMPP StreamID (Used for digest_auth)
	  stream_error,
	  receiver_ref,
	  from_pid           %% Use by gen_fsm to handle postponed replies
	 }).

%% This timeout should match the connect timeout
-define(TIMEOUT, 5000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
%% Start the session (used to get a reference):
start() ->
    case gen_fsm:start(?MODULE, [self()], []) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.
%% Start the session (used to get a reference):
start_link() ->
    case gen_fsm:start_link(?MODULE, [self()], []) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.
start({1,0}) ->
    case gen_fsm:start(?MODULE, [self(), {1,0}], []) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.
%% Start the session (used to get a reference):
start_link({1,0}) ->
    case gen_fsm:start_link(?MODULE, [self(), {1,0}], []) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.

%% Start the session in debug mode
%% (trace events)
start_debug() ->
    case gen_fsm:start(?MODULE, [self()], [{debug,[trace]}]) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.

%% Close session and disconnect
stop(Session) ->
    catch gen_fsm:sync_send_all_state_event(Session, stop),
    ok.

%% Set authentication mode to basic (password)
auth_basic(Session, JID, Password)
  when is_pid(Session),
       is_list(Password) ->
    case exmpp_jid:is_jid(JID) of
	false -> erlang:error({incorrect_jid,JID});
	true ->
	    Auth = {basic, password, JID, Password},
	    gen_fsm:sync_send_event(Session, {set_auth, Auth})
    end.

%% Set authentication mode to basic (digest)
auth_basic_digest(Session, JID, Password)
  when is_pid(Session),
       is_list(Password) ->
    case exmpp_jid:is_jid(JID) of
	false -> erlang:error({incorrect_jid,JID});
	true ->
	    Auth = {basic, digest, JID, Password},
	    gen_fsm:sync_send_event(Session, {set_auth, Auth})
    end.



%% Initiate standard TCP XMPP server connection.
%% Shortcut for  connect_TCP(Session, Server, Port, []).
%% As the domain is not passed we expect to find it in the authentication
%% method. It should thus be set before.
%% Returns {ok,StreamId::String} | {ok, StreamId::string(), Features :: xmlel{}}
connect_TCP(Session, Server, Port) ->
	connect_TCP(Session, Server, Port, []).

%% Initiate standard TCP XMPP server connection
%% Returns {ok,StreamId::String} | {ok, StreamId::string(), Features :: xmlel{}}
%%  Option() = {local_ip, IP} | {local_port, fun() -> integer()}   bind sockets to this local ip / port.
%%      | {domain, Domain}
%% If the domain is not passed we expect to find it in the authentication
%% method. It should thus be set before.
connect_TCP(Session, Server, Port, Options)
  when is_pid(Session),
       is_list(Server),
       is_integer(Port),
       is_list(Options) ->
    case gen_fsm:sync_send_event(Session,
				 {connect_tcp, Server, Port, Options},
				 ?TIMEOUT) of
	{ok, StreamId} -> {ok, StreamId};
    {ok, StreamId, Features} -> {ok, StreamId, Features};
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.

%% Initiate HTTP Bosh XMPP server connection
%% If the domain is not passed we expect to find it in the authentication
%% method. It should thus be set before.
%% Returns {ok,StreamId::String} | {ok, StreamId::string(), Features :: xmlel{}}
%%  Options = [option()]
%%  Option() = {local_ip, IP} | {local_port, fun() -> integer()}  bind sockets to this local ip / port.

connect_BOSH(Session, URL, Server, Options)
  when is_pid(Session),
       is_list(Server),
       is_list(Options) ->
    case gen_fsm:sync_send_event(Session, {connect_bosh, URL, Server, Options},
                                 ?TIMEOUT) of
	{ok, StreamId} -> {ok, StreamId};
    {ok, StreamId, Features} -> {ok, StreamId, Features};
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.

%% Initiate SSL XMPP server connection
%% Shortcut for  connect_SSL(Session, Server, Port, []).
%% As the domain is not passed we expect to find it in the authentication
%% method. It should thus be set before.
%% Returns {ok,StreamId::String} | {ok, StreamId::string(), Features :: xmlel{}}
connect_SSL(Session, Server, Port) ->
	connect_SSL(Session, Server, Port, []).

%% Initiate SSL XMPP server connection
%% Returns {ok,StreamId::String} | {ok, StreamId::string(), Features :: xmlel{}}
%%  Options = [option()]
%%  Option() = {local_ip, IP} | {local_port, fun() -> integer()}  bind sockets to this local ip / port.
connect_SSL(Session, Server, Port, Options)
  when is_pid(Session),
       is_list(Server),
       is_integer(Port),
       is_list(Options) ->
    case gen_fsm:sync_send_event(Session,
				 {connect_ssl, Server, Port, Options},
				 ?TIMEOUT) of
	{ok, Streamid} -> {ok, Streamid};
    {ok, Streamid, Features} -> {ok, Streamid, Features};
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.

%% Try to add the session user with inband registration
%% In this case, we use the jid data provided with the auth method
%% Returns ok
register_account(Session, Password) ->
    case gen_fsm:sync_send_event(Session, {register_account, Password}) of
	ok -> ok;
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.

%% Try to add the session user with inband registration
%% The domain is implicite and depends on the opened stream
%% Returns ok
register_account(Session, Username, Password) ->
    case gen_fsm:sync_send_event(Session,
				 {register_account, Username, Password}) of
	ok -> ok;
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.


%% Login session user
%% Returns {ok, JID}
login(Session) when is_pid(Session) ->
    case gen_fsm:sync_send_event(Session, {login}) of
	{ok, JID} -> {ok, JID};
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.
login(Session, Mechanism) when is_pid(Session), is_list(Mechanism) ->
    case gen_fsm:sync_send_event(Session, {login, sasl, Mechanism}) of
	{ok, JID} -> {ok, JID};
	Error when is_tuple(Error) -> erlang:throw(Error)
    end.

%% Send any exmpp formatted packet
send_packet(Session, Packet) when is_pid(Session) ->
    case gen_fsm:sync_send_event(Session, {send_packet, Packet}) of
	Error when is_tuple(Error) -> erlang:throw(Error);
        Id -> Id
    end.

set_controlling_process(Session,Client) when is_pid(Session), is_pid(Client) ->
    case gen_fsm:sync_send_all_state_event(Session, {set_controlling_process,
						     Client}) of
	Error when is_tuple(Error) -> erlang:throw(Error);
        Id -> Id
    end.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Pid]) ->
    %% TODO: This shouldn't be needed, but see 
    %%       https://support.process-one.net/browse/EXMPP-23
    inets:start(),
    exmpp_stringprep:start(),

    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    {ok, setup, #state{client_pid=Pid, stream_version = {0,0}}}; %%if not specified, do not use version 1.0
init([Pid, Version]) ->
    inets:start(),
    exmpp_stringprep:start(),

    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    {ok, setup, #state{client_pid=Pid, stream_version = Version}}.

handle_event(tcp_closed, _StateName, State) ->
    {stop, tcp_closed, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(stop, _From, _StateName, State) ->
    Reply = ok,
    {stop, normal, Reply, State};
handle_sync_event({set_controlling_process,Client}, _From, StateName, State) ->
    Reply = ok,
    {reply,Reply,StateName,State#state{client_pid=Client}};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(Reason, _StateName, #state{connection_ref = undefined,
				     stream_ref = undefined,
				     from_pid=From}) ->
    reply(Reason, From),
    ok;
terminate(Reason, _StateName, #state{connection_ref = undefined,
				     stream_ref = StreamRef,
				     from_pid=From}) ->
    exmpp_xmlstream:stop(StreamRef),
    exmpp_xml:stop_parser(exmpp_xmlstream:get_parser(StreamRef)),
    reply(Reason, From),
    ok;
terminate(Reason, _StateName, #state{connection_ref = ConnRef,
				     connection = Module,
				     stream_ref = undefined,
				     from_pid=From}) ->
    Module:close(ConnRef),
    reply(Reason, From),
    ok;
terminate(Reason, _StateName, #state{connection_ref = ConnRef,
				     connection = Module,
				     stream_ref = StreamRef,
				     receiver_ref = ReceiverRef,
				     from_pid=From}) ->
    Module:close(ConnRef, ReceiverRef), %stop receiving data from socket
    exmpp_xmlstream:stop(StreamRef),
    exmpp_xml:stop_parser(exmpp_xmlstream:get_parser(StreamRef)),
    reply(Reason, From),
    ok.

%% Send gen_fsm reply if needed
reply(_Reply, undefined) ->
    ok;
reply(Reply, {P, _} = From) when is_pid(P) ->
    gen_fsm:reply(From, Reply);
reply(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% States
%%====================================================================
%% ---------------------------
%% Setup state: Configuration

%% Define JID and authentication method
setup({set_auth, Auth}, _From, State) when is_tuple(Auth) ->
    {reply, ok, setup, State#state{auth_method=Auth}};
setup({connect_tcp, Host, Port, Options}, From, State) ->
    case {proplists:get_value(domain, Options, undefined), State#state.auth_method} of
	{undefined, undefined} ->
	    {reply, {connect_error,
		     authentication_or_domain_undefined}, setup, State};
	{undefined, _Other} ->
	    connect(exmpp_tcp, {Host, Port, Options}, From, State);
	{Domain, _Any} ->
    	    connect(exmpp_tcp, {Host, Port, Options}, Domain, From, State)
    end;
setup({connect_bosh, URL, Host, Port}, From, State) ->
    case State#state.auth_method of
        undefined ->
            {reply, {connect_error,
                     authentication_or_domain_undefined}, setup, State};
        _Other ->
            connect(exmpp_bosh, {URL, Host, Port}, From, State)
    end;
setup({connect_ssl, Host, Port, Options}, From, State) ->
    case {proplists:get_value(domain, Options, undefined), State#state.auth_method} of
	{undefined, undefined} ->
	    {reply, {connect_error,
		     authentication_or_domain_undefined}, setup, State};
	{undefined, _Other} ->
	    connect(exmpp_ssl, {Host, Port, Options}, From, State);
	{Domain, _Any} ->
    	    connect(exmpp_ssl, {Host, Port, Options}, Domain, From, State)
    end;
setup({presence, _Status, _Show}, _From, State) ->
    {reply, {error, not_connected}, setup, State};
setup(_UnknownMessage, _From, State) ->
    {reply, {error, unallowed_command}, setup, State}.

%% ---------------------------
%% Stream negociation:

%% TODO: Defines should probably be refactored with the other parts of
%% exmpp.

%% Standard opening stream:
-define(stream,
	#xmlstreamstart{element=#xmlel{
			  ns='http://etherx.jabber.org/streams',
			  name=stream}}).
%% Standard stream error:
-define(streamerror,
	#xmlstreamelement{element=#xmlel{
			    ns='http://etherx.jabber.org/streams',
			    name=error,
			    children=[#xmlel{name=Reason} | _MoreReasons]}}).

%% Special stream error: disconnected
-define(streamdisconnected,
        #xmlstreamelement{element=#xmlel{
			    ns='http://etherx.jabber.org/streams',
			    name=error,
			    children=[#xmlcdata{cdata=  <<"Disconnected">> }]}}).

%% Standard end of stream:
-define(streamend,
        #xmlstreamend{endtag=#xmlendtag{
			ns='http://etherx.jabber.org/streams',
			name=stream}}).

%% Extract IQElement from IQ
-define(iq,
	#xmlstreamelement{
	  element=#xmlel{name=iq, attrs=Attrs}=IQElement}).
-define(iq_no_attrs,
	#xmlstreamelement{
	  element=#xmlel{name=iq}=IQElement}).

%% Used to match a presence packet in stream.
-define(presence,
	#xmlstreamelement{
	  element=#xmlel{name=presence, attrs=Attrs}=PresenceElement}).
%% Used to match a message packet in stream
-define(message,
	#xmlstreamelement{
	  element=#xmlel{name=message, attrs=Attrs}=MessageElement}).
%% To match an XMLNSElement of type Iq:
-define(iqattrs, #xmlel{name=iq, attrs=Attrs}=IQElement).
%% To match either presence or message
-define(elementattrs, #xmlel{attrs=Attrs}=Element).


%% We cannot receive API call in this state
wait_for_stream(_Event, _From, State) ->
    {reply, {error, busy_connecting_to_server}, wait_for_stream, State}.
%% TODO: Check that we receive a client stream. Need change in the
%% parsing library.

%% stream already authenticated by sasl
wait_for_stream(?stream, State = #state{authenticated = true}) ->
    {next_state, wait_for_stream_features, State};

wait_for_stream(Start = ?stream, State = #state{connection = _Module,
						connection_ref = _ConnRef,
						auth_method = _Auth,
						from_pid = From}) ->
    %% Get StreamID
    StreamId = exmpp_xml:get_attribute_as_list(Start#xmlstreamstart.element, id, ""),
    
    case exmpp_xml:get_attribute_as_list(Start#xmlstreamstart.element, version, "") of
            "" ->
                gen_fsm:reply(From, {ok,StreamId}),
                {next_state, stream_opened, State#state{from_pid=undefined,
					    stream_id = StreamId, stream_version = {0,0}}};
            "1.0" ->
                {next_state, wait_for_stream_features, State#state{stream_id =  StreamId}}
    end.

wait_for_stream_features(#xmlstreamelement{element=#xmlel{name='features'} = F}, State) ->
    #state{connection = Module, 
           connection_ref = ConnectionRef, 
           from_pid = From, 
           authenticated = Authenticated, 
           stream_id = StreamId} = State,
    case Authenticated of
        true ->
            Bind = exmpp_client_binding:bind(get_resource(State#state.auth_method)),
            Module:send(ConnectionRef, Bind),
            %%@pablo TODO:  send bind request
            {next_state, wait_for_bind_response, State};
        false ->
            gen_fsm:reply(From, {ok, StreamId, F}),
            {next_state, stream_opened, State#state{from_pid = undefined}}
    end;

wait_for_stream_features(X, State) ->
    io:format("Unknown element waiting for stream features ~p \n", [X]),
    {next_state, wait_for_stream_features, State}.
   

wait_for_bind_response(#xmlstreamelement{element = #xmlel{name ='iq'} = IQ}, State) ->
    #state{connection = Module, connection_ref = ConnectionRef} = State,
    case exmpp_iq:get_type(IQ) of
        result -> 
            JID = exmpp_client_binding:bounded_jid(IQ),  
            NewAuthMethod = {basic, sasl_anonymous, JID, undefined}, %%TODO: is this neccesary? 
            Module:send(ConnectionRef, exmpp_client_session:establish()),
            {next_state, wait_for_session_response, State#state{auth_method=NewAuthMethod}};
        _ ->
            {stop, {bind, IQ}, State}
    end.

wait_for_session_response(#xmlstreamelement{element = #xmlel{name='iq'} = IQ}, State) ->
    #state{from_pid = From} = State,
    case exmpp_iq:get_type(IQ) of
        result ->
            gen_fsm:reply(From, {ok, get_jid(State#state.auth_method)}),  %%after successful login, bind and session
            {next_state, stream_opened, State#state{from_pid = undefined}};
        _ ->
            {stop, {bind, IQ}, State}
    end.
        


%% ---------------------------
%% Between stream opening and session opening

%% Supported user commands at this stage:
%% login and register
stream_opened({login}, _From,State=#state{auth_method=undefined}) ->
    {reply, {error, auth_method_undefined}, stream_opened, State};
stream_opened({login}, From, State=#state{connection = Module,
					  connection_ref = ConnRef,
					  auth_method=Auth}) ->
    %% Retrieve supported authentication methods:
    %% TODO: Do different thing if we use basic or SASL auth
    %% For now, we consider everything is legacy (basic)
    Domain = get_domain(Auth),
    Username = get_username(Auth),
    Module:send(ConnRef,
 		exmpp_client_legacy_auth:request_with_user(Domain, Username)),
    {next_state, wait_for_legacy_auth_method, State#state{from_pid=From}};
stream_opened({login, sasl, "PLAIN"}, From, State=#state{connection = Module,
					  connection_ref = ConnRef,
					  auth_method=Auth}) ->
    Domain = get_domain(Auth),
    Username = get_username(Auth),
    Password = get_password(Auth),
    InitialResp = iolist_to_binary([Domain, 0, Username, 0, Password]),
    Module:send(ConnRef,
 		exmpp_client_sasl:selected_mechanism("PLAIN", InitialResp)),
    {next_state, wait_for_sasl_response, State#state{from_pid=From}};
stream_opened({login, sasl, "ANONYMOUS"}, From, State=#state{connection = Module,
					  connection_ref = ConnRef
					  }) ->
    Module:send(ConnRef, exmpp_client_sasl:selected_mechanism("ANONYMOUS")),
    {next_state, wait_for_sasl_response, State#state{from_pid=From}};

stream_opened({register_account, Password}, From,
	      State=#state{connection = Module,
			   connection_ref = ConnRef,
			   auth_method=Auth}) ->
    Username = get_username(Auth),
    register_account(ConnRef, Module, Username, Password),
    {next_state, wait_for_register_result, State#state{from_pid=From}};
stream_opened({register_account, Username, Password}, From,
	      State=#state{connection = Module,
			   connection_ref = ConnRef}) ->
    register_account(ConnRef, Module, Username, Password),
    {next_state, wait_for_register_result, State#state{from_pid=From}};

%% We can define update login informations after we are connected to
%% the XMPP server:
%% Define JID and authentication method
stream_opened({set_auth, Auth}, _From, State) when is_tuple(Auth) ->
    {reply, ok, stream_opened, State#state{auth_method=Auth}};
stream_opened({presence, _Status, _Show}, _From, State) ->
    {reply, {error, not_logged_in}, setup, State};
%% We allow to send packet here to give control to the developer on all packet
%% send to the server. The developer can implements his own login management
%% code.
%% If the packet is an iq set or get:
%% We check that there is a valid id and return it to match the reply
stream_opened({send_packet, Packet}, _From,
	      State = #state{connection = Module,
			     connection_ref = ConnRef}) ->
    Id = send_packet(Packet, Module, ConnRef),
    {reply, Id, stream_opened, State}.

%% Process incoming
%% Dispatch incoming messages
stream_opened(?message, State = #state{connection = _Module,
				       connection_ref = _ConnRef}) ->
    process_message(State#state.client_pid, Attrs, MessageElement),
    {next_state, stream_opened, State};
%% Dispach IQs from server
stream_opened(?iq, State) ->
    process_iq(State#state.client_pid, Attrs, IQElement),
    {next_state, stream_opened, State};
%% Handle stream error: We keep the process alive to be able
%%                      return errors
stream_opened(?streamerror, State) ->
    {next_state, stream_error, State#state{stream_error=Reason}};
%% Handle end of stream
stream_opened(?streamend, State) ->
    {next_state, stream_closed, State};

%% any other element (features and starttls for 1.0 streams)
stream_opened(#xmlstreamelement{element=Packet}, State) ->
    State#state.client_pid ! #received_packet{raw_packet = Packet},
    {next_state, stream_opened, State}.

%% TODO: handle errors
wait_for_sasl_response(#xmlstreamelement{element=#xmlel{name='success'}}, State) ->
    #state{connection_ref = ConnRef, receiver_ref = ReceiverRef, connection = Module, auth_method = Auth} = State,
    Domain = get_domain(Auth),
    Module:reset_parser(ReceiverRef),
    ok = Module:send(ConnRef, exmpp_stream:opening(Domain, ?NS_JABBER_CLIENT, {1,0})),
    {next_state, wait_for_stream, State#state{authenticated = true}}.

stream_error(_Signal, _From, State) ->
    {reply, {stream_error, State#state.stream_error}, stream_error, State}.
stream_error(?streamend, State) ->
    {next_state, stream_closed, State};
stream_error(_Signal, State) ->
    {next_state, stream_error, State}.

stream_closed(_Signal, _From, State = #state{stream_error = undefined}) ->
    {reply, {stream_closed, undefined}, stream_closed, State};
stream_closed(_Signal, _From, State) ->
    {reply, {stream_error, State#state.stream_error}, stream_closed, State}.
stream_closed(_Signal, State) ->
    {next_state, stream_closed, State}.

%% Reason comes from streamerror macro
wait_for_legacy_auth_method(?iq_no_attrs, State = #state{connection = Module,
							 connection_ref = ConnRef,
							 auth_method = Auth,
							 stream_id = StreamId}) ->
    Username = get_username(Auth),
    Password = get_password(Auth),
    Resource = get_resource(Auth),
    Method = get_method(Auth),
    case check_auth_method(Method, IQElement) of
	ok ->
	    case do_auth(Method, ConnRef, Module, Username, Password, Resource,
                         StreamId) of
		ok ->
		    {next_state, wait_for_auth_result, State};
		Error ->
		    {stop, Error, State}
	    end;
	{error, Reason} ->
	    {stop, {error, Reason}, State}
    end;
wait_for_legacy_auth_method(?streamerror, State) ->
    {stop, {error, Reason}, State}.

%% TODO: We should be able to match on iq type directly on the first
%% level record
wait_for_auth_result(?iq_no_attrs, State = #state{from_pid=From, auth_method = Auth}) ->
    case exmpp_xml:get_attribute_as_binary(IQElement, type, undefined) of
 	<<"result">> ->
            gen_fsm:reply(From, {ok, get_jid(Auth)}),
            {next_state, logged_in, State#state{from_pid=undefined}};
	<<"error">> ->
            Reason = exmpp_stanza:get_condition(IQElement),
            gen_fsm:reply(From, {auth_error, Reason}),
            {next_state, stream_opened, State#state{from_pid=undefined}}
    end.

%% Note: We do not get the field received from server to perform register
%% TODO: The API should be flexible to adapt to server
%% requirements. Check that a client can get the list of fields and
%% override this simple method of registration.
wait_for_register_result(?iq_no_attrs, State = #state{from_pid=From}) ->
    case exmpp_xml:get_attribute_as_binary(IQElement, type, undefined) of
 	<<"result">> ->
            gen_fsm:reply(From, ok),
            {next_state, stream_opened, State#state{from_pid=undefined}};
	<<"error">> ->
            Reason = exmpp_stanza:get_condition(IQElement),
            gen_fsm:reply(From, {register_error, Reason}),
            {next_state, stream_opened, State#state{from_pid=undefined}}
    end;
wait_for_register_result(?streamerror, State) ->
    {stop, {error, Reason}, State}.

%% ---
%% Send packets
%% If the packet is an iq set or get:
%% We check that there is a valid id and return it to match the reply
logged_in({send_packet, Packet}, _From,
	  State = #state{connection = Module,
			 connection_ref = ConnRef}) ->
    Id = send_packet(Packet, Module, ConnRef),
    {reply, Id, logged_in, State}.

%% ---
%% Receive packets
%% When logged in we dispatch the event we receive
%% Dispatch incoming presence packets
logged_in(?presence,
	  State = #state{connection = _Module,
			 connection_ref = _ConnRef}) ->
    process_presence(State#state.client_pid, Attrs, PresenceElement),
    {next_state, logged_in, State};
%% Dispatch incoming messages
logged_in(?message, State = #state{connection = _Module,
				   connection_ref = _ConnRef}) ->
    process_message(State#state.client_pid, Attrs, MessageElement),
    {next_state, logged_in, State};
%% Dispach IQs from server
logged_in(?iq, State) ->
    process_iq(State#state.client_pid, Attrs, IQElement),
    {next_state, logged_in, State};
logged_in(?streamerror, State) ->
    process_stream_error(State#state.client_pid, Reason),
    {next_state, stream_error, State#state{stream_error=Reason}};
%% Process unexpected packet
logged_in(_Packet, State) ->
    %% log it or do something better
    %%io:format("!!!ALERT!!! Unknown packet:~p~p~n", [_Packet, State]),
    {next_state, logged_in, State}.

%% TODO:
%% Handle disconnections
%% Connection replaced.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Connect to server
connect(Module, Params, From, State) ->
    Domain = get_domain(State#state.auth_method),
    connect(Module, Params, Domain, From, State).
connect(Module, Params, Domain, From, #state{client_pid=_ClientPid, stream_version = Version} = State) ->
    try start_parser() of
	StreamRef ->
	    try Module:connect(self(), StreamRef, Params) of
		{ConnRef, ReceiverRef} ->
		    ok = Module:send(ConnRef,
				     exmpp_stream:opening(Domain,
							  ?NS_JABBER_CLIENT,
							  Version)),
		    %% TODO: Add timeout on wait_for_stream to return
		    %% meaningfull error.
		    {next_state, wait_for_stream,
		     State#state{domain = Domain,
				 connection = Module,
				 connection_ref = ConnRef,
				 stream_ref = StreamRef,
				 receiver_ref = ReceiverRef,
				 from_pid = From}}
	    catch
		Error ->
		    exmpp_xmlstream:stop(StreamRef),
		    %% We do not stop here, because the developer
		    %% might want to start a connection using another
		    %% transport
		    {reply, Error, setup,
		     State#state{stream_ref = undefined,
				 from_pid = From}}
	    end
    catch
	Error ->
	    {reply, Error, setup, State}
    end.

%% Authentication
%% digest auth will fail if we do not have streamid
do_auth(password, ConnRef, Module, Username, Password, Resource, _StreamId) ->
    Module:send(ConnRef,
		exmpp_client_legacy_auth:password_plain(Username, Password,
							Resource));
do_auth(digest, ConnRef, Module, Username, Password, Resource, StreamId)
  when is_list(StreamId) ->
    Module:send(ConnRef,
		exmpp_client_legacy_auth:password_digest(Username,
							 Password,
							 Resource,
							 StreamId));
%% In this case StreamId can be false
do_auth(digest, _ConnRef, _Module, _Username, _Password, _Resource, StreamId)
  when is_atom(StreamId) ->
    {auth_error, no_streamid_for_digest_auth}.

%% Extraction functions

%% Extract domain from Auth Method
get_domain({basic, _Method, JID, _Password}) when ?IS_JID(JID) ->
    exmpp_jid:domain_as_list(JID).
get_username({basic, _Method, JID, _Password}) when ?IS_JID(JID) ->
    exmpp_jid:node_as_list(JID).
get_resource({basic, _Method, JID, _Password}) when ?IS_JID(JID) ->
    exmpp_jid:resource_as_list(JID).
get_password({basic, _Method, _JID, Password}) when is_list(Password) ->
    Password.
get_method({basic, Method, _JID, _Password}) when is_atom(Method) ->
    Method.
get_jid({_, _Method, JID, _Password}) when ?IS_JID(JID) ->
    JID.

%% Parsing functions

%% Define parser options
%% No compatibility mode: We use all the nice optimisation of exmpp:
-define(PARSER_OPTIONS,
	[
	 {names_as_atom, true},
	 {check_nss, xmpp},
	 {check_elems, xmpp},
	 {check_attrs, xmpp},
	 {emit_endtag, false},
	 {root_depth, 0},
	 {max_size, infinity}]).

%% Start parser and return stream reference
start_parser() ->
    exmpp_xmlstream:start({gen_fsm, self()},
                          exmpp_xml:start_parser(?PARSER_OPTIONS),
                          [{xmlstreamstart,new}]).

%% Authentication functions
check_auth_method(Method, IQElement) ->
    %% Check auth method if we have the IQ result
    case exmpp_xml:get_attribute_as_binary(IQElement, type, undefined) of
	<<"result">> ->
	    check_auth_method2(Method, IQElement);
	_ ->
	    {error, not_auth_method_result}
    end.
check_auth_method2(Method, IQElement) ->
    QueryElement = exmpp_xml:get_element(IQElement,
					 'jabber:iq:auth',
					 'query'),
    case exmpp_xml:get_element(QueryElement,
			       'jabber:iq:auth',
			       Method) of
	undefined ->
	    {error, no_supported_auth_method};
	_ ->
	    ok
    end.

%% Packet processing functions
process_presence(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, type, "available"),
    Who = case get_attribute_value(Attrs, from, undefined) of
                undefined -> undefined;
                "" -> undefined;
                Value -> exmpp_jid:to_lower(Value)
          end,
    Id = get_attribute_value(Attrs, id, ""),
    ClientPid ! #received_packet{packet_type = presence,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 raw_packet = Packet}.

process_message(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, type, "normal"),
    Who = case get_attribute_value(Attrs, from, undefined) of
                undefined -> undefined;
                "" -> undefined;
                Value -> exmpp_jid:to_lower(Value)
          end,
    Id = get_attribute_value(Attrs, id, ""),
    ClientPid ! #received_packet{packet_type = message,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 raw_packet = Packet}.

process_iq(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, type, ""),
    Who = case get_attribute_value(Attrs, from, undefined) of
                undefined -> undefined;
                "" -> undefined;
                Value -> exmpp_jid:to_lower(Value)
          end,
    Id = get_attribute_value(Attrs, id, ""),
    NS = exmpp_iq:get_payload_ns_as_atom(Packet),
    ClientPid ! #received_packet{packet_type = iq,
                                 queryns = NS,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 raw_packet = Packet}.

process_stream_error(ClientPid, Reason) ->
    ClientPid ! {stream_error, Reason}.

%% Add a packet ID is needed:
%% Check that the attribute list has defined an ID.
%% If no ID has been defined, add a packet id to the list of attributes
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.
check_id(Attrs) ->
    case exmpp_xml:get_attribute_from_list_as_binary(Attrs, id, <<>>) of
	<<>> ->
	    Id = exmpp_utils:random_id("session"),
	    {exmpp_xml:set_attribute_in_list(Attrs, id, Id), Id};
        Id -> {Attrs, Id}
    end.

%% Try getting a given atribute from a list of xmlattr records
%% Return default value if attribute is not found
get_attribute_value(Attrs, Attr, Default) ->
    exmpp_xml:get_attribute_from_list_as_list(Attrs, Attr, Default).

%% Internal operations
%% send_packet: actually format and send the packet:
send_packet(?iqattrs, Module, ConnRef) ->
    Type = exmpp_xml:get_attribute_from_list_as_binary(Attrs, type, undefined),
    case Type of
	<<"error">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId;
	<<"result">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId;
	<<"set">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId;
	<<"get">> ->
	    {Attrs2, PacketId} = check_id(Attrs),
	    Module:send(ConnRef, IQElement#xmlel{attrs=Attrs2}),
	    PacketId
    end;
send_packet(?elementattrs, Module, ConnRef) ->
    {Attrs2, Id} = check_id(Attrs),
    Module:send(ConnRef, Element#xmlel{attrs=Attrs2}),
    Id.

register_account(ConnRef, Module, Username, Password) ->
    Module:send(ConnRef,
		exmpp_client_register:register_account([{username, Username},
							{password, Password}])).


