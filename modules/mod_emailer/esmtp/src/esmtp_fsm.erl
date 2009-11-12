%%
%% file: smtp_fsm.erl
%% author: Michael Bradford <michael.bradford@t-mobile.uk.net>
%% description: simple smtp client using gen_fsm behaviour
%%
%% this client has been 
%% fully tested against exim 3.36
%% partially tested against sendmail (apart from auth)
%%
%% 0.2 adds md5 authentication
%% 0.3 included revisions to style by Francesco 
%% 0.4 fixes md5 authentication
%% 1.0 is first release, including 
%% 	extra fixes for MD5
%%	better error messages for sendemail/4
%% 1.1 fixes mail from & rcpt to bugs where they weren't 
%% 	strictly standards compliant
%% esmtp Packaging for use in a small smtp client library.
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Usage Example
%%
%% 6> c(smtp_fsm).
%% ./smtp_fsm.erl:15: Warning: undefined call-back function 
%%   code_change/4
%% ./smtp_fsm.erl:15: Warning: undefined call-back function
%%   handle_info/3
%% {ok,smtp_fsm}
%% 7> {ok,Pid} = smtp_fsm:start("172.24.75.203").
%% connecting to "172.24.75.203"
%% socket open
%% {ok,<0.45.0>}
%% 8> smtp_fsm:ehlo(Pid).
%% {ok,"250-smtp.test.co.uk Hello test.test.co.uk [172.24.72.71]
%%   \r\n250-SIZE\r\n250-PIPELINING\r\n250-AUTH PLAIN LOGIN 
%%   CRAM-MD5\r\n250-HELP\r\n"}
%% 9> smtp_fsm:features(Pid). 
%% {ok,["SIZE","PIPELINING","AUTH PLAIN LOGIN CRAM-MD5","HELP"]}
%% 10> smtp_fsm:login(Pid,"mike","secret").
%% {ok,"235 Authentication succeeded\r\n"}
%% 11> smtp_fsm:sendemail(Pid,"michael.bradford@t-mobile.uk.net",
%%   "test@test.co.uk", Msg).
%% ok
%% 12> smtp_fsm:close(Pid).
%% ok
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
-module(esmtp_fsm).
-behaviour(gen_fsm).
-vsn('v1.1-esmtp').

-define(RESPONSE_TIMEOUT, timer:minutes(1)).

%% Client Functions
-export([start/0, start/1, start/3, start_link/3,
         ehlo/1, ehlo/2, helo/1,
	 helo/2,close/1,noop/1,rset/1, mail_from/2, 
	 rcpt_to/2,message/2,sendemail/4,features/1,
	 login/3,plain_login/3,login_login/3,md5_login/3]).

%% states
-export([smtp_start/3, smtp_conn/3, smtp_data/3, 
	 smtp_login/3, smtp_md5/3]).

%% call back behaviours
-export([handle_event/3, handle_sync_event/4, init/1,
        code_change/4, handle_info/3,
	 terminate/3]).

%% testing
-export([]).

-record(info, {socket, features}).
-define(AUTHLIST, [ "CRAM-MD5", "PLAIN", "LOGIN"]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% use start/ to connect to the SMTP server & port
%% default port (if undefined) is 25
%% default host (if undefined) is localhost
start()          -> start("localhost", 25, false).
start(Host)      -> start(Host, 25, false).
start(Host,Port,SSL) -> gen_fsm:start(?MODULE,[Host,Port,SSL],[]).

start_link(Host, Port, SSL) ->
    gen_fsm:start_link(?MODULE, [Host, Port, SSL], []).

%% use helo/ when you want to use SMTP
%% if the client name is undefined then the FQDN is used
helo(Fsm)      -> helo(Fsm, getFQDN()).
helo(Fsm,Name) -> gen_fsm:sync_send_event(Fsm, {helo,Name}).

%% use ehlo/ when you want to use ESMTP
%% if the client name is undefined then the FQDN is used
ehlo(Fsm)      -> ehlo(Fsm, getFQDN()).
ehlo(Fsm,Name) -> gen_fsm:sync_send_event(Fsm,{ehlo,Name}).

%% use sendemail/4 if you can't be bothered sending the 
%% individual SMTP commands
sendemail(Fsm,From,To,Message) ->
    case mail_from(Fsm,From) of
	{ok, _Resp}         -> sendemail(Fsm,To,Message);
	{mfrom_error, _Resp} = Resp -> 
	    %io:format("mfrom_error ~p~n",[Resp]),
	    rset(Fsm),
	    Resp;
	Resp                -> Resp
    end.
%% sendemail/3 only used by sendemail/4
sendemail(Fsm,To,Message) ->
	case rcpt_to(Fsm,To) of
    {ok, _Resp}        -> sendemail(Fsm,Message);
    {rcpt_error, _Resp} = Resp -> 
		%io:format("rcpt_error ~p~n",[Resp]),
		rset(Fsm),
		Resp;
    Resp               -> Resp
	end.
%% sendemail/2 only used by sendemail/3
sendemail(Fsm,Message) ->
	case message(Fsm,Message) of
    {ok, _Resp}        -> ok;
    {data_error, _Resp} = Resp -> 
		%io:format("data_error ~p~n",[Resp]),
	    rset(Fsm),
	    Resp;
	Resp               -> Resp
	end.

%% individual functions used for sending emails
%%
%% use mail_from/2 to send SMTP mail from:
mail_from(Fsm,Address) -> 
    gen_fsm:sync_send_event(Fsm,{mfrom,Address}).
%% use rcpt_to/2 to send SMTP rcpt to:
rcpt_to(Fsm,Address) -> 
    gen_fsm:sync_send_event(Fsm,{rcpt_to,Address}).
%% use message/2 to send email content
message(Fsm,Message) -> 
    case gen_fsm:sync_send_event(Fsm,data) of
	{ok, _Resp} -> msg(Fsm,Message);
	Resp -> Resp
    end.
%% msg/2 only used by message/2
msg(Fsm,Message) ->
    gen_fsm:sync_send_event(Fsm,{msg,Message}).

%% login methods
%%
%% use login/3 if you want the smtp client to choose the 
%% AUTH method
login(Fsm,User,Pwd) ->
    {ok, Features} = features(Fsm),
    case does_auth(Features) of
	no       -> {auth_error, "AUTH not supported"};
	Supports -> choose_login(Fsm,User,Pwd,Supports)
    end.
%% choose_login/4 only used by login/3
choose_login(Fsm,User,Pwd,Supports) ->
    case pref_auth(Supports) of
	{ok, "CRAM-MD5"}-> md5_login(Fsm,User,Pwd);
	{ok, "PLAIN"}  	-> plain_login(Fsm,User,Pwd);
	{ok, "LOGIN"}  	-> login_login(Fsm,User,Pwd);
	{error, Error} 	-> {error, Error}
    end.

%% interfaces for plain, login & md5 authentication
%% use plain_login/3 for SMTP AUTH PLAIN
plain_login(Fsm,User,Pwd) -> 
    gen_fsm:sync_send_event(Fsm,{plain_login,User,Pwd}).

%% use login_login/3 for SMTP AUTH LOGIN
login_login(Fsm,User,Pwd) ->
    %% send the user name
    case gen_fsm:sync_send_event(Fsm,{login_login,User}) of
	{ok, _Resp} -> login_login(Fsm,Pwd);
	Resp        -> Resp
    end.
%% login_login/2 only used by login_login/3
login_login(Fsm,Pwd) ->
    %% then send the password
    case gen_fsm:sync_send_event(Fsm,{login_pass,Pwd}) of
	{ok, Resp} -> {ok, Resp};
	Resp       -> Resp
    end.	

%% use md5_login/3 for SMTP AUTH MD5
md5_login(Fsm,User,Pwd) ->
    case gen_fsm:sync_send_event(Fsm,md5_login) of
	{ok, Resp} -> 
	    md5_login(Fsm,decode_challenge(Resp),User,Pwd);
	Resp -> 
	    Resp
    end.
%% md5_login/4 only used by md5_login/3
md5_login(Fsm,Challenge,User,Pwd) ->
    Md5Code = md5_hmac(Challenge,Pwd),
    gen_fsm:sync_send_event(Fsm, {md5_send,User,Md5Code}).
	
%% use these for noop & rset as defined in ESMTP rfc
noop(Fsm) -> gen_fsm:sync_send_event(Fsm, noop).
rset(Fsm) -> gen_fsm:sync_send_all_state_event(Fsm, rset).

%% use this to close the SMTP connection
close(Fsm) -> gen_fsm:send_all_state_event(Fsm, close).

%% use this to find out what ESMTP features are supported
features(Fsm) -> gen_fsm:sync_send_event(Fsm, features).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Server,Port,_SSL=true]) ->
    %io:format("connecting to ~p~n",[Server]),
    {ok,S} = ssl:connect(Server, Port, [list,{packet,0}], ?RESPONSE_TIMEOUT),
    Socket = {ssl, S},
    init({sock, Socket});
init([Server,Port,_SSL=false]) ->
    %io:format("connecting to ~p~n",[Server]),
    {ok,S} = gen_tcp:connect(Server, Port, [list,{packet,0}], ?RESPONSE_TIMEOUT),
    Socket = {gen_tcp, S},
    init({sock, Socket});

init({sock, Socket}) ->
    %io:format("socket open~n"),
    case get_response(Socket) of
	{"220", _Resp} -> 
	    {ok, smtp_start, #info{socket=Socket}};
	_Error -> 
	    {stop, conn_error}
    end.

%% state=smtp_start after connection but before we've sent
%% helo or ehlo
smtp_start({helo, Name}, _Pid, Info)->
    Msg = ["helo ", Name, "\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"250", Resp} -> 
	    {reply,{ok,Resp},smtp_conn,Info};
	{_Code, Resp} -> 
	    {reply,{helo_error,Resp},smtp_start, Info};
	_Error -> 
	    {stop, conn_error, conn_error, []}
    end;
smtp_start({ehlo, Name}, _Pid, Info) ->
    Msg = ["ehlo ", Name, "\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"250", Resp} -> 
	    Tokens = string:tokens(Resp, "\r\n"),
	    Strs = [string:sub_string(X,5) || X <- Tokens],
	    NewInfo = Info#info{features = tl(Strs)},
	    {reply, {ok, Resp}, smtp_conn, NewInfo};
	{_Code, Resp} -> 
	    {reply, {ehlo_error, Resp}, smtp_start, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end.

%% state=smtp_conn - we can now send emails, login etc.
%%
smtp_conn(noop, _Pid, Info) ->
    ok = socket_send(Info#info.socket, "noop\r\n"),
    case get_response(Info#info.socket) of
	{"250", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {noop_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end;
smtp_conn({mfrom, Address}, _Pid, Info) ->
    Msg = ["mail from:", Address, "\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"250", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {mfrom_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end;
smtp_conn({rcpt_to, Address}, _Pid, Info) ->
    Msg = ["rcpt to:", Address, "\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"250", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {rcpt_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end;
smtp_conn(data, _Pid, Info) ->
    ok = socket_send(Info#info.socket, "Data\r\n"),
    case get_response(Info#info.socket) of
	{"354", Resp} -> 
	    {reply, {ok, Resp}, smtp_data, Info};
	{_Code, Resp} -> 
	    {reply, {data_error, Resp}, smtp_conn, Info};
	Resp -> 
	    {stop, conn_error, {conn_error, Resp}, []}
    end;
smtp_conn({plain_login,User,Pwd}, _Pid, Info) ->
    Msg =  ["AUTH PLAIN ", plain_encode(User,Pwd), "\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"235", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {auth_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end;
smtp_conn({login_login,User}, _Pid, Info) ->
    B64Usr =  base64:encode(User),
    Msg = ["AUTH LOGIN ", B64Usr, "\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"334", Resp} -> 
	    {reply, {ok, Resp}, smtp_login, Info};
	{_Code, Resp} -> 
	    {reply, {auth_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end;
smtp_conn(features, _Pid, Info) ->
    {reply, {ok,Info#info.features}, smtp_conn, Info};
smtp_conn(md5_login, _Pid, Info) ->
    ok = socket_send(Info#info.socket,"AUTH CRAM-MD5\r\n"),
    case get_response(Info#info.socket) of
	{"334", Resp} -> 
	    {reply, {ok, Resp}, smtp_md5, Info};
	{_Code, Resp} -> 
	    {reply, {auth_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end;
smtp_conn(Unknown, _From, Info) ->
    {stop,
     {unknown_cmd, Unknown},
     {error, {unknown_cmd, Unknown}},
     Info}.

%% state used in md5 authentication
smtp_md5({md5_send, User, Md5_hmac}, _Pid, Info) ->
    Md5Usr = base64:encode(User++" "++Md5_hmac),
    ok = socket_send(Info#info.socket, [Md5Usr,"\r\n"]),
    case get_response(Info#info.socket) of
	{"235", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {auth_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end.

%% state used in sending data, it's after data has been sent,
%% but before the message has been sent
smtp_data({msg,Message}, _Pid, Info) ->
    Msg = [Message, "\r\n.\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"250", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {data_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end.

%% state used in login, it's after the username has been sent, 
%% but before the password has been sent.
smtp_login({login_pass,Pwd}, _Pid, Info) ->
    Msg = [base64:encode(Pwd), "\r\n"],
    ok = socket_send(Info#info.socket, Msg),
    case get_response(Info#info.socket) of
	{"235", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {auth_error, Resp}, smtp_conn, Info};
	Resp -> 
	    {stop, conn_error, {conn_error, Resp}, []}
    end.

%% non-specific callbacks

handle_sync_event(rset, _Pid, _State, Info) ->
    ok = socket_send(Info#info.socket, "rset\r\n"),
    case get_response(Info#info.socket) of
	{"250", Resp} -> 
	    {reply, {ok, Resp}, smtp_conn, Info};
	{_Code, Resp} -> 
	    {reply, {rset_error, Resp}, smtp_conn, Info};
	Error -> 
	    {stop, conn_error, {conn_error, Error}, []}
    end.

handle_event(close, _State, Info) ->
    ok = socket_send(Info#info.socket, "quit\r\n"),
    {stop, normal, Info}.

terminate(normal, _StateName, _StateData)->
    normal;
terminate(Reason,_StateName,_StateData) -> 
    {terminated, Reason}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% works out fully qualified domain name for host
getFQDN()->
    {ok, Hostname} = inet:gethostname(),
    {ok, HostEnt} = inet:gethostbyname(Hostname),
    {hostent,Fqdn,_,inet,_,_IP_addrs} = HostEnt,
    Fqdn.

%% encodes user & password as required by PLAIN AUTH method
plain_encode(User, Pwd) ->
    Plain_str = lists:concat([User,"\0",User,"\0",Pwd]),
    base64:encode(Plain_str).

%% extracts & decodes server challenge from server response
%% used in MD5 AUTH
decode_challenge(Resp) ->
    Str = string:substr(Resp,5,string:len(Resp)-6),
    base64:decode(string:strip(Str)).

%% returns a hex digest of a binary value
hexdigest(Binary) ->
    List = binary_to_list(Binary),
    Hexlist = lists:map(fun hexit/1,List),
    string:to_lower(lists:concat(Hexlist)).

%% returns hex value of integer (always) as 2 chr string
hexit(Int) ->
    Hex = httpd_util:integer_to_hexlist(Int),
    case string:len(Hex) of
	1 -> "0" ++ Hex;
	_ -> Hex
    end.
    
%% generates MD5 mac and returns as hex string
md5_hmac(Challenge,Pwd) ->
    Md5_bin = crypto:md5_mac(Pwd,Challenge),
    hexdigest(Md5_bin).

%% selects the preferred AUTH method supported
pref_auth(Supports)->
    pref_auth(?AUTHLIST, Supports).	
pref_auth([H|T], Supports) ->
    case lists:member(H, Supports) of
	true  -> {ok, H};
	false -> pref_auth(T,Supports)
    end;
pref_auth([], _Supports) ->
    {error, none}.

%% checks Features list if AUTH is supported and returns 
%% a list of methods
does_auth(["AUTH"++Supp|_]) -> string:tokens(Supp, " ");
does_auth([_Other|Tail])     -> does_auth(Tail);
does_auth([])                -> no.

socket_send({gen_tcp, S}, Msg) ->
    gen_tcp:send(S, Msg);
socket_send({ssl, S}, Msg) ->
    ssl:send(S, Msg).

%% listens for smtp response & parses to get code
get_response(Socket) ->
    get_response(Socket, ?RESPONSE_TIMEOUT).

get_response({gen_tcp, Socket}, Timeout) ->
    receive
	{tcp, Socket, Resp} ->
	    {lists:sublist(Resp, 3), Resp}; 
	{tcp_error, Socket, Reason} ->
	    {conn_error, Reason};
	{tcp_closed,Socket} ->
	    conn_closed
    after Timeout ->
            {conn_error, timeout}
    end;
get_response({ssl, Socket}, Timeout) ->
    receive
	{ssl, Socket, Resp} ->
	    {lists:sublist(Resp, 3), Resp}; 
	{ssl_error, Socket, Reason} ->
	    {conn_error, Reason};
	{ssl_closed,Socket} ->
	    conn_closed
    after Timeout ->
            {conn_error, timeout}
    end.
