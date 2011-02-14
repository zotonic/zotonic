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
%% The module <strong>{@module}</strong> implements a simple BOSH XMPP client.
%%
%% <p>
%% This is a example use of the exmpp framework.
%% </p>
%%
%% <p>
%% Usage:
%% </p>
%% <pre>{ok, session} = bosh_client:start().
%% bosh_client:stop(Session).</pre>

-module(bosh_client).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, stop/1]).
-export([init/0]).

start() ->
    spawn(?MODULE, init, []).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

init() ->
    ok = application:start(exmpp),
    ok = application:start(lhttpc),
    MySession = exmpp_session:start({1,0}),
    MyJID = exmpp_jid:make("bosh", "localhost", random),
    exmpp_session:auth_basic_digest(MySession, MyJID, "password"),
    %% Connect in standard TCP:
    {ok, _StreamId, _Features} = exmpp_session:connect_BOSH(MySession,
					   "http://127.0.0.1:5280/http-bind",
					   "localhost", []),
    session(MySession, MyJID).

%% We are connected. We now log in (and try registering if authentication fails)
session(MySession, _MyJID) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession, "PLAIN")
    catch
	throw:{auth_error, 'not-authorized'} ->
 	    %% Try creating a new user:
 	    io:format("Register~n",[]),
 	    %% In a real life client, we should trap error case here
	    %% and print the correct message.
	    exmpp_session:register_account(MySession, "password"),
	    %% After registration, retry to login:
	    exmpp_session:login(MySession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
 			      exmpp_presence:set_status(
 				exmpp_presence:available(), "Echo Ready")),
    loop(MySession).

%% Process exmpp packet:
loop(MySession) ->
    receive
        stop ->
            exmpp_session:stop(MySession);
        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message, raw_packet=_Packet} ->
            io:format("message: ~p~n", [Record]),
            loop(MySession);
        Record ->
            io:format("~p~n", [Record]),
            loop(MySession)
    end.
