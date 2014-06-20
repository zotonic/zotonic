%% @doc SMTP server for handling bounced messages.
%%      Code based on the example callback module supplied 
%%      with gen_smtp. 
%%      Original author: Andrew Thompson (andrew@hijacked.us)
%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010-2013 Maximonster Interactive Things

%% Copyright 2010-2013 Maximonster Interactive Things
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(z_email_receive_server).
-behaviour(gen_smtp_server_session).

-compile([{parse_transform, lager_transform}]).

-include_lib("zotonic.hrl").

-export([start_link/0]).

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
         handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
         handle_other/3, code_change/3, terminate/2]).

-record(state, {
    options = [] :: list(),
    peer :: tuple(),
    banner :: string(),
    hostname :: binary(),
    helo :: binary(),
    rcpt :: binary(),
    from :: binary()
}).
        

start_link() ->
    %% Collect the configuration args of the bounce server 
    Args1 = case get_listen_domain() of
        undefined -> [];
        ListenDomain -> [{domain, ListenDomain}]
    end,    
    Args2 = case get_listen_ip() of
        undefined -> [];
        any -> [{address, {0,0,0,0}} | Args1];
        ListenIp -> 
            {ok, Address} = inet:getaddr(ListenIp, inet),
            [{address, Address} | Args1]
    end,    
    Args3 = case get_listen_port() of
        undefined -> Args2;
        ListenPort -> [{port, ListenPort} | Args2]
    end,
    start_link([Args3]).

start_link(Args) when is_list(Args) ->
    gen_smtp_server:start_link({local, ?MODULE}, ?MODULE, Args).

-spec init(Hostname :: binary(), SessionCount :: non_neg_integer(), PeerName :: tuple(), Options :: list()) -> {'ok', string(), #state{}} | {'stop', any(), string()}.
init(Hostname, SessionCount, PeerName, Options) ->
    case SessionCount > 20 of
        false ->
            Banner = io_lib:format("~s ESMTP Zotonic ~s", [Hostname, ?ZOTONIC_VERSION]),
            State = #state{options = Options, peer=PeerName, hostname=Hostname},
            {ok, Banner, State};
        true ->
            lager:warning("SMTP Connection limit exceeded (~p)", [SessionCount]),
            {stop, normal, io_lib:format("421 ~s is too busy to accept mail right now", [Hostname])}
    end.

-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', pos_integer(), #state{}} | {'ok', #state{}}.
handle_HELO(Hostname, State) ->
    {ok, 655360, State#state{helo=Hostname}}. % 640kb of HELO should be enough for anyone.
    % If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', list(), #state{}}.
handle_EHLO(Hostname, Extensions, State) ->
    MyExtensions = case proplists:get_value(auth, State#state.options, false) of
                       true ->
                           % auth is enabled, so advertise it
                           Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
                       false ->
                           Extensions
                   end,
    {ok, MyExtensions, State#state{helo=Hostname}}.

-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_MAIL(From, State) ->
    {ok, State#state{from=From}}.

-spec handle_MAIL_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_MAIL_extension(_Extension, State) ->
    {ok, State}.

-spec handle_RCPT(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_RCPT(_To, State) ->
    % Check if the "To" address exists
    % Check domain, check addressee in domain.
    % For bounces:
    % - To = <noreply+MSGID@example.org> 
    % - Return-Path header should be present and contains <>
    {ok, State}.

-spec handle_RCPT_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_RCPT_extension(_Extension, State) ->
    {ok, State}.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_DATA(From, To, Data, State) ->
    MsgId = z_convert:to_binary(z_ids:id(32)),
    DataRcvd = add_received_header(Data, MsgId, State),
    decode_and_receive(MsgId, From, To, DataRcvd, State).

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
    % reset any relevant internal state
    reset_state(State).

-spec handle_VRFY(Address :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
    {lists:flatten(io_lib:format("500 Error: command not recognized : '~s'", [Verb])), State}.


-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
    {ok, Reason, State}.


%%% ------------------------------------------------------------------------------------------
%%% Internal functions
%%% ------------------------------------------------------------------------------------------

reset_state(State) ->
    State#state{helo=undefined, rcpt=undefined, from=undefined}.

decode_and_receive(MsgId, From, To, DataRcvd, State) ->
    case decode(DataRcvd) of
        {ok, {Type, Subtype, Headers, _Params, Body} = Decoded} ->
            case find_bounce_id({Type, Subtype}, To, Headers) of
                {ok, MessageId} ->
                    % The e-mail server knows about the messages sent from our system.
                    % Only report fatal bounces, silently ignore delivery warnings
                    case z_email_receive_check:is_nonfatal_bounce({Type, Subtype}, Headers, Body) of
                        true -> nop;
                        false -> z_email_server:bounced(State#state.peer, MessageId)
                    end,
                    {ok, MsgId, reset_state(State)};
                ok ->
                    % Bounced, but without a message id (accept & silently drop the message)
                    {ok, MsgId, reset_state(State)};
                no_bounce ->
                    receive_data(z_email_spam:spam_check(DataRcvd),
                                 Decoded, MsgId, From, To, DataRcvd, State)
            end;
        {error, Reason} ->
            lager:error("SMTP receive: Message decode FAILED with ~p", [Reason]),
            {error, "550 Your email cannot be parsed", State}
    end.

receive_data({ok, {ham, SpamStatus, _SpamHeaders}}, {Type, Subtype, Headers, Params, Body}, MsgId, From, To, DataRcvd, State) ->
    lager:debug("Handling email from ~s to ~p (id ~s) (peer ~s) [~p]", 
                [From, To, MsgId, inet_parse:ntoa(State#state.peer), SpamStatus]),
    Received = z_email_receive:received(To, From, State#state.peer, MsgId, 
                                        {Type, Subtype}, Headers, Params, Body, DataRcvd),
    reply_handled_status(Received, MsgId, reset_state(State));
receive_data({ok, {spam, SpamStatus, _SpamHeaders}}, _Decoded, MsgId, From, To, _DataRcvd, State) ->
    lager:info("Refusing spam from ~s to ~p (id ~s) (peer ~s) [~p]", 
               [From, To, MsgId, inet_parse:ntoa(State#state.peer), SpamStatus]),
    {error, z_email_spam:smtp_status(SpamStatus, From, To, State#state.peer), reset_state(State)}.


reply_handled_status(Received, MsgId, State) ->
    KnownHosts = [ X || X <- Received, X =/= {error, unknown_host} ],
    Handled    = [ X || X <- KnownHosts, X =/= undefined ],
    case {KnownHosts, Handled} of
        {[], _} ->
            {error, "551 User not local. Relay denied.", State};
        {_, []} ->
            {error, "550 No such user here", State};
        {_, _} ->
            {ok, MsgId, State}
    end.


%%% If the message is a {<<"multipart">>,<<"report">>} then here is also
%%% a {<<"message">>,<<"rfc822">>} part that contains the original message.
%%% From that original message we can find the original message id

%% @doc A message is classified as a bounce if the recipient is noreply+MSGID@@example.org
%% OR if the Return-Path is set to an empty address and other appropriate headers are present
find_bounce_id(Type, Recipients, Headers) ->
    case find_bounce_email(Recipients) of
        {ok, _MessageId} = M -> 
            M;
        undefined ->
            case z_email_receive_check:is_bounce(Type, Headers) of
                true -> ok;
                false -> no_bounce
            end
    end.

% Check if one of the recipients is a bounce address
find_bounce_email([]) ->
    undefined;
find_bounce_email([To|Other]) ->
    case z_email_server:is_bounce_email_address(To) of
        true -> {ok, To};
        false -> find_bounce_email(Other)
    end.

%% Smtp listen to IP address, Domain and Port
get_listen_domain() ->
    case os:getenv("ZOTONIC_SMTP_LISTEN_DOMAIN") of
        false -> z_config:get(smtp_listen_domain);
        SmtpListenDomain_ -> SmtpListenDomain_
    end.

get_listen_ip() ->
    case os:getenv("ZOTONIC_SMTP_LISTEN_IP") of
        false -> z_config:get(smtp_listen_ip);
        SmtpListenAny when SmtpListenAny == []; SmtpListenAny == "*"; SmtpListenAny == "any" -> any;
        SmtpListenIp_-> SmtpListenIp_
    end.

get_listen_port() ->
    case os:getenv("ZOTONIC_SMTP_LISTEN_PORT") of
        false -> z_config:get(smtp_listen_port);
        SmtpListenPort_ -> list_to_integer(SmtpListenPort_)
    end.


decode(Data) ->
    try 
        {ok, mimemail:decode(Data)}
    catch
        Type:Reason ->
            {error, {Type,Reason}}
    end.

%% TODO: Add 'for user@example.com' header (depends on recipient, we might have multiple)
%% TODO: Add Return-Path
add_received_header(Data, MsgId, State) ->
    iolist_to_binary([
        <<"Received:">>,
        <<" from [">>, inet_parse:ntoa(State#state.peer), <<"] (helo=">>, filter_string(State#state.helo), <<")">>,
        <<"\r\n\tby ">>, State#state.hostname, <<" with ESMTP (Zotonic ">>, ?ZOTONIC_VERSION, <<")">>,
        <<"\r\n\t(envelope-from <">>, filter_string(State#state.from), <<">)">>, 
        <<"\r\n\tid ">>, MsgId, 
        <<"; ">>, z_dateformat:format(calendar:local_time(), "r", []),
        <<"\r\n">>,
        Data
    ]).

filter_string(S) ->
    lists:filter(fun(C) ->
                            C >= 32 
                    andalso C =< 126
                    andalso C =/= $[
                    andalso C =/= $]
                    andalso C =/= $(
                    andalso C =/= $)
                    andalso C =/= $<
                    andalso C =/= $>
                    andalso C =/= $\\
                 end,
                 z_convert:to_list(S)).
