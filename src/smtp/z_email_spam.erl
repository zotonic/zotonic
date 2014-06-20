%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Maximonster Interactive Things
%% @copyright 2014 Marc Worrell
%% @doc Check spam score for received e-mail. Accept, bounce, drop or soft-error.

%% Copyright 2011 Maximonster Interactive Things
%% Copyright 2014 Marc Worrell
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

-module(z_email_spam).
-author("Atilla Erdodi <atilla.erdodi@gmail.com>").
-author("Marc Worrell <marc@worrell.nl>").

-export([ 
    spam_check/1,
    smtp_status/4
]).

-export([ test/0 ]).

% Timeout value for the connection of the spamassassin daemon
-define(SPAMD_TIMEOUT, 60000).


-spec spam_check(binary()) -> {ok, {ham|spam, SpamdStatus::list({binary(),binary()}), SpamdHeaders::list({binary(),binary()})}} | {error, term()}.
spam_check(EncodedMail) ->
    SmtpSpamdIp = z_config:get(smtp_spamd_ip),
    SmtpSpamdPort = z_config:get(smtp_spamd_port, 783),
    case {SmtpSpamdIp, SmtpSpamdPort} of
        {Addr, _Port} when Addr =:= [] orelse Addr =:= undefined ->
            {ok, {ham, [], []}};
        {Addr, Port} ->
            spamcheck(EncodedMail, Addr, Port)
    end.


%% TODO: check the spam status to give a specific message.
smtp_status(_SpamStatus, _From, _To, Peer) ->
    io_lib:format("451 Your message was classified as spam. Check if your mail server [~s] is black listed.",
                  [inet_parse:ntoa(Peer)]).


spamcheck(EncodedMail, SpamDServer, SpamDPort) ->
    {ok, Socket} = gen_tcp:connect(SpamDServer, SpamDPort, [binary]),
    gen_tcp:send(Socket, [
    	<<"HEADERS SPAMC/1.2\r\n">>,
    	<<"Content-length: ">>, integer_to_list(size(EncodedMail) + 2), <<"\r\n">>,
    	<<"User: spamd\r\n">>,
    	<<"\r\n">>,
    	EncodedMail,
    	<<"\r\n">>]),
    case recv_spamd(Socket, <<>>) of
        {ok, Response} ->
            SpamHeaders = parse_response(Response),
            SpamStatus = proplists:get_value(<<"X-Spam-Status">>, SpamHeaders),
            check_status(spam_status(SpamStatus), SpamHeaders);
        {error, Reason} = Error ->
            lager:error("spamcheck error ~p", [Reason]),
            Error
    end.

check_status({true, Args}, Headers) ->
	{ok, {spam, Args, Headers}};
check_status({false, Args}, Headers) ->
	{ok, {ham, Args, Headers}}.

spam_status(<<"Yes,", RestStatus/binary>>) -> {true, parse_status_args(RestStatus)};
spam_status(<<"No,", RestStatus/binary>>) -> {false, parse_status_args(RestStatus)}.

recv_spamd(Socket, Res) ->
    receive
        {tcp, Socket, <<"SPAMD/1.1 0 EX_OK\r\n", Data/binary>>} ->
            recv_spamd(Socket, Data);
        {tcp, Socket, Data} ->
            recv_spamd(Socket, <<Res/binary, Data/binary>>);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            {ok, Res}
    after ?SPAMD_TIMEOUT ->
        gen_tcp:close(Socket),
        {error, timeout}
    end.

parse_response(Response) ->
    {_ContentLengthHdr, Rest1} = mimemail:parse_headers(Response),
    {Headers, _Rest2} = mimemail:parse_headers(Rest1),
    lists:filter(fun is_spamd_header/1, Headers).

is_spamd_header({<<"X-Spam", _/binary>>, _}) -> true;
is_spamd_header(_) -> false.

parse_status_args(Status) ->
    parse_status_args(Status, []).

parse_status_args(<<>>, Args) ->
    [ parse_expand_arg(KV) || KV <- lists:reverse(Args) ];
parse_status_args(<<WS, Rest/binary>>, Args) when WS =< 32 ->
    parse_status_args(Rest, Args);
parse_status_args(Rest, Args) ->
    case binary:split(Rest, <<"=">>) of
        [Arg,<<$[, VR/binary>>] ->
            [SubArgs,Rest1] = binary:split(VR,<<"]">>),
            SAs = binary:split(SubArgs, <<",">>, [global,trim]),
            SAs1 = [ z_string:trim(S) || S <- SAs ],
            SubKVs = [ binary:split(KV, <<"=">>) || KV <- SAs1 ],
            parse_status_args(Rest1, [{Arg,SubKVs}|Args]);
        [Arg,VR] ->
            fetch_value(VR, <<>>, Arg, Args)
    end.

fetch_value(<<>>, V, Arg, Args) ->
    parse_status_args(<<>>, [{Arg,V}|Args]);
fetch_value(<<C,Rest/binary>>, V, Arg, Args) when C =:= 13; C=:= 10; C=:= 9 ->
    fetch_value(Rest, V, Arg, Args);
fetch_value(<<C,Rest/binary>>, V, Arg, Args) when C =< 32 ->
    parse_status_args(Rest, [{Arg,V}|Args]);
fetch_value(<<C,Rest/binary>>, V, Arg, Args) ->
    fetch_value(Rest, <<V/binary, C>>, Arg, Args).

parse_expand_arg({<<"tests">>,V}) when is_binary(V) ->
    Ks = binary:split(V, <<",">>, [global,trim]),
    {<<"tests">>, [{K,<<>>} || K <- Ks]};
parse_expand_arg(KV) ->
    KV.


test() ->
    {false,[
      {<<"score">>,<<"-0.11">>},
      {<<"tagged_above">>,<<"-10">>},
      {<<"required">>,<<"4.9">>},
      {<<"tests">>,
       [[<<"AWL">>,<<"0.159">>],
        [<<"BAYES_00">>,<<"-1.9">>],
        [<<"HTML_MESSAGE">>,<<"0.001">>],
        [<<"MIME_HTML_ONLY">>,<<"2">>],
        [<<"RP_MATCHES_RCVD">>,<<"-0.38">>],
        [<<"T_REMOTE_IMAGE">>,<<"0.01">>]]},
      {<<"autolearn">>,<<"no">>}]} = spam_status(
    <<"No, score=-0.11 tagged_above=-10 required=4.9 tests=[AWL=0.159,\r\n"
      "\tBAYES_00=-1.9, HTML_MESSAGE=0.001, MIME_HTML_ONLY=2,\r\n"
      "\tRP_MATCHES_RCVD=-0.38, T_REMOTE_IMAGE=0.01] autolearn=no">>),

    {true,[
       {<<"score">>,<<"1001.1">>},
       {<<"required">>,<<"5.0">>},
       {<<"tests">>,
        [{<<"ALL_TRUSTED">>,<<>>},
         {<<"DATE_IN_PAST_96_XX">>,<<>>},
         {<<"GTUBE">>,<<>>},
         {<<"HEADER_FROM_DIFFERENT_DOMAINS">>,<<>>}]},
       {<<"autolearn">>,<<"noautolearn_force=no">>},
       {<<"version">>,<<"3.4.0'">>}]} = spam_status(
      <<"Yes, score=1001.1 required=5.0 tests=ALL_TRUSTED,\r\n"
        "\tDATE_IN_PAST_96_XX,GTUBE,HEADER_FROM_DIFFERENT_DOMAINS autolearn=no\r\n"
        "\tautolearn_force=no version=3.4.0'">>),
    ok.
