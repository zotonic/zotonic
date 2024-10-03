%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2024 Marc Worrell
%% @doc Handle received e-mail.
%% @end

%% Copyright 2011-2024 Marc Worrell
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

-module(zotonic_listen_smtp_receive).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    received/9,
    get_site/1
]).

-export([
    parse_email/4,
    parse_file/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Handle a received e-mail
received(Recipients, EnvelopFrom, Peer, Reference, {Type, Subtype}, Headers, Params, Body, Data) ->
    ParsedEmail = parse_email({Type, Subtype}, Headers, Params, Body),
    ParsedEmail1 = generate_text(generate_html(ParsedEmail)),
    ParsedEmail2 = ParsedEmail1#email{
        subject = sanitize_utf8(proplists:get_value(<<"Subject">>, Headers)),
        to = Recipients,
        from = sanitize_utf8(EnvelopFrom),
        html = sanitize_utf8(ParsedEmail1#email.html),
        text = sanitize_utf8(ParsedEmail1#email.text)
    },
    lists:map(
        fun(Recipient) ->
            received_1(
                Recipient, ParsedEmail2,
                Peer, Reference,
                {Type, Subtype}, Headers, Params,
                Body, Data)
        end,
        Recipients).

received_1(Recipient, ParsedEmail,
         Peer, Reference,
         {Type, Subtype}, Headers, Params,
         Body, Data) ->
    case get_site(Recipient) of
        {ok, {LocalPart, LocalTags, Domain, Site}} ->
            ?LOG_INFO(#{
                text => <<"SMTP received email">>,
                in => zotonic_listen_smtp,
                recipient => Recipient,
                localpart => LocalPart,
                localtags => LocalTags,
                domain => Domain,
                site => Site
            }),
            Context = z_context:new(Site),
            z_notifier:notify(
                #zlog{
                    props=#log_email{
                        severity = ?LOG_LEVEL_INFO,
                        mailer_status = received,
                        mailer_host = z_convert:ip_to_list(Peer),
                        message_nr = Reference,
                        envelop_to = Recipient,
                        envelop_from = ParsedEmail#email.from,
                        props = [
                            {headers, Headers}
                        ]
                    }
                },
                Context),
            LHeaders = lowercase_headers(Headers),
            case is_blocked(ParsedEmail#email.from, Context)
                orelse is_blocked(proplists:get_value(<<"from">>, LHeaders), Context)
                orelse is_blocked(proplists:get_value(<<"to">>, LHeaders), Context)
            of
                false ->
                    Email = #email_received{
                        localpart = LocalPart,
                        localtags = LocalTags,
                        domain = Domain,
                        to = Recipient,
                        from = ParsedEmail#email.from,
                        reference = Reference,
                        email = ParsedEmail,
                        headers = LHeaders,
                        decoded = {Type, Subtype, Headers, Params, Body},
                        raw = Data
                    },
                    Email1 = Email#email_received{
                            is_bulk = zotonic_listen_smtp_check:is_bulk(Email),
                            is_auto = zotonic_listen_smtp_check:is_auto(Email)
                          },
                    case z_notifier:first(Email1, Context) of
                        {ok, MsgId} when is_binary(MsgId) ->
                            ?LOG_INFO(#{
                                text => <<"SMTP received email handled">>,
                                in => zotonic_listen_smtp,
                                result => ok,
                                recipient => Recipient,
                                message_id => MsgId
                            }),
                            {ok, MsgId};
                        {ok, Other} ->
                            ?LOG_INFO(#{
                                text => <<"SMTP received email handled">>,
                                in => zotonic_listen_smtp,
                                result => ok,
                                recipient => Recipient,
                                message_id => Other
                            }),
                            {ok, undefined};
                        ok ->
                            ?LOG_INFO(#{
                                text => <<"SMTP received email handled">>,
                                in => zotonic_listen_smtp,
                                result => ok,
                                recipient => Recipient,
                                message_id => undefined
                            }),
                            {ok, undefined};
                        {error, Reason} = Error ->
                            ?LOG_WARNING(#{
                                text => <<"SMTP received email, handler error">>,
                                in => zotonic_listen_smtp,
                                result => error,
                                reason => Reason,
                                recipient => Recipient
                            }),
                            Error;
                        undefined ->
                            ?LOG_WARNING(#{
                                text => <<"SMTP received email, unhandled assuming unknown recipient">>,
                                in => zotonic_listen_smtp,
                                result => error,
                                reason => unknown_recipient,
                                recipient => Recipient
                            }),
                            {error, unknown_recipient};
                        Other ->
                            ?LOG_WARNING(#{
                                text => <<"SMTP received email, unexpected return value">>,
                                in => zotonic_listen_smtp,
                                result => error,
                                reason => Other,
                                recipient => Recipient
                            }),
                            {ok, Other}
                    end;
                true ->
                    ?LOG_NOTICE(#{
                        text => <<"[smtp] Dropping email from blocked address">>,
                        in => zotonic_core,
                        result => error,
                        reason => blocked,
                        email => ParsedEmail#email.from,
                        recipient => Recipient
                    }),
                    LogEmail = #log_email{
                        severity = ?LOG_LEVEL_WARNING,
                        mailer_status = blocked,
                        mailer_message = <<"Sender blocked by Zotonic module (#email_is_blocked)">>,
                        props = [
                            {reason, sender_blocked},
                            {headers, Headers}
                        ],
                        mailer_host = z_convert:ip_to_list(Peer),
                        message_nr = Reference,
                        envelop_to = Recipient,
                        envelop_from = ParsedEmail#email.from
                    },
                    z_notifier:notify(#zlog{ props = LogEmail }, Context),
                    % Silently ignore - do now warn sender
                    {ok, undefined}
            end;
        {error, unknown_host} ->
            ?LOG_WARNING(#{
                text => <<"SMTP received email, dropped">>,
                in => zotonic_listen_smtp,
                result => error,
                reason => unknown_host,
                recipient => Recipient
            }),
            {error, unknown_host};
        {error, not_running} ->
            ?LOG_NOTICE(#{
                text => <<"SMTP received email, host for recipient is not up">>,
                in => zotonic_listen_smtp,
                result => warning,
                reason => not_running,
                recipient => Recipient
            }),
            {error, not_running}
    end.

% @doc Check if we can receive an email from this address. If an email address is blocked
% for sending, then we also block it for receiving email.
-spec is_blocked(EmailAddress, Context) -> boolean() when
    EmailAddress :: binary() | undefined,
    Context :: z:context().
is_blocked(undefined, _Context) ->
    false;
is_blocked(EmailAddress, Context) ->
    {_Name, Email} = z_email:split_name_email(EmailAddress),
    case z_notifier:first(#email_is_blocked{ recipient = Email }, Context) of
        undefined -> false;
        true -> true;
        false -> false
    end.


-spec get_site( binary() ) ->
        {ok, {binary(), [ binary() ], binary(), atom()}}
      | {error, unknown_host | not_running}.
get_site(Recipient) ->
    [Username, Domain] = binstr:split(Recipient, <<"@">>, 2),
    [LocalPart|LocalTags] = binstr:split(Username, <<"+">>),
    case z_sites_dispatcher:get_site_for_hostname(z_string:to_lower(Domain)) of
        {ok, Site} ->
            case z_sites_manager:wait_for_running(Site) of
                ok ->
                    {ok, {LocalPart, LocalTags, Domain, Site}};
                {error, bad_name} ->
                    {error, unknown_host};
                {error, _Reason} ->
                    {error, not_running}
            end;
        undefined ->
            {error, unknown_host}
    end.

lowercase_headers(Hs) ->
    [ {z_string:to_lower(H), V} || {H,V} <- Hs ].

sanitize_utf8(undefined) ->
    undefined;
sanitize_utf8(S) ->
    z_string:sanitize_utf8(S).

%% @doc Parse a file using the html/text parse routines. This is used for testing
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} ->
            case decode(Data) of
                {ok, {Type, Subtype, Headers, Params, Body}} ->
                    LHeaders = lowercase_headers(Headers),
                    ParsedEmail = parse_email({Type,Subtype}, Headers, Params, Body),
                    From = fix_email(proplists:get_value(<<"from">>, LHeaders)),
                    To = fix_email(proplists:get_value(<<"to">>, LHeaders)),
                    ParsedEmail1 = generate_text(generate_html(ParsedEmail)),
                    ParsedEmail2 = ParsedEmail1#email{
                         subject = sanitize_utf8(proplists:get_value(<<"Subject">>, Headers)),
                         headers = LHeaders,
                         to = To,
                         body = Body,
                         from = sanitize_utf8(From),
                         html = sanitize_utf8(ParsedEmail1#email.html),
                         text = sanitize_utf8(ParsedEmail1#email.text)
                    },
                    {ok, ParsedEmail2};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

decode(Data) ->
    try
        {ok, mimemail:decode(Data)}
    catch
        Type:Reason ->
            {error, {Type,Reason}}
    end.

fix_email(undefined) ->
    <<"nobody@example.com">>;
fix_email(Email) ->
    % Fix illegal addresses like "k..allen@enron.com"
    case binary:replace(Email, <<"..">>, <<".">>, [ global ]) of
        Email -> Email;
        E1 -> fix_email(E1)
    end.

%% @doc Parse an #email_received to a sanitized #email try to make
%% sense of all parts.
%%
%% All bodies are converted from the charset in the headers to UTF-8.
%% This might not be correct for HTML bodies, where we have to check
%% the content-type in the html head (but then not in all cases...)
parse_email({<<"text">>, <<"plain">>}, _Headers, Params, Body) ->
    case opt_attachment({<<"text">>, <<"plain">>}, Params, Body) of
        #email{} = E -> E;
        undefined -> #email{text=Body}
    end;

parse_email({<<"text">>, <<"html">>}, _Headers, Params, Body) ->
    case opt_attachment({<<"text">>, <<"html">>}, Params, Body) of
        #email{} = E -> E;
        undefined -> #email{html=z_html:sanitize(Body)}
    end;

parse_email({<<"multipart">>, Mergeable}, Headers, _Params, Body)
  when Mergeable =:= <<"alternative">>; Mergeable =:= <<"related">> ->
    Parts = [
             parse_email({PartType, PartSubType}, PartHs, PartPs, PartBody)
             || {PartType, PartSubType, PartHs, PartPs, PartBody} <- Body
            ],
    lists:foldr(fun(A,B) -> merge_email(A,B) end,
                #email{subject=proplists:get_value(<<"Subject">>, Headers)},
                Parts);

parse_email({<<"multipart">>, Appendable}, Headers, _Params, Body)
  when Appendable =:= <<"mixed">>; Appendable =:= <<"digest">>; Appendable =:= <<"signed">> ->
    Parts = [
             parse_email({PartType, PartSubType}, PartHs, PartPs, PartBody)
             || {PartType, PartSubType, PartHs, PartPs, PartBody} <- Body
            ],
    lists:foldr(fun(A,B) -> append_email(A,B) end,
                #email{subject=proplists:get_value(<<"Subject">>, Headers)},
                Parts);

parse_email({<<"message">>, <<"rfc822">>}, _Headers, _Params, Body) ->
    {FwdType, FwdSubType, FwdHeaders, FwdParameters, FwdBody} = Body,
    Cc = proplists:get_value(<<"Cc">>, FwdHeaders),
    Text = iolist_to_binary(
             [
              <<"From: ">>, proplists:get_value(<<"From">>, FwdHeaders, <<>>), $\n,
              <<"To: ">>, proplists:get_value(<<"To">>, FwdHeaders, <<>>), $\n,
              case Cc of
                  undefined -> <<>>;
                  _ -> [<<"Cc: ">>, Cc, $\n]
              end,
              <<"Subject: ">>, proplists:get_value(<<"Subject">>, FwdHeaders, <<>>), $\n,
              <<"Date: ">>, proplists:get_value(<<"Date">>, FwdHeaders, <<>>), $\n,
              $\n
             ]),
    Html = iolist_to_binary(
             [
              <<"<dl class='rfc822-header'>">>,
              <<"<dt>From:</dt><dd>">>, z_html:escape(proplists:get_value(<<"From">>, FwdHeaders, <<>>)), <<"</dd>">>,
              <<"<dt>To:</dt><dd>">>, z_html:escape(proplists:get_value(<<"To">>, FwdHeaders, <<>>)), <<"</dd>">>,
              case Cc of
                  undefined -> <<>>;
                  _ -> [<<"<dt>Cc:</dt><dd>">>, z_html:escape(Cc), <<"</dd>">>]
              end,
              <<"<dt>Subject:</dt><dd>">>, z_html:escape(proplists:get_value(<<"Subject">>, FwdHeaders, <<>>)), <<"</dd>">>,
              <<"<dt>Date:</dt><dd>">>, z_html:escape(proplists:get_value(<<"Date">>, FwdHeaders, <<>>)), <<"</dd>">>,
              <<"</dl>">>
             ]),
    H = #email{html=Html, text=Text},
    E = parse_email({FwdType, FwdSubType}, FwdHeaders, FwdParameters, FwdBody),
    append_email(H, E);

%% @doc Fallback to attachments
parse_email(Mime, _Headers, Params, Body) ->
    attachment(Mime, Params, Body).

%% @doc Merge two email parts
merge_email(A, B) ->
    A#email{
      subject=take_defined(A#email.subject, B#email.subject),
      text=take_defined(A#email.text, B#email.text),
      html=take_defined(A#email.html, B#email.html),
      attachments=A#email.attachments++B#email.attachments
     }.

%% @doc Append two e-mails (used in multipart/mixed messages)
append_email(A, B) ->
    A1 = generate_text(generate_html(A)),
    B1 = generate_text(generate_html(B)),
    #email{
            subject=take_defined(A1#email.subject, B1#email.subject),
            html=append(A1#email.html, B1#email.html),
            text=append(A1#email.text, B1#email.text),
            attachments=A1#email.attachments++B1#email.attachments
          }.

take_defined(undefined, B) -> B;
take_defined(A, _) -> A.

append(undefined, B) -> B;
append(A, undefined) -> A;
append(A, B) -> <<A/binary, B/binary>>.

generate_text(#email{text=undefined, html=undefined} = E) ->
    E;
generate_text(#email{text=undefined, html=Html} = E) ->
    E#email{text=z_markdown:to_markdown(Html, [no_html])};
generate_text(E) ->
    E.

generate_html(#email{text=undefined, html=undefined} = E) ->
    E;
generate_html(#email{text=Text, html=undefined} = E) ->
    E#email{html=z_html:escape_link(Text)};
generate_html(E) ->
    E.

%% @doc Return a #email with an attachment if the disposition is attachment
opt_attachment(Mime, Params, Body) ->
    case maps:get(disposition, Params, <<"inline">>) of
        <<"attachment">> -> attachment(Mime, Params, Body);
        _ -> undefined
    end.

attachment({Type, SubType}, Params, Body) ->
    DispParams = maps:get(disposition_params, Params, []),
    #email{
            attachments=[
                         #upload{
                            mime=append_mime(Type, SubType),
                            data=Body,
                            filename=proplists:get_value(<<"filename">>, DispParams)
                           }
                        ]
          }.

append_mime(undefined, B) -> B;
append_mime(A, undefined) -> A;
append_mime(A, B) -> <<A/binary, $/, B/binary>>.

