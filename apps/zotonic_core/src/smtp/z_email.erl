%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Send e-mail to a recipient. Optionally queue low priority messages.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(z_email).
-author("Marc Worrell <marc@worrell.nl>").

%% interface functions
-export([
    email_domain/1,
    ensure_domain/2,
    bounce_domain/1,

	get_admin_email/1,
    get_email_from/1,
    format_recipient/2,
    ensure_to_email/2,

	send_admin/3,

	send_page/3,

	send/2,
	send/3,

    send/4,
    send_render/4,
    send_render/5,

    sendq/4,
    sendq_render/4,
    sendq_render/5,

    split_name_email/1,
    combine_name_email/2
]).

-include_lib("zotonic.hrl").


% The email domain depends on the site sending the e-mail
-spec email_domain(z:context()) -> binary().
email_domain(Context) ->
    case z_convert:to_binary( m_config:get_value(site, smtphost, Context) ) of
        <<>> -> z_context:hostname(Context);
        SmtpHost -> SmtpHost
    end.

% Ensure that the sites's domain is attached to the email address.
-spec ensure_domain(binary()|string(), z:context()) -> binary().
ensure_domain(Email, Context) when is_binary(Email) ->
    case binary:match(Email, <<"@">>) of
        {_,_} -> Email;
        nomatch -> <<Email/binary, $@, (email_domain(Context))/binary>>
    end;
ensure_domain(Email, Context) ->
    ensure_domain(z_convert:to_binary(Email), Context).


% Bounces can be forced to a different e-mail server altogether
-spec bounce_domain(z:context()) -> binary().
bounce_domain(Context) ->
    case z_config:get('smtp_bounce_domain') of
        undefined -> email_domain(Context);
        BounceDomain -> z_convert:to_binary(BounceDomain)
    end.

%% @doc Return the email address to be used in emails for the
%% given id. The address will be formatted using the recipient's name.
-spec format_recipient(RecipientId, Context) -> {ok, EmailAddress} | {error, Reason} when
    RecipientId :: m_rsc:resource(),
    Context :: z:context(),
    EmailAddress :: binary(),
    Reason :: enoent | no_email | eacces.
format_recipient(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            case z_acl:rsc_visible(RscId, Context) of
                true ->
                    case m_rsc:p_no_acl(Id, email_raw, Context) of
                        undefined ->
                            {error, no_email};
                        Email ->
                            {Name, _NameCtx} = z_template:render_to_iolist("_name.tpl", [{id, RscId}], Context),
                            {ok, combine_name_email(iolist_to_binary(Name), Email)}
                    end;
                false ->
                    {error, eacces}
            end
    end.

%% @doc If the recipient is an resource id, ensure that it is formatted as an email address.
-spec ensure_to_email( #email{}, z:context() ) -> {ok, #email{}} | {error, term()}.
ensure_to_email(#email{ to = undefined }, _Context) ->
    {error, no_recipient};
ensure_to_email(#email{ to = <<>> }, _Context) ->
    {error, no_recipient};
ensure_to_email(#email{ to = "" }, _Context) ->
    {error, no_recipient};
ensure_to_email(#email{ to = Id } = E, Context) when is_integer(Id) ->
    case format_recipient(Id, Context) of
        {ok, To} ->
            {ok, E#email{ to = To }};
        {error, _} = Error ->
            Error
    end;
ensure_to_email(#email{} = E, _Context) ->
    {ok, E}.

%% @doc Fetch the e-mail address of the site administrator
-spec get_admin_email(z:context()) -> binary().
get_admin_email(Context) ->
	case m_config:get_value(zotonic, admin_email, Context) of
		undefined ->
			case m_site:get(admin_email, Context) of
				undefined ->
					case m_rsc:p_no_acl(1, email_raw, Context) of
                        undefined -> wwwadmin_email(Context);
                        <<>> -> wwwadmin_email(Context);
						Email when is_binary(Email) -> Email
					end;
				Email ->
                    z_convert:to_binary(Email)
			end;
		Email ->
            z_convert:to_binary(Email)
	end.

%% @doc Fetch the default From e-mail address. Defaults to noreply@hostname
-spec get_email_from(z:context()) -> binary().
get_email_from(Context) ->
    z_email_server:get_email_from(Context).


wwwadmin_email(Context) ->
    <<"wwwadmin@", (z_context:hostname(Context))/binary>>.

%% @doc Send a simple text message to the administrator
-spec send_admin(iodata(), iodata(), z:context()) ->
          {ok, MsgId::binary()}
        | {error, sender_disabled|term()}.
send_admin(Subject, Message, Context) ->
	Email = get_admin_email(Context),
	Subject1 = iolist_to_binary([ $[, z_context:hostname(Context), "] ", Subject ]),
	Message1 = iolist_to_binary([
		Message,
		"\n\n-- \nYou receive this e-mail because you are registered as the admin of the site ",
		z_context:abs_url(<<"/">>, Context)
	]),
	z_email_server:send(#email{queue=false, to=Email, subject=Subject1, text=Message1}, Context).


%% @doc Send a page to an e-mail address, assumes the correct template "mailing_page.tpl" is available.
%%		 Defaults for these pages are supplied by mod_mailinglist.
send_page(undefined, _Id, _Context) ->
	{error, not_email};
send_page(_Email, undefined, _Context) ->
	{error, not_found};
send_page(Email, Id, Context) when is_integer(Id) ->
	Vars = [
		{id, Id},
		{recipient, Email}
	],
	case m_rsc:is_a(Id, document, Context) of
		false ->
			send_render(Email, {cat, "mailing_page.tpl"}, Vars, Context);
		true ->
			E = #email{
				to=Email,
				html_tpl={cat, "mailing_page.tpl"},
				vars=Vars,
				attachments=[Id]
			},
			send(E, Context)
	end;
send_page(Email, Id, Context) ->
	send_page(Email, m_rsc:rid(Id, Context), Context).


%% @doc Send an email message defined by the email record.
send(#email{} = Email, Context) ->
    case ensure_to_email(Email, Context) of
        {ok, Email1} ->
            z_email_server:send(Email1, Context);
        {error, _} = Error ->
            Error
    end.

send(MsgId, #email{} = Email, Context) ->
	z_email_server:send(MsgId, Email, Context).

%% @doc Send a simple text message to an email address
send(To, Subject, Message, Context) ->
	send(#email{queue=false, to=To, subject=Subject, text=Message}, Context).

%% @doc Queue a simple text message to an email address
sendq(To, Subject, Message, Context) ->
	send(#email{queue=true, to=To, subject=Subject, text=Message}, Context).

%% @doc Send a html message to an email address, render the message using a template.
send_render(To, HtmlTemplate, Vars, Context) ->
    send_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Send a html and text message to an email address, render the message using two templates.
send_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	send(#email{queue=false, to=To, from=proplists:get_value(email_from, Vars),
	            html_tpl=HtmlTemplate, text_tpl=TextTemplate, vars=Vars}, Context).

%% @doc Queue a html message to an email address, render the message using a template.
sendq_render(To, HtmlTemplate, Vars, Context) ->
    sendq_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Queue a html and text message to an email address, render the message using two templates.
sendq_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	send(#email{queue=true, to=To, from=proplists:get_value(email_from, Vars),
	            html_tpl=HtmlTemplate, text_tpl=TextTemplate, vars=Vars}, Context).


%% @doc Combine a name and an email address to the format `jan janssen <jan@example.com>'
%% @todo do we need rfc2047:encode/1 call here?
-spec combine_name_email(Name, Email) -> NameEmail when
    Name :: binary() | string() | undefined,
    Email :: binary() | string(),
    NameEmail :: binary().
combine_name_email(undefined, Email) ->
    Email;
combine_name_email(Name, Email) ->
    Name1 = z_string:trim(unicode:characters_to_binary(Name)),
    Email1 = unicode:characters_to_binary(Email),
    smtp_util:combine_rfc822_addresses([{Name1, Email1}]).


%% @doc Split the name and email from the format `jan janssen <jan@example.com>'
-spec split_name_email(String) -> {Name, Email} when
    String :: string() | binary(),
    Name :: binary(),
    Email :: binary().
split_name_email(Email) ->
    Email1 = z_string:trim(rfc2047:decode(unicode:characters_to_binary(Email, utf8))),
    case smtp_util:parse_rfc5322_addresses(Email1) of
        {ok, [{N,E}|_]} ->
            {z_string:trim(unicode:characters_to_binary(b(N), utf8)), unicode:characters_to_binary(b(E), utf8)};
        {error,{1,smtp_rfc5322_parse,["syntax error before: ","'>'"]}} ->
            % Issue parsing emails without domain, add a domain before the final '>' and try again
            Email2 = iolist_to_binary(re:replace(Email1, <<">$">>, <<"x@example.com>">>)),
            case smtp_util:parse_rfc5322_addresses(Email2) of
                {ok, [{N,E}|_]} ->
                    N1 = z_string:trim(unicode:characters_to_binary(b(N), utf8)),
                    E1 = unicode:characters_to_binary(re:replace(b(E), <<"x@example.com$">>, <<>>), utf8),
                    {N1, E1};
                {error, _} ->
                    {z_string:trim(z_convert:to_binary(Email1)), <<>>}
            end;
        {error, _} ->
            {z_string:trim(z_convert:to_binary(Email1)), <<>>}
    end.

b(undefined) -> <<>>;
b(S) -> S.

