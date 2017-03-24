%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%%
%% @doc Send e-mail to a recipient. Optionally queue low priority messages.

%% Copyright 2009-2017 Marc Worrell
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

-define(EMAIL_SRV, z_email_server).


% The email domain depends on the site sending the e-mail
-spec email_domain(z:context()) -> binary().
email_domain(Context) ->
    case m_config:get_value(site, smtphost, Context) of
        undefined -> z_context:hostname(Context);
        SmtpHost -> z_convert:to_binary(SmtpHost)
    end.

% Ensure that the sites's domain is attached to the email address.
-spec ensure_domain(binary()|string(), z:context()) -> binary().
ensure_domain(Email, Context) when is_binary(Email) ->
    case binary:match(Email, <<"@">>) of
        {_,_} -> Email;
        nomatch -> <<Email/binary, "$@", (email_domain(Context))/binary>>
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


%% @doc Fetch the e-mail address of the site administrator
-spec get_admin_email(z:context()) -> binary().
get_admin_email(Context) ->
	case m_config:get_value(zotonic, admin_email, Context) of
		undefined ->
			case m_site:get(admin_email, Context) of
				undefined ->
					case m_rsc:p_no_acl(1, email_raw, Context) of
						Empty when Empty == undefined orelse Empty == <<>> ->
                            <<"wwwadmin@", (z_context:hostname(Context))/binary>>;
						Email -> Email
					end;
				Email -> z_convert:to_binary(Email)
			end;
		Email -> z_convert:to_binary(Email)
	end.

%% @doc Send a simple text message to the administrator
-spec send_admin(iolist(), iolist(), z:context()) ->
          {ok, MsgId::binary()}
        | {error, no_admin_email|sender_disabled|term()}.
send_admin(Subject, Message, Context) ->
	case get_admin_email(Context) of
		undefined ->
			{error, no_admin_email};
		Email ->
			Subject1 = [
				$[,
				z_context:hostname(Context),
				"] ",
				Subject
			],
			Message1 = [
				Message,
				"\n\n-- \nYou receive this e-mail because you are registered as the admin of the site ",
				z_context:abs_url("/", Context)
			],
			z_email_server:send(#email{queue=false, to=Email, subject=Subject1, text=Message1}, Context)
	end.


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
	z_email_server:send(Email, Context).

send(MsgId, #email{} = Email, Context) ->
	z_email_server:send(MsgId, Email, Context).

%% @doc Send a simple text message to an email address
send(To, Subject, Message, Context) ->
	z_email_server:send(#email{queue=false, to=To, subject=Subject, text=Message}, Context).

%% @doc Queue a simple text message to an email address
sendq(To, Subject, Message, Context) ->
	z_email_server:send(#email{queue=true, to=To, subject=Subject, text=Message}, Context).

%% @doc Send a html message to an email address, render the message using a template.
send_render(To, HtmlTemplate, Vars, Context) ->
    send_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Send a html and text message to an email address, render the message using two templates.
send_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	z_email_server:send(#email{queue=false, to=To, from=proplists:get_value(email_from, Vars),
	                        html_tpl=HtmlTemplate, text_tpl=TextTemplate, vars=Vars}, Context).

%% @doc Queue a html message to an email address, render the message using a template.
sendq_render(To, HtmlTemplate, Vars, Context) ->
    sendq_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Queue a html and text message to an email address, render the message using two templates.
sendq_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	z_email_server:send(#email{queue=true, to=To, from=proplists:get_value(email_from, Vars),
	                             html_tpl=HtmlTemplate, text_tpl=TextTemplate, vars=Vars}, Context).


%% @doc Combine a name and an email address to the format `jan janssen <jan@example.com>'
-spec combine_name_email(Name::binary()|string(), Email::binary()|string()) -> binary().
combine_name_email(Name, Email) ->
    Name1 = z_convert:to_binary(Name),
    Email1 = z_convert:to_binary(Email),
    case Name1 of
        <<>> -> Email1;
        _ ->
            iolist_to_binary([
                $",rfc2047:encode(z_string:trim(filter_name(Name1))),$",
                " <", Email1, ">"
            ])
    end.

filter_name(Name) ->
    filter_name(Name, <<>>).

filter_name(<<>>, Acc) -> Acc;
filter_name(<<$", N/binary>>, Acc) -> filter_name(N, <<Acc/binary, " ">>);
filter_name(<<$<, N/binary>>, Acc) -> filter_name(N, <<Acc/binary, " ">>);
filter_name(<<$>, N/binary>>, Acc) -> filter_name(N, <<Acc/binary, " ">>);
filter_name(<<C, N/binary>>, Acc) when C < 32 -> filter_name(N, <<Acc/binary, " ">>);
filter_name(<<C/utf8, N/binary>>, Acc) -> filter_name(N, <<Acc/binary, C/utf8>>).


%% @doc Split the name and email from the format `jan janssen <jan@example.com>'
-spec split_name_email(binary()|string()) -> {binary(), binary()}.
split_name_email(Email) ->
    Email1 = z_string:trim(rfc2047:decode(Email)),
    case split_ne(Email1, in_name, <<>>, <<>>) of
        {ends_in_name, E} ->
			% Only e-mail
            {<<>>, z_string:trim(z_convert:to_binary(E))};
        {N, E} ->
			% E-mail and name
            {z_string:trim(z_convert:to_binary(N)), z_string:trim(z_convert:to_binary(E))}
    end.

split_ne(<<>>, in_name, <<>>, Acc) ->
    {ends_in_name, Acc};
split_ne(<<>>, in_qname, <<>>, Acc) ->
    {ends_in_name, Acc};
split_ne(<<>>, to_email, <<>>, Acc) ->
    {ends_in_name, Acc};
split_ne(<<>>, _, Name, Acc) ->
    {Name, Acc};
split_ne(<<$<,T/binary>>, to_email, Name, Acc) ->
    split_ne(T, in_email, <<Name/binary, Acc/binary>>, <<>>);
split_ne(<<$<,T/binary>>, in_name, Name, Acc) ->
    split_ne(T, in_email, <<Name/binary, Acc/binary>>, <<>>);
split_ne(<<$>,_/binary>>, in_email, Name, Acc) ->
    {Name, Acc};
split_ne(<<$",T/binary>>, in_name, <<>>, Acc) ->
    split_ne(T, in_qname, <<>>, Acc);
split_ne(<<$",T/binary>>, in_qname, <<>>, Acc) ->
    split_ne(T, to_email, Acc, <<>>);
split_ne(<<H/utf8,T/binary>>, to_email, Name, Acc) ->
    split_ne(T, to_email, Name, <<Acc/binary,H/utf8>>);
split_ne(<<H/utf8,T/binary>>, State, Name, Acc) ->
    split_ne(T, State, Name, <<Acc/binary,H/utf8>>).

