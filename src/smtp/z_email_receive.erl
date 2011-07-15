%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Handle received e-mail.

%% Copyright 2011 Marc Worrell
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

-module(z_email_receive).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	received/9,
	get_host/1
]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_log.hrl").

%% @doc Handle a received e-mail
received(Recipients, From, Peer, Reference, {Type, Subtype}, Headers, Params, Body, Data) ->
	ParsedEmail = parse_email({Type, Subtype}, Headers, Params, Body),
	ParsedEmail1 = generate_text(generate_html(ParsedEmail)),
	ParsedEmail2 = ParsedEmail1#email{
						subject=proplists:get_value(<<"Subject">>, Headers),
						to=Recipients,
						from=From
					},
	[
		case get_host(Recipient) of
			{ok, LocalPart, LocalTags, Domain, Host} ->
			    Context = z_context:new(Host),
			    z_notifier:notify({log, #log_email{
			                            severity = ?LOG_INFO,
                                        mailer_status = received,
                                        mailer_host = z_convert:ip_to_list(Peer),
			                            message_nr = Reference,
                                        envelop_to = Recipient,
                                        envelop_from = From,
                                        props = [
                                            {headers, Headers}
                                        ]
			                        }},
			                        Context),
				z_notifier:notify(#email_received{
										localpart=LocalPart,
										localtags=LocalTags,
										domain=Domain,
										to=Recipient,
										from=From,
										reference=Reference,
										email=ParsedEmail2,
										headers=Headers,
										decoded={Type, Subtype, Headers, Params, Body},
										raw=Data
									}, 
									Context);
			undefined ->
				error_logger:info_msg("SMTP Dropping message, unknown host for recipient: ~p", [Recipient]),
				skip
		end
		|| Recipient <- Recipients
	].

get_host(Recipient) ->
	[Username, Domain] = binstr:split(Recipient, <<"@">>, 2),
	[LocalPart|LocalTags] = binstr:split(Username, <<"+">>),
	case z_sites_dispatcher:get_host_for_domain(Domain) of
		{ok, Host} ->
			{ok, LocalPart, LocalTags, Domain, Host};
		undefined ->
			undefined
	end.


%% @doc Parse an #email_received to a sanitized #email try to make sense of all parts
%%      All bodies are converted from the charset in the headers to the UTF-8
%%      This might not be correct for HTML bodies, where we have to check the content-type in
%%      the header (but then not in all cases...)
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

parse_email({<<"multipart">>, <<"alternative">>}, Headers, _Params, Body) ->
	Parts = [
		parse_email({PartType, PartSubType}, PartHs, PartPs, PartBody)
		|| {PartType, PartSubType, PartHs, PartPs, PartBody} <- Body
	],
	lists:foldr(fun(A,B) -> merge_email(A,B) end, 
				#email{subject=proplists:get_value(<<"Subject">>, Headers)}, 
				Parts);

parse_email({<<"multipart">>, <<"related">>}, Headers, _Params, Body) ->
	Parts = [
		parse_email({PartType, PartSubType}, PartHs, PartPs, PartBody)
		|| {PartType, PartSubType, PartHs, PartPs, PartBody} <- Body
	],
	lists:foldr(fun(A,B) -> merge_email(A,B) end, 
				#email{subject=proplists:get_value(<<"Subject">>, Headers)}, 
				Parts);

parse_email({<<"multipart">>, <<"mixed">>}, Headers, _Params, Body) ->
	Parts = [
		parse_email({PartType, PartSubType}, PartHs, PartPs, PartBody)
		|| {PartType, PartSubType, PartHs, PartPs, PartBody} <- Body
	],
	lists:foldr(fun(A,B) -> append_email(A,B) end, 
				#email{subject=proplists:get_value(<<"Subject">>, Headers)}, 
				Parts);

parse_email({<<"multipart">>, <<"digest">>}, Headers, _Params, Body) ->
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
	Text = iolist_to_binary([
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
	Html = iolist_to_binary([
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
	
% Fallback to attachments
parse_email(Mime, _Headers, Params, Body) ->
	attachment(Mime, Params, Body).


% Merge two email parts
merge_email(A, B) ->
	A#email{
		subject=take_defined(A#email.subject, B#email.subject),
		text=take_defined(A#email.text, B#email.text), 
		html=take_defined(A#email.html, B#email.html), 
		attachments=A#email.attachments++B#email.attachments
	}.

% Append two e-mails (used in multipart/mixed messages)
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
	case proplists:get_value(<<"disposition">>, Params, <<"inline">>) of
		<<"attachment">> -> attachment(Mime, Params, Body);
		_ -> undefined
	end.

attachment({Type, SubType}, Params, Body) ->
	DispParams = proplists:get_value(<<"disposition-params">>, Params, []),
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
    