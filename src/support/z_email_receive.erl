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

%% interface functions
-export([
	received/8
]).

-include_lib("zotonic.hrl").

%% @doc Handle a received e-mail
received(Recipients, From, Reference, {Type, Subtype}, Headers, Params, Body, Data) ->
	ParsedEmail = parse_email({Type, Subtype}, Headers, Params, Body),
	?DEBUG(ParsedEmail),
	ParsedEmail1 = ParsedEmail#email{to=Recipients, from=From, raw=Data},
	[
		case get_host(Recipient) of
			{ok, LocalPart, Domain, Host} ->
				% TODO: lookup the localpart in the identity table?
				%       --> better let a site module handle that
				z_notifier:notify(#email_received{
										localpart=LocalPart,
										domain=Domain,
										to=Recipient,
										from=From,
										reference=Reference,
										email=ParsedEmail1,
										headers=Headers
									}, 
									z_context:new(Host));
			undefined ->
				error_logger:info_msg("SMTP Unknown host for recipient: ~p", [Recipient]),
				skip
		end
		|| Recipient <- Recipients
	].

get_host(Recipient) ->
	[Username, Domain] = binstr:split(Recipient, <<"@">>, 2),
	case z_sites_dispatcher:get_host_for_domain(Domain) of
		{ok, Host} ->
			{ok, Username, Domain, Host};
		undefined ->
			undefined
	end.


%% @doc Parse an #email_received to a sanitized #email try to make sense of all parts
%%      All bodies are converted from the charset in the headers to the UTF-8
%%      This might not be correct for HTML bodies, where we have to check the content-type in
%%      the header (but then not in all cases...)
parse_email({<<"text">>, <<"plain">>}, Headers, _Params, Body) ->
	#email{
		subject=proplists:get_value(<<"Subject">>, Headers),
		text=Body,
		html=z_html:escape_link(Body)
	};

parse_email({<<"text">>, <<"html">>}, Headers, _Params, Body) ->
	Html = z_html:sanitize(Body),
	Text = z_markdown:to_markdown(Html, [no_html]),
	#email{
		subject=proplists:get_value(<<"Subject">>, Headers),
		text=Text,
		html=Html
	};

parse_email({Type, Subtype}, Headers, Params, Body) ->
	?DEBUG({{Type, Subtype}, Headers, Params, Body}),
	#email{}.
