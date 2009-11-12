%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-02
%%
%% @doc Send e-mail to a recipient. Optionally queue low priority messages.

%% Copyright 2009 Marc Worrell
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
	get_admin_email/1,
	send_admin/3,
	
	send/2,
	
    send/4,
    send_render/4,
    send_render/5,

    sendq/4,
    sendq_render/4,
    sendq_render/5
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the e-mail address of the site administrator
get_admin_email(Context) ->
	case m_config:get_value(zotonic, admin_email, Context) of
		undefined -> 
			case m_site:get(admin_email, Context) of
				undefined -> 
					case m_rsc:p_no_acl(1, email, Context) of
						Empty when Empty == undefined orelse Empty == <<>> ->
							hd(string:tokens("wwwadmin@" ++ z_convert:to_list(m_site:get(hostname, Context)), ":"));
						Email -> Email
					end;
				Email -> Email
			end;
		Email -> Email
	end.

%% @doc Send a simple text message to the administrator
send_admin(Subject, Message, Context) ->
	case get_admin_email(Context) of
		undefined -> error;
		Email -> z_notifier:notify1(#email{queue=false, to=Email, subject=Subject, text=Message}, Context)
	end.

%% @doc Send an email message defined by the email record.
send(#email{} = Email, Context) ->
	z_notifier:notify1(Email, Context).

%% @doc Send a simple text message to an email address
send(To, Subject, Message, Context) ->
	z_notifier:notify1(#email{queue=false, to=To, subject=Subject, text=Message}, Context).

%% @doc Queue a simple text message to an email address
sendq(To, Subject, Message, Context) ->
	z_notifier:notify1(#email{queue=true, to=To, subject=Subject, text=Message}, Context).

%% @doc Send a html message to an email address, render the message using a template.
send_render(To, HtmlTemplate, Vars, Context) ->
    send_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Send a html and text message to an email address, render the message using two templates.
send_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	z_notifier:notify1(#email{queue=false, to=To, html_tpl=HtmlTemplate, text_tpl=TextTemplate, vars=Vars}, Context).

%% @doc Queue a html message to an email address, render the message using a template.
sendq_render(To, HtmlTemplate, Vars, Context) ->
    sendq_render(To, HtmlTemplate, undefined, Vars, Context).

%% @doc Queue a html and text message to an email address, render the message using two templates.
sendq_render(To, HtmlTemplate, TextTemplate, Vars, Context) ->
	z_notifier:notify1(#email{queue=true, to=To, html_tpl=HtmlTemplate, text_tpl=TextTemplate, vars=Vars}, Context).

