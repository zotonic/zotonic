%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%%
%% Show a form to subscribe to a mailinglist.  Prefill the form with the account details
%% of the current user (if any).

%% Copyright 2010 Marc Worrell
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

-module(scomp_mailinglist_mailinglist_subscribe).
-behaviour(gen_scomp).

-export([vary/2, render/3, event/2]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.
render(Params, _Vars, Context) ->
    Template = proplists:get_value(template, Params, "_scomp_mailinglist_subscribe.tpl"),
    Props = [
        {id, m_rsc:rid(proplists:get_value(id, Params), Context)},
        {user_id, z_acl:user(Context)},
        {delegate, ?MODULE}
        | Params
    ],
    {ok, z_template:render(Template, Props, Context)}.


event(#submit{message={recipient_add, Props}}, Context) ->
    Id = proplists:get_value(id, Props),
    InAdmin = z_convert:to_bool(proplists:get_value(in_admin, Props)),
	case z_acl:rsc_visible(Id, Context) of
		true ->
			Email = z_context:get_q_validated(email, Context),
			Notification = case not InAdmin orelse z_convert:to_bool(z_context:get_q(send_welcome, Context)) of
				true -> send_welcome;
				false -> silent
			end,
			RecipientProps = [
			    {user_id, undefined},
			    {in_admin, InAdmin},
			    {name_first, z_string:trim(z_context:get_q(name_first, Context, ""))},
			    {name_surname_prefix, z_string:trim(z_context:get_q(name_surname_prefix, Context, ""))},
			    {name_surname, z_string:trim(z_context:get_q(name_surname, Context, ""))}
			],
			case m_mailinglist:insert_recipient(Id, Email, RecipientProps, Notification, Context) of
				ok -> 
				    case InAdmin of
				        true ->
        					z_render:wire([	{growl, [{text, "Added the recipient."}]}, 
        									{dialog_close, []},
        									{reload, []}], Context);
        				false ->
					        z_render:wire([ {slide_fade_in, [{target, "mailinglist_subscribe_done"}]},
					                        {slide_fade_out, [{target, "mailinglist_subscribe_form"}]}], Context)
        			end;
				{error, _Reason} ->
				    case InAdmin of
				        true ->
					        z_render:growl_error("Could not add the recipient.", Context);
					    false ->
					        z_render:wire([ {slide_fade_in, [{target, "mailinglist_subscribe_error"}]}], Context)
					end
			end;
		false ->
		    case InAdmin of
		        true ->
			        z_render:growl_error("You are not allowed to add or enable recipients.", Context);
			    false ->
			        z_render:wire([ {slide_fade_in, [{target, "mailinglist_subscribe_error"}]}], Context)
			end
	end;

event(#submit{message={recipient_edit, Props}}, Context) ->
    Id = proplists:get_value(id, Props),
    RcptId = proplists:get_value(recipient_id, Props),
    InAdmin = z_convert:to_bool(proplists:get_value(in_admin, Props)),
	case z_acl:rsc_visible(Id, Context) of
		true ->
			RecipientProps = [
                {email, z_context:get_q_validated(email, Context)},
			    {user_id, undefined},
			    {in_admin, InAdmin},
			    {name_first, z_string:trim(z_context:get_q(name_first, Context, ""))},
			    {name_surname_prefix, z_string:trim(z_context:get_q(name_surname_prefix, Context, ""))},
			    {name_surname, z_string:trim(z_context:get_q(name_surname, Context, ""))}
			],
            ok = m_mailinglist:update_recipient(RcptId, RecipientProps, Context),
            z_render:wire([	{growl, [{text, "Updated the recipient."}]}, 
                            {dialog_close, []},
                            {reload, []}], Context);
		false ->
		    case InAdmin of
		        true ->
			        z_render:growl_error("You are not allowed to edit recipients.", Context);
			    false ->
			        z_render:wire([ {slide_down, [{target, "mailinglist_subscribe_error"}]}], Context)
			end
	end.
