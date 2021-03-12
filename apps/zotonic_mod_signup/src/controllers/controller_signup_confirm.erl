%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2020 Marc Worrell
%% @doc Handle the signup confirmation link

%% Copyright 2010-2020 Marc Worrell
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

-module(controller_signup_confirm).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
]).
-export([event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    z_context:lager_md(Context),
    Rendered = z_template:render("signup_confirm.tpl", [], Context),
    z_context:output(Rendered, Context).


%% @doc Handle the submit of the signup form.
event(#submit{}, Context) ->
    event_confirm(z_context:get_q(<<"key">>, Context, <<>>), Context);
event(#postback{ message={confirm, [ {key, Key} ]}}, Context) ->
    event_confirm(Key, Context).

event_confirm(Key, Context) ->
    case confirm(Key, Context) of
        {ok, UserId} ->
            {ok, ContextUser} = z_auth:logon(UserId, Context),
            Location = confirm_location(UserId, ContextUser),
            % Post a onetime-token to the auth worker on the page
            % The auth worker will exchange it for a valid cookie and then perform
            % the redirect to the url.
            Token = z_authentication_tokens:encode_onetime_token(UserId, ContextUser),
            AuthMsg = #{
                token => Token,
                url => z_convert:to_binary( Location )
            },
            z_mqtt:publish(<<"~client/model/auth/post/onetime-token">>, AuthMsg, Context),
            z_render:wire([
                    {unmask, [{target, "signup_confirm_form"}]},
                    {mask, []}
                ], Context);
        {error, _Reason} ->
            z_render:wire([
                    {unmask, [{target, "signup_confirm_form"}]},
                    {show, [{target,"confirm_error"}]}
                ], Context)
    end.


confirm(Key, Context) ->
    case m_identity:lookup_by_verify_key(Key, Context) of
        undefined ->
            {error, unknown_key};
        Row ->
            UserId = proplists:get_value(rsc_id, Row),
            {ok, UserId} = m_rsc:update(
                UserId,
                #{
                    <<"is_published">> => true,
                    <<"is_verified_account">> => true
                },
                z_acl:sudo(Context)),
            m_identity:set_verified(proplists:get_value(id, Row), Context),
            z_notifier:map(#signup_confirm{id=UserId}, Context),
            {ok, UserId}
    end.

confirm_location(UserId, ContextUser) ->
    case z_convert:to_binary( z_notifier:first(#signup_confirm_redirect{ id = UserId }, ContextUser) ) of
        <<>> -> m_rsc:p_no_acl(UserId, page_url, ContextUser);
        Loc -> Loc
    end.
