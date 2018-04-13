%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-12
%% @doc Display a form to sign up.

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

-module(controller_signup_confirm).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    charsets_provided/1,
    content_types_provided/1,
    provide_content/1
]).
-export([event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").


charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/html">>, provide_content}], Context}.

provide_content(Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:lager_md(Context2),
    Key = z_context:get_q(<<"key">>, Context2, <<>>),
    {Vars, ContextConfirm} = case Key of
                                <<>> ->
                                    {[], Context2};
                                _ ->
                                    case confirm(Key, Context2) of
                                        {ok, UserId} ->
                                            {ok, ContextUser} = z_auth:logon(UserId, Context2),
                                            Location = confirm_location(UserId, ContextUser),
                                            {[{user_id, UserId}, {location,Location}], ContextUser};
                                        {error, _Reason} ->
                                            {[{error, true}], Context2}
                                    end
                              end,
    Rendered = z_template:render("signup_confirm.tpl", Vars, ContextConfirm),
    z_context:output(Rendered, ContextConfirm).


%% @doc Handle the submit of the signup form.
event(#submit{}, Context) ->
    Key = z_context:get_q(key, Context, []),
    case confirm(Key, Context) of
        {ok, UserId} ->
            {ok, ContextUser} = z_auth:logon(UserId, Context),
            Location = confirm_location(UserId, ContextUser),
            z_render:wire({redirect, [{location, Location}]}, ContextUser);
        {error, _Reason} ->
            z_render:wire({show, [{target,"confirm_error"}]}, Context)
    end.


confirm(Key, Context) ->
    case m_identity:lookup_by_verify_key(Key, Context) of
        undefined ->
            {error, unknown_key};
        Row ->
            UserId = proplists:get_value(rsc_id, Row),
            {ok, UserId} = m_rsc:update(UserId, [{is_published, true},{is_verified_account, true}], z_acl:sudo(Context)),
            m_identity:set_verified(proplists:get_value(id, Row), Context),
            z_notifier:map(#signup_confirm{id=UserId}, Context),
            {ok, UserId}
    end.

confirm_location(UserId, Context) ->
    case z_notifier:first(#signup_confirm_redirect{id=UserId}, Context) of
        undefined -> m_rsc:p(UserId, page_url, Context);
        Loc -> Loc
    end.
