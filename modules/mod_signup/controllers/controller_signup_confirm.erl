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

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([provide_content/2]).
-export([event/2]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context1 = z_context:new_request(ReqData, DispatchArgs, ?MODULE),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    z_context:lager_md(Context2),
    Rendered = z_template:render("signup_confirm.tpl", [], Context2),
    {Output, OutputContext} = z_context:output(Rendered, Context2),
    ?WM_REPLY(Output, OutputContext).

%% @doc Handle the submit of the signup form.
event(#submit{}, Context) ->
    Key = z_context:get_q(key, Context, []),
    case confirm(Key, Context) of
        {ok, UserId} ->
            {ok, ContextUser} = z_auth:logon(UserId, Context),
            Location = confirm_location(UserId, ContextUser),
            Location1 = z_context:site_url(Location, Context),
            z_render:wire([
                    {hide, [{target, "confirm_form"}]},
                    {show, [{target, "confirm_ok"}]},
                    {redirect, [{location, Location1}]}
                ], ContextUser);
        {error, _Reason} ->
            z_render:wire([
                    {show, [{target,"confirm_error"}]},
                    {show, [{target,"confirm_key"}]}
                ], Context)
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

confirm_location(UserId, ContextUser) ->
    % Fetch ready page, stored at signup
    ReadyPage = z_notifier:first(
            #tkvstore_get{
                type = <<"signup_ready_page">>,
                key = integer_to_binary(UserId)
            },
            ContextUser),
    z_notifier:first(
        #tkvstore_delete{
            type = <<"signup_ready_page">>,
            key = integer_to_binary(UserId)
        },
        ContextUser),
    case is_empty(ReadyPage) of
        true ->
            case z_notifier:first(#signup_confirm_redirect{id=UserId}, ContextUser) of
                undefined ->
                    m_rsc:p(UserId, page_url, ContextUser);
                Loc ->
                    Loc
            end;
        false ->
            z_convert:to_list(ReadyPage)
    end.

is_empty(undefined) -> true;
is_empty("") -> true;
is_empty("#reload") -> true;
is_empty(<<>>) -> true;
is_empty(<<"#reload">>) -> true;
is_empty(_) -> false.
