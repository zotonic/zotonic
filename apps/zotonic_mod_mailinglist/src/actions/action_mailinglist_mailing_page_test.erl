%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Post a message to the test mailing list.
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

-module(action_mailinglist_mailing_page_test).
-moduledoc("
Post a message to the test mailing list, given with the `id` argument.

The page does not need to be published. The sender must be allowed to use
mod_mailinglist and view the page. The mailing is queued for immediate sending.

The `on_success` argument decides which actions are triggered after the page has been sent.

Todo

Extend documentation
").
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {mailing_page_test, Id, OnSuccess},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

event(#postback{ message = {mailing_page_test, PageId, OnSuccess} }, Context) ->
    PageId1 = m_rsc:rid(PageId, Context),
    case is_allowed(PageId1, Context) of
        true ->
            send_test_mailing(PageId1, OnSuccess, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end.

send_test_mailing(PageId, OnSuccess, Context) ->
    case m_rsc:name_to_id(mailinglist_test, Context) of
        {ok, ListId} ->
            ok = mod_mailinglist:queue_mailing(
                ListId,
                PageId,
                [{is_send_all, true}],
                Context),
            Context1 = z_render:growl(
                ?__("The test mailing has been queued for immediate sending...", Context),
                Context),
            z_render:wire(OnSuccess, Context1);
        {error, _} ->
            z_render:growl_error(
                ?__("There is no mailing list with the name ‘mailinglist_test’.", Context),
                Context)
    end.

is_allowed(PageId, Context) ->
    is_integer(PageId)
    andalso z_acl:is_allowed(use, mod_mailinglist, Context)
    andalso z_acl:rsc_visible(PageId, Context).
