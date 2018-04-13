%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2009-11-15
%% @doc Redirect to the URL of a resource of type 'website'.

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

-module(controller_website_redirect).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    z_context:lager_md(ContextQs),
    Id = m_rsc:rid(z_context:get_q(<<"id">>, ContextQs), ContextQs),
    Exists = m_rsc:exists(Id, ContextQs)
             andalso z_acl:rsc_visible(Id, ContextQs),
    {Exists, ContextQs}.

moved_temporarily(Context) ->
    Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
    case m_rsc:p(Id, website, Context) of
        undefined ->
            {false, Context};
        <<>> ->
            {false, Context};
        Url ->
            AbsUrl = iolist_to_binary(z_context:abs_url(z_html:unescape(Url), Context)),
            {{true, AbsUrl}, Context}
    end.

