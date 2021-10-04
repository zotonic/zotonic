%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2021 Marc Worrell
%% @doc Redirects to the correct location after an user authenticated.

%% Copyright 2020-2021 Marc Worrell
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

-module(controller_logon_done).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

%% @doc Force a redirect by stating that the resource moved to a new location.
-spec resource_exists( z:context() ) -> {boolean(), z:context()}.
resource_exists(Context) ->
    {false, Context}.

-spec previously_existed( z:context() ) -> {boolean(), z:context()}.
previously_existed(Context) ->
    {true, Context}.

%% @doc Do not let the browser remember this redirect; use a temporary redirect.
-spec moved_temporarily( z:context() ) -> {{true, binary()}, z:context()}.
moved_temporarily(Context) ->
    Page = get_ready_page(Context),
    Url = z_context:abs_url(cleanup_url(Page), Context),
    {{true, Url}, Context}.


%% @doc User logged on, fetch the location of the next page to show
-spec get_ready_page( z:context() ) -> binary().
get_ready_page(Context) ->
    get_ready_page(z_context:get_q(<<"p">>, Context, <<>>), Context).

get_ready_page(undefined, Context) ->
    get_ready_page(<<>>, Context);
get_ready_page(Page, Context) when is_binary(Page) ->
    Page1 = case z_context:is_site_url(Page, Context) of
        true -> Page;
        false -> <<>>
    end,
    case z_notifier:first(#logon_ready_page{request_page=Page1}, Context) of
        undefined -> Page1;
        Url when is_binary(Url) -> Url;
        Url when is_list(Url) -> unicode:characters_to_binary(Url, utf8)
    end.

cleanup_url(<<>>) -> <<"/">>;
cleanup_url(Url) -> z_html:noscript(Url).

