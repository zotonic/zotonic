%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Check if the given URL is a url to the current site.

%% Copyright 2021 Marc Worrell
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

-module(filter_is_site_url).
-export([is_site_url/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

is_site_url(#trans{ } = Tr, Context) ->
    is_site_url(z_trans:lookup_fallback(Tr, Context), Context);
is_site_url(Url, Context) when is_list(Url) ->
    is_site_url(unicode:characters_to_binary(Url, utf8), Context);
is_site_url(Url, Context) when is_binary(Url) ->
    z_context:is_site_url(Url, Context);
is_site_url(_, _Context) ->
    false.

