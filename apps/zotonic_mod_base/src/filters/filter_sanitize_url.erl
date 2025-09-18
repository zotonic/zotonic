%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc 'sanitize_url' filter, sanitize an URL. The url must be an unescaped URL.

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

-module(filter_sanitize_url).
-moduledoc("
Sanitize an URL. Removes URLs that might be dangerous, like `javascript:` URLs.

Ensure that the input to this filter is HTML unescaped. As the filter is not unescaping any HTML entities.
").

-export([
    sanitize_url/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

sanitize_url(undefined, _Context) ->
    undefined;
sanitize_url(#trans{} = Tr, Context) ->
    sanitize_url(z_trans:lookup_fallback(Tr, Context), Context);
sanitize_url(Url, _Context) ->
    z_html:sanitize_uri(z_convert:to_binary(Url)).
