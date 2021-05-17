%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc 'sanitize_html' filter, sanitize HTML.

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

-module(filter_sanitize_html).

-export([
    sanitize_html/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

sanitize_html(undefined, _Context) ->
    undefined;
sanitize_html(#trans{} = Tr, Context) ->
    sanitize_html(z_trans:lookup_fallback(Tr, Context), Context);
sanitize_html(Url, _Context) ->
    z_html:sanitize(z_convert:to_binary(Url)).
