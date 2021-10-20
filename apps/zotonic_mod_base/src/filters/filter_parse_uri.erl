%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Parse an uri. Interface to uri_string:parse/1

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

-module(filter_parse_uri).
-export([parse_uri/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

parse_uri(undefined, _Context) ->
    undefined;
parse_uri(Uri, _Context) when is_binary(Uri) ->
    uri_string:parse(Uri);
parse_uri(Uri, Context) when is_list(Uri) ->
    parse_uri(unicode:characters_to_binary(Uri, utf8), Context);
parse_uri(#trans{} = Tr, Context) ->
    parse_uri(z_trans:lookup_fallback(Tr, Context), Context);
parse_uri(V, Context) ->
    parse_uri(z_convert:to_binary(V), Context).

