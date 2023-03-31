%% @author    Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc 'urldecode' filter, decode the %-encoded characters in a string.

%% Copyright 2022 Marc Worrell
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

-module(filter_urldecode).
-export([urldecode/2]).

-include("zotonic.hrl").

urldecode(undefined, _Context) ->
    undefined;
urldecode({trans, _} = Tr, Context) ->
    urldecode(z_trans:lookup_fallback(Tr, Context), Context);
urldecode(Input, _Context) when is_binary(Input) ->
    list_to_binary(z_url:url_decode(binary_to_list(Input)));
urldecode(Input, _Context) when is_list(Input) ->
    z_url:url_decode(iolist_to_binary(Input));
urldecode(_Input, _Context) ->
    <<>>.
