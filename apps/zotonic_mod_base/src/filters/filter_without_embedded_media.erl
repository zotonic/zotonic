%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2017 Arjan Scherpenisse
%% @doc 'without_embedded_media' filter, remove media ids embedded in texts.

%% Copyright 2010-2017 Arjan Scherpenisse
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

-module(filter_without_embedded_media).
-export([
    without_embedded_media/3,
    without_embedded_media/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


without_embedded_media(Input, Id, Context) ->
    without_embedded_media(Input, Id, true, Context).

without_embedded_media(undefined, _Id, _IsCheckBlocks, _Context) ->
    undefined;
without_embedded_media(Input, undefined, _IsCheckBlocks, Context) ->
    z_template_compiler_runtime:to_list(Input, Context);
without_embedded_media(Input, Id, IsCheckBlocks, Context) ->
    case z_template_compiler_runtime:to_list(Input, Context) of
        [] -> [];
        Ids ->
            Ids -- filter_embedded_media:embedded_media(Id, IsCheckBlocks, Context)
    end.
