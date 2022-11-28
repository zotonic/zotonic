%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Lookup a specific translation in a trans record. None trans record values
%% are passed as-is.
%% @end

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

-module(filter_translation).

-export([
    translation/3
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

translation(_V, undefined, _Context) ->
    undefined;
translation(#trans{ tr = Tr }, Lang, _Context) ->
    case z_language:to_language_atom(Lang) of
        {ok, Iso} ->
            proplists:get_value(Iso, Tr);
        {error, _} ->
            undefined
    end;
translation(V, _Lang, _Context) ->
    V.
