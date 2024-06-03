%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc Return the list of languages in a #trans{} record.
%% @end

%% Copyright 2024 Marc Worrell
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

-module(filter_trans_languages).

-export([ trans_languages/2 ]).

-include_lib("zotonic_core/include/zotonic.hrl").

trans_languages(#trans{ tr = Tr }, _Context) ->
    [ Iso || {Iso, _} <- Tr ];
trans_languages(_, _Context) ->
    [].
