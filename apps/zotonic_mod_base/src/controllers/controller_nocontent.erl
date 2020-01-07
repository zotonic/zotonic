%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2020 Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @doc A controller which always returns 204 no-content.

%% Copyright 2019-2020 Maas-Maarten Zeeman
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

-module(controller_nocontent).

-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    allowed_methods/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[ <<"GET">>, <<"PUT">>, <<"POST">>, <<"HEAD">>, <<"DELETE">> ], Context}.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    {{halt, 204}, Context}.
