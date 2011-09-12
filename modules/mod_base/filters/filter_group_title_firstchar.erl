%% @author Arjan Scherpenisse <marc@worrell.nl>
%% @copyright 2010,2011 Arjan Scherpenisse
%% @doc group a list of sorted rsc ids on their first letter of the title. Then, split this list in #Cols more-or-less even columns.

%% Copyright 2010,2011 Arjan Scherpenisse
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

-module(filter_group_title_firstchar).
-export([group_title_firstchar/3]).

-include("zotonic.hrl").

group_title_firstchar(undefined, _Cols, _Context) ->
    undefined;
group_title_firstchar(In, Cols, Context) ->
    filter_group_firstchar:group_firstchar(In, title, Cols, Context).
