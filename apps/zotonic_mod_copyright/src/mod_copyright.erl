%% @copyright 2024 Driebit BV
%% @doc Add a panel to the admin to manage the copyright and
%% attribution of resources.
%% @end

%% Copyright 2024 Driebit BV
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

-module(mod_copyright).

-mod_title("Copyright").
-mod_description("Add copyrights and attribution to resources").
-mod_prio(500).

-export([
]).

-include_lib("zotonic_core/include/zotonic.hrl").
