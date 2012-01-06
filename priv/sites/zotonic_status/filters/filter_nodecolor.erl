%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-01-06

%% @doc Node color

%% Copyright 2011 Arjan Scherpenisse
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

-module(filter_nodecolor).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-export([nodecolor/2]).

nodecolor(1, _) -> "#edc240";
nodecolor(2, _) -> "#afd8f8";
nodecolor(3, _) -> "#cb4b4b";
nodecolor(4, _) -> "4da74d";
nodecolor(5, _) -> "#9440ed";
nodecolor(_, _) -> "red".
