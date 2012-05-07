%% @author Hans-Christian Esperer <hc-git@hcesperer.org>
%% @copyright 2012 Hans-Christian ESperer
%% Date: 2012-05-06
%% @doc Integrate an audio player (currently jPlayer) into zotonic

%% Copyright 2012 hc
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

-module(mod_audioplayer).
-author("Hans-Christian Esperer <hc-git@hcesperer.org>").

-mod_title("Audio Player").
-mod_description("Integrate jPlayer into zotonic").
-mod_depends([admin, base]).
-mod_provides([audioplayer]).

-include_lib("zotonic.hrl").

