%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse

%% Copyright 2010 Arjan Scherpenisse
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

-module(scomp_base_loremipsum).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

-define(LOREM_IPSUM, 
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam lobortis, lacus id molestie suscipit, "
    "enim leo pharetra velit, ac cursus felis mauris at velit. Cras ultrices, massa pharetra faucibus cursus, "
    "neque leo bibendum mauris, eu vulputate leo magna vitae lacus. Etiam pharetra gravida elementum. Maecenas "
    "lacinia, sem sed eleifend euismod, risus velit interdum nulla, convallis sagittis orci dui a metus. Aliquam "
    "tristique orci a ipsum dapibus viverra. Donec vestibulum varius ante, vitae placerat magna ultrices ac. "
    "Donec nec ante magna. Donec porttitor, arcu a condimentum aliquam, lectus nunc rhoncus quam, id ornare neque "
    "ligula quis dui. Pellentesque sit amet lectus augue, ut ullamcorper nisi. Donec et rutrum sem. In porta "
    "ultricies nibh, in sagittis lacus euismod at. Etiam consectetur tristique tellus, quis tristique dui vulputate "
    "vel. Curabitur sagittis gravida dui, vel sagittis lectus suscipit ac.").

vary(_Params, _Context) -> nocache.
render(Params, _Vars, _Context) ->
    case proplists:get_value(words, Params) of
        undefined -> {ok, ?LOREM_IPSUM};
        Words -> {ok, z_string:truncatewords(?LOREM_IPSUM, z_convert:to_integer(Words), ".")}
    end.

