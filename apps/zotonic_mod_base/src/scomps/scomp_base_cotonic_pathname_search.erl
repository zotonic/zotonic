%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Url encode the path_info arguments set by the dispatcher.

%% Copyright 2019 Marc Worrell
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

-module(scomp_base_cotonic_pathname_search).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

vary(_Params, _Context) -> default.

render(_Params, _Vars, Context) ->
    case cowmachine_req:path_info(Context) of
        undefined ->
            {ok, <<>>};
        PathInfo ->
            PathInfo1 = lists:map(
                fun
                    ({K, true}) -> {z_convert:to_binary(K), <<>>};
                    ({K, V}) -> {z_convert:to_binary(K), z_convert:to_binary(V)}
                end,
                PathInfo),
            {ok, cow_qs:qs(PathInfo1)}
    end.

