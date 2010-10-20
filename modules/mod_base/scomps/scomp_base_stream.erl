%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%% @doc Start the websockets connection or comet loop, enabling push notifications from the server.

%% Copyright 2009-2010 Marc Worrell
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

-module(scomp_base_stream).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(_Params, _Vars, Context) ->
    {ok, z_script:add_script(
                ["z_stream_start('",
                 add_subdomain(z_context:streamhost(Context)),
                 "');"], 
                Context)
    }.


add_subdomain([$*|Hostname]) ->
    z_ids:id(3) ++ Hostname;
add_subdomain(<<$*,Hostname/binary>>) ->
    [z_ids:id(3), Hostname];
add_subdomain([$.|_] = Hostname) ->
    z_ids:id(3) ++ Hostname;
add_subdomain(<<$.,_/binary>> = Hostname) ->
    [z_ids:id(3), Hostname];
add_subdomain(none) ->
    "window.location.host";
add_subdomain(Hostname) ->
    Hostname.
