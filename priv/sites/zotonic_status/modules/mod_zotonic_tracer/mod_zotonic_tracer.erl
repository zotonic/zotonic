%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010 Maximonster Interactive Things
%% @doc Zotonic tracer module.

%% Copyright 2010 Maximonster Interactive Things
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

-module(mod_zotonic_tracer).
-author("Atilla Erdodi <atilla@maximonster.com>").

-mod_title("Zotonic tracer module").
-mod_description("Allows setting up and inspecting traces on Zotonic core.").
-mod_prio(500).

-export([
    event/2
        ]).

-include("zotonic.hrl").


event(#postback{message={trace, _Args}}, Context) ->    
    case z_tracer:get_tracer() of 
        {ok, _TracerPid} ->             
            z_tracer:stop(),
            notice('Zotonic', "Tracing disabled...", Context); 
        {error, _Error} ->            
            z_tracer:stop(), % dbg workaround            
            z_tracer:start(),
            notice('Zotonic', "Tracing enabled...", Context)
    end.
    
notice(SiteName, Text, Context) ->
    Context1 = z_render:appear_top(
        "notices", 
        #render{template="_notice.tpl", vars=[{site,SiteName},{notice,Text}]}, 
        Context),
    z_render:wire({fade_out, [{selector, "#notices > p:gt(0)"}, {speed, 1000}]}, Context1).
