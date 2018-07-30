%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2018 Marc Worrell
%%
%% @doc Middleware for cowmachine, extra Context based initializations.
%% This starts the https request processing after the site and dispatch rule
%% have been selected by the z_sites_dispatcher middleware.

%% Copyright 2016-2018 Marc Worrell
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

-module(z_cowmachine_middleware).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(cowboy_middleware).

-export([
    execute/2
]).

%% @doc Call cowmachine to handle the request with the given controller. Prepare the
%%      metadata for lager and set the relevant Context arguments.
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, #{ controller := Controller, controller_options := ControllerOpts } = Env) ->
    Context1 = z_context:set(ControllerOpts, maps:get(context, Env)),
    Context2 = z_context:set_controller_module(Controller, Context1),
    Context3 = z_context:set_reqdata(Req, Context2),
    Options = #{
        on_welformed => fun(Ctx) -> z_context:lager_md(Ctx), z_context:ensure_qs(Ctx) end
    },
    cowmachine:request(Controller, Context3, Env, Options).

