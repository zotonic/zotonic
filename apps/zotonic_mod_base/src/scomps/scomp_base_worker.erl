%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Start a Cotonic web-worker

%% Copyright 2018 Marc Worrell
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

-module(scomp_base_worker).

-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    case proplists:get_value(src, Params) of
        undefined ->
            lager:info("Warning: {% worker %} without 'src' parameter."),
            {ok, <<>>};
        Src ->
            Context1 = z_context:set_language(undefined, Context),
            Base = proplists:get_value(base, Params, <<"cotonic/cotonic-worker-bundle.js">>),
            SrcUrl = z_lib_include:url([ Src ], Context1),
            BaseUrl = z_lib_include:url([ Base ], Context1),
            Name = proplists:get_value(name, Params, <<>>),
            Spawn = [
                <<"cotonic.spawn_named(\"">>,
                    z_utils:js_escape(Name), "\", \"",
                    SrcUrl, "\", \"",
                    BaseUrl, "\");"
            ],
            {ok, [
                <<"<script type='text/javascript'>">>,
                    <<"if (typeof cotonic === 'undefined') { ">>,
                        <<"window.addEventListener('cotonic-ready', function() {">>, Spawn, <<"}, false); ">>,
                    <<"} else { ">>, Spawn, <<" } ">>,
                <<"</script>">>
            ]}
    end.

