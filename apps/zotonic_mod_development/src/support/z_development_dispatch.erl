%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2021 Marc Worrell
%% @doc Match &amp; explain request dispatching.
%% @end

%% Copyright 2014-2021 Marc Worrell
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

-module(z_development_dispatch).

-export([
    event/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#submit{message=explain_dispatch}, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            ReqPath = ensure_abs(z_string:trim(z_context:get_q(<<"explain_req">>, Context, <<>>))),
            Protocol = to_protocol(z_context:get_q(<<"explain_protocol">>, Context)),
            case z_sites_dispatcher:dispatch_trace(Protocol, ReqPath, Context) of
                {ok, Trace} ->
                    Vars = [
                        {trace, Trace},
                        {path, ReqPath},
                        {protocol, Protocol}
                    ],
                    Context1 = z_render:update(
                                    "explain-dispatch-output",
                                    #render{template="_development_dispatch_trace.tpl", vars=Vars},
                                    Context),
                    z_render:wire({fade_in, [{target, "explain-dispatch-output"}]}, Context1);
                {error, _} ->
                    z_render:growl(?__("Could not fetch tracer output, please try again.", Context), Context)
            end;
        false ->
            z_render:growl(?__("You are not allowed to use the dispatch debugging.", Context), Context)
    end.

ensure_abs(<<>>) -> <<"/">>;
ensure_abs(<<$/, _/binary>> = P) -> P;
ensure_abs(P) -> <<$/, P/binary>>.

to_protocol(<<"http">>) -> http;
to_protocol(<<"https">>) -> https.
