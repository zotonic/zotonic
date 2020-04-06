%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Toggle database trace on/off

%% Copyright 2020 Marc Worrell
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

-module(z_development_dbtrace).

-export([
    event/2,

    set_tracing/2,
    copy_from_session/1,
    is_tracing/1
    ]).

-include("zotonic.hrl").

event(#postback{ message=dbtrace_toggle }, Context) ->
    IsTracing = is_tracing(Context),
    set_tracing(not IsTracing, Context),
    Context.

set_tracing(IsTracing, Context) ->
    z_context:set_session(is_dbtrace, IsTracing, Context),
    erlang:put(is_dbtrace, IsTracing).

copy_from_session(Context) ->
    Tracing = z_convert:to_bool( z_context:get_session( is_dbtrace, Context ) ),
    erlang:put(is_dbtrace, Tracing).

is_tracing(Context) ->
    case erlang:get(is_dbtrace) of
        true ->
            true;
        _ ->
            Tracing = z_convert:to_bool( z_context:get_session( is_dbtrace, Context ) ),
            erlang:put(is_dbtrace, Tracing),
            Tracing
    end.
