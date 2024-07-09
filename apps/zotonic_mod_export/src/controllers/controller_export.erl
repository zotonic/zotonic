%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2024 Marc Worrell <marc@worrell.nl>
%% @doc Export data in the given format, uses notifiers or the
%% export_module controller option for fetching and encoding data.
%% @end

%% Copyright 2013-2024 Marc Worrell
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

-module(controller_export).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    forbidden/1,
    content_types_provided/1,

    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

forbidden(Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:logger_md(Context2),
    Dispatch = z_context:get(zotonic_dispatch, Context2),
    case z_acl:is_allowed(use, mod_export, Context2) of
        true ->
            case export_helper:call(#export_resource_visible{dispatch=Dispatch}, Context2) of
                undefined -> {false, Context2};
                true -> {false, Context2};
                false -> {true, Context2}
            end;
        false ->
            {true, Context2}
    end.

content_types_provided(Context) ->
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case export_helper:get_content_type(undefined, Dispatch, Context) of
        {ok, ContentType} when is_binary(ContentType); is_tuple(ContentType) ->
            {[ ContentType ], Context};
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"mod_export error when fetching content type">>,
                in => zotonic_mod_export,
                dispatch => Dispatch,
                result => Error,
                reason => Reason
            }),
            throw(Error)
    end.

process(_Method, _AcceptedCT, ProvidedCT, Context) ->
    Dispatch = z_context:get(zotonic_dispatch, Context),
    StreamOpts = [
        {dispatch, Dispatch},
        {is_query, z_convert:to_bool(z_context:get(is_query, Context))},
        {is_raw, z_convert:to_bool(z_context:get_q(raw, Context))}
    ],
    Stream = export_encoder:stream(undefined, ProvidedCT, StreamOpts, Context),
    Context1 = set_filename(ProvidedCT, Dispatch, Context),
    {Stream, Context1}.

set_filename(ProvidedCT, Dispatch, Context) ->
    export_helper:set_filename(undefined, ProvidedCT, Dispatch, Context).

