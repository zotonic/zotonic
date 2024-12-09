%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2024 Marc Worrell <marc@worrell.nl>
%% @doc Export a (list of) resource(s) in the given format, uses notifiers or the
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

-module(controller_export_resource).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/1,
    previously_existed/1,
    forbidden/1,
    content_types_provided/1,

    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

resource_exists(Context) ->
    {Id, Context2} = get_id(Context),
    {m_rsc:exists(Id, Context2), Context2}.

previously_existed(Context) ->
    {Id, Context2} = get_id(Context),
    {m_rsc_gone:is_gone(Id, Context2), Context2}.

forbidden(Context) ->
    {Id, Context2} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context2),
    case z_acl:is_allowed(use, mod_export, Context2) of
        true ->
            case export_helper:call(#export_resource_visible{id=Id, dispatch=Dispatch}, Context2) of
                undefined -> {not z_acl:rsc_visible(Id, Context2), Context2};
                true -> {false, Context2};
                false -> {true, Context2}
            end;
        false ->
            {true, Context2}
    end.

content_types_provided(Context) ->
    {Id, Context1} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context1),
    case export_helper:get_content_type(Id, Dispatch, Context1) of
        {ok, ContentType} when is_binary(ContentType); is_tuple(ContentType) ->
            {[ ContentType ], Context};
        {error, no_content_type} ->
            ContentTypes = export_encoder:content_types(Context),
            {ContentTypes, Context};
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"mod_export error when fetching content type">>,
                in => zotonic_mod_export,
                dispatch => Dispatch,
                rsc_id => Id,
                result => error,
                reason => Reason
            }),
            throw(Error)
    end.

process(_Method, _AcceptedCT, ProvidedCT, Context) ->
    {Id, _} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    StreamOpts = [
        {dispatch, Dispatch},
        {is_query, z_convert:to_bool(z_context:get(is_query, Context))},
        {is_raw, z_convert:to_bool(z_context:get_q(raw, Context))}
    ],
    Stream = export_encoder:stream(Id, ProvidedCT, StreamOpts, Context),
    Context1 = export_helper:set_filename(Id, ProvidedCT, Dispatch, Context),
    {Stream, Context1}.

get_id(Context) ->
    case z_context:get(id, Context) of
        undefined ->
            case z_context:get_q(<<"id">>, Context) of
                undefined ->
                    {undefined, Context};
                <<>> ->
                    {undefined, Context};
                Id ->
                    RscId = m_rsc:rid(Id, Context),
                    {RscId, z_context:set(id, {ok, RscId}, Context)}
            end;
        {ok, Id} ->
            {Id, Context}
    end.

