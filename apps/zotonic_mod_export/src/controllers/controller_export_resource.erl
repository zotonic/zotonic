%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2017 Marc Worrell <marc@worrell.nl>
%% @doc Export a (list of) resource(s) in the given format, uses notifiers for fetching and encoding data.

%% Copyright 2013-2017 Marc Worrell
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

    process/4,

    % Exports for controller_export
    get_content_type/3,
    set_filename/4
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
            case z_notifier:first(#export_resource_visible{id=Id, dispatch=Dispatch}, Context2) of
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
    case get_content_type(Id, Dispatch, Context1) of
        {ok, ContentType} when is_binary(ContentType); is_tuple(ContentType) ->
            {[ ContentType ], Context};
        {error, no_content_type} ->
            ContentTypes = export_encoder:content_types(Context),
            {ContentTypes, Context};
        {error, Reason} = Error ->
            lager:error("mod_export error when fetching content type for ~p:~p: ~p", [Dispatch, Id, Reason]),
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
    Context1 = set_filename(Id, ProvidedCT, Dispatch, Context),
    {Stream, Context1}.

set_filename(Id, ProvidedCT, Dispatch, Context) ->
    Mime = cowmachine_util:format_content_type(ProvidedCT),
    Extension = case mimetypes:mime_to_exts(Mime) of
                    undefined -> <<"bin">>;
                    Exts -> hd(Exts)
                end,
    {ok, Disposition} = z_notifier:first(
            #export_resource_content_disposition{
                id = Id,
                dispatch = Dispatch,
                content_type = ProvidedCT
            },
            Context),
    Filename = case z_notifier:first(
            #export_resource_filename{
                id = Id,
                dispatch = Dispatch,
                content_type = ProvidedCT
            },
            Context)
    of
        undefined ->
            Cat = m_rsc:p_no_acl(Id, category, Context),
            iolist_to_binary([
                        "export-",
                        z_convert:to_binary(proplists:get_value(name, Cat)),
                        "-",
                        z_convert:to_binary(Id),
                        ".",
                        Extension
                    ]);
        {ok, FN} ->
            FN1 = z_convert:to_binary(FN),
            case filename:extension(FN1) of
                <<".", Extension/binary>> -> FN1;
                <<>> -> <<FN1/binary, $., Extension/binary>>;
                _Ext -> <<(filename:rootname(FN1))/binary, $., Extension/binary>>
            end
    end,
    z_context:set_resp_header(
        <<"content-disposition">>,
        <<(z_convert:to_binary(Disposition))/binary, "; filename=", Filename/binary>>,
        Context).


%% @doc Fetch the content type being served
get_content_type(Id, Dispatch, Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            get_content_type_observer(Id, Dispatch, Context);
        Type when is_atom(Type) ->
            get_content_type_extension(z_convert:to_binary(Type), Context);
        ContentType when is_binary(ContentType); is_list(ContentType) ->
            {ok, z_convert:to_binary(ContentType)}
    end.

get_content_type_observer(Id, Dispatch, Context) ->
    case z_notifier:first(#export_resource_content_type{id=Id, dispatch=Dispatch}, Context) of
        undefined ->
            get_content_type_extension(z_context:get_q(<<"type">>, Context), Context);
        {error, _} = Error ->
            Error;
        {ok, _} = Ok ->
            Ok
    end.

get_content_type_extension(undefined, _Context) ->
    {error, no_content_type};
get_content_type_extension(<<"bert">>, _Context) ->
    {ok, {<<"application">>, <<"x-bert">>, []}};
get_content_type_extension(<<"ubf">>, _Context) ->
    {ok, {<<"text">>, <<"x-ubf">>, []}};
get_content_type_extension(Type, Context) ->
    [Mime|_] = mimetypes:ext_to_mimes(Type),
    case Mime of
        <<"application/octet-stream">> ->
            {error, no_content_type};
        _ ->
            % Must have an exporter
            Mime1 = cowmachine_util:normalize_content_type(Mime),
            ContentTypes = export_encoder:content_types(Context),
            case lists:member(Mime1, ContentTypes) of
                true -> {ok, Mime1};
                false -> {error, no_content_type}
            end
    end.


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
