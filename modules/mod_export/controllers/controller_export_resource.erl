%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell <marc@worrell.nl>
%% @doc Export a (list of) resource(s) in the given format, uses notifiers for fetching and encoding data.

%% Copyright 2013 Marc Worrell
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
    init/1,
    service_available/2,
    resource_exists/2,
    previously_existed/2,
    forbidden/2,
    content_types_provided/2,
    charsets_provided/2,

    do_export/2,

    % Exports for controller_export
    get_content_type/3,
    set_filename/4
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    z_context:lager_md(Context),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    {Id, ContextQs} = get_id(z_context:ensure_qs(z_context:continue_session(Context1))),
    z_context:lager_md(ContextQs),
    ?WM_REPLY(m_rsc:exists(Id, ContextQs), ContextQs).

previously_existed(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    {Id, Context2} = get_id(Context1),
    IsGone = m_rsc_gone:is_gone(Id, Context2),
    ?WM_REPLY(IsGone, Context2).

forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    {Id, Context2} = get_id(z_context:ensure_qs(z_context:continue_session(Context1))),
    Dispatch = z_context:get(zotonic_dispatch, Context2),
    case z_acl:is_allowed(use, mod_export, Context2) of
        true ->
            case z_notifier:first(#export_resource_visible{id=Id, dispatch=Dispatch}, Context2) of
                undefined -> ?WM_REPLY(not z_acl:rsc_visible(Id, Context2), Context2);
                true -> ?WM_REPLY(false, Context2);
                false -> ?WM_REPLY(true, Context2)
            end;
        false ->
            ?WM_REPLY(true, Context2)
    end.

content_types_provided(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    {Id, Context1} = get_id(z_context:ensure_qs(z_context:continue_session(Context))),
    Dispatch = z_context:get(zotonic_dispatch, Context1),
    case get_content_type(Id, Dispatch, Context1) of
        {ok, ContentType} ->
            ?WM_REPLY([{ContentType, do_export}], Context);
        {error, no_content_type} ->
            ContentTypes = export_encoder:content_types(Context),
            Accepted = [ {Mime, do_export} || Mime <- ContentTypes ],
            ?WM_REPLY(Accepted, Context);
        {error, Reason} = Error ->
            lager:error("~p: mod_export error when fetching content type for ~p:~p: ~p",
                        [z_context:site(Context1), Dispatch, Id, Reason]),
            throw(Error)
    end.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

do_export(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    {Id, _} = get_id(Context),
    ContentType = wrq:resp_content_type(ReqData),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    StreamOpts = [
        {dispatch, Dispatch},
        {is_query, z_convert:to_bool(z_context:get(is_query, Context))},
        {is_raw, z_convert:to_bool(z_context:get_q(raw, Context))}
    ],
    Stream = export_encoder:stream(Id, ContentType, StreamOpts, Context),
    Context1 = set_filename(Id, ContentType, Dispatch, Context),
    ?WM_REPLY(Stream, Context1).

set_filename(Id, ContentType, Dispatch, Context) ->
    Extension = case mimetypes:mime_to_exts(ContentType) of
                    undefined -> "bin";
                    Exts -> binary_to_list(hd(Exts))
                end,
    case z_notifier:first(#export_resource_filename{
                                id=Id,
                                dispatch=Dispatch,
                                content_type=ContentType}, Context)
    of
        undefined ->
            Cat = m_rsc:p_no_acl(Id, category, Context),
            Filename = "export-"
                        ++z_convert:to_list(proplists:get_value(name, Cat))
                        ++"-"
                        ++z_convert:to_list(Id)
                        ++"."
                        ++Extension,
            z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename, Context);
        {ok, Filename} ->
            Filename1 = z_convert:to_list(Filename),
            Filename2 = case filename:extension(Filename1) of
                            "." ++ Extension -> Filename1;
                            "" -> Filename1 ++ [$.|Extension];
                            _Ext -> filename:rootname(Filename1) ++ [$.|Extension]
                        end,
            z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename2, Context)
    end.

%% @doc Fetch the content type being served
get_content_type(Id, Dispatch, Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            get_content_type_observer(Id, Dispatch, Context);
        Type when is_atom(Type) ->
            get_content_type_extension(z_convert:to_list(Type), Id, Dispatch, Context);
        ContentType when is_binary(ContentType) ->
            {ok, z_convert:to_list(ContentType)};
        ContentType when is_list(ContentType) ->
            {ok, ContentType}
    end.

get_content_type_observer(Id, Dispatch, Context) ->
    case z_notifier:first(#export_resource_content_type{id=Id, dispatch=Dispatch}, Context) of
        undefined ->
            get_content_type_extension(z_context:get_q("type", Context), Id, Dispatch, Context);
        {error, _} = Error ->
            Error;
        {ok, _} = Ok ->
            Ok
    end.

get_content_type_extension(undefined, _Id, _Dispatch, _Context) ->
    {error, no_content_type};
get_content_type_extension(Type, _Id, _Dispatch, Context) ->
    [Mime|_] = mimetypes:ext_to_mimes(Type),
    case Mime of
        <<"application/octet-stream">> ->
            {error, no_content_type};
        _ ->
            % Must have an exporter
            ContentTypes = export_encoder:content_types(Context),
            MimeS = z_convert:to_list(Mime),
            case lists:member(MimeS, ContentTypes) of
                true -> {ok, MimeS};
                false -> {error, no_content_type}
            end
    end.


get_id(Context) ->
    case z_context:get(id, Context) of
        undefined ->
            case z_context:get_q("id", Context) of
                undefined ->
                    {undefined, Context};
                [] ->
                    {undefined, Context};
                Id ->
                    RscId = m_rsc:rid(Id, Context),
                    {RscId, z_context:set(id, {ok, RscId}, Context)}
            end;
        {ok, Id} ->
            {Id, Context}
    end.
