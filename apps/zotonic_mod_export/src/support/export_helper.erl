%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell <marc@worrell.nl>
%% @doc Helper functions for exports.
%% @end

%% Copyright 2024 Marc Worrell
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

-module(export_helper).

-export([
    call/2,
    get_content_type/3,
    set_filename/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Call the export observer or module.
-spec call(Notification, Context) -> Result | undefined when
    Notification :: #export_resource_visible{}
                  | #export_resource_filename{}
                  | #export_resource_content_disposition{}
                  | #export_resource_content_type{}
                  | #export_resource_header{}
                  | #export_resource_data{}
                  | #export_resource_encode{}
                  | #export_resource_footer{},
    Context :: z:context(),
    Result :: term().
call(Notification, Context) ->
    case z_context:get(export_module, Context) of
        undefined ->
            z_notifier:first(Notification, Context);
        Module ->
            code:ensure_loaded(Module),
            Function = erlang:element(1, Notification),
            case erlang:function_exported(Module, Function, 2) of
                true ->
                    Module:Function(Notification, Context);
                false ->
                    undefined
            end
    end.

%% @doc Determine the content type being served.
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
    case call(#export_resource_content_type{id=Id, dispatch=Dispatch}, Context) of
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

%% @doc Determine the filename for the export and set the content-disposition header.
set_filename(Id, ProvidedCT, Dispatch, Context) ->
    Mime = cowmachine_util:format_content_type(ProvidedCT),
    Extension = maybe_add_dot(z_media_identify:extension(Mime)),
    Disposition = case call(
            #export_resource_content_disposition{
                id = Id,
                dispatch = Dispatch,
                content_type = Mime
            }, Context)
    of
        {ok, Disp} -> Disp;
        undefined -> <<"attachment">>
    end,
    Filename = case call(
            #export_resource_filename{
                id = Id,
                dispatch = Dispatch,
                content_type = Mime
            },
            Context)
    of
        undefined ->
            CatId = m_rsc:p_no_acl(Id, category_id, Context),
            iolist_to_binary([
                        "export-",
                        z_convert:to_binary(m_rsc:p_no_acl(CatId, name, Context)),
                        "-",
                        z_convert:to_binary(Id),
                        Extension
                    ]);
        {ok, FN} ->
            FN1 = z_convert:to_binary(FN),
            case filename:extension(FN1) of
                FE when FE =:= Extension -> FN1;
                <<>> -> <<FN1/binary, Extension/binary>>;
                _Ext -> <<(filename:rootname(FN1))/binary, Extension/binary>>
            end
    end,
    z_context:set_resp_header(
        <<"content-disposition">>,
        <<(z_convert:to_binary(Disposition))/binary, "; filename=", Filename/binary>>,
        Context).

maybe_add_dot(<<".", _/binary>> = Ext) -> Ext;
maybe_add_dot(Ext) -> <<".", Ext/binary>>.
