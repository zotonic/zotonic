%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell <marc@worrell.nl>
%% @doc Export data in the given format, uses notifiers for fetching and encoding data.

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

-module(controller_export).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    forbidden/2,
    content_types_provided/2,
    charsets_provided/2,
    
    do_export/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    z_context:lager_md(Context),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(z_context:continue_session(Context1)),
    z_context:lager_md(Context2),
    Dispatch = z_context:get(zotonic_dispatch, Context2),
    case z_notifier:first(#export_resource_visible{dispatch=Dispatch}, Context2) of
        undefined -> ?WM_REPLY(false, Context2);
        {ok, true} -> ?WM_REPLY(false, Context2);
        {ok, false} -> ?WM_REPLY(true, Context2)
    end.

content_types_provided(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case controller_export_resource:get_content_type(undefined, Dispatch, Context) of
        {ok, ContentType} ->
            Context2 = z_context:set(content_type_mime, ContentType, Context),
            ?WM_REPLY([{ContentType, do_export}], Context2);
        {error, Reason} = Error ->
            lager:error("~p: mod_export error when fetching content type for ~p ~p",
                        [z_context:site(Context), Dispatch, Reason]),
            throw(Error)
    end.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

do_export(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    Stream = {stream, {<<>>, fun() -> controller_export_resource:do_header(Context) end}},
    Context1 = set_filename(Context),
    ?WM_REPLY(Stream, Context1).


set_filename(Context) ->
    ContentType = z_context:get(content_type_mime, Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case z_notifier:first(#export_resource_filename{
                                dispatch=Dispatch,
                                content_type=ContentType}, Context)
    of
        undefined ->
            Extension = case mimetypes:mime_to_exts(ContentType) of
                            undefined -> "bin";
                            Exts -> binary_to_list(hd(Exts))
                        end, 
            Filename = "export."++Extension,
            z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename, Context);
        {ok, Filename} ->
            Filename1 = z_convert:to_list(Filename),
            z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename1, Context)
    end.

