%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell <marc@worrell.nl>
%% @doc Export a resource in the given format, uses notifiers for fetching and encoding data.

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
    do_header/1
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
    case z_notifier:first(#export_resource_visible{id=Id, dispatch=Dispatch}, Context2) of
        undefined -> ?WM_REPLY(not z_acl:rsc_visible(Id, Context2), Context2);
        true -> ?WM_REPLY(false, Context2);
        false -> ?WM_REPLY(true, Context2)
    end.

content_types_provided(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    {Id, Context1} = get_id(z_context:ensure_qs(z_context:continue_session(Context))),
    Dispatch = z_context:get(zotonic_dispatch, Context1),
    case get_content_type(Id, Dispatch, Context1) of
        {ok, ContentType} ->
            Context2 = z_context:set(content_type_mime, ContentType, Context1),
            ?WM_REPLY([{ContentType, do_export}], Context2);
        {error, Reason} = Error ->
            lager:error("~p: mod_export error when fetching content type for ~p:~p: ~p",
                        [z_context:site(Context1), Dispatch, Id, Reason]),
            throw(Error)
    end.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

do_export(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    Stream = {stream, {<<>>, fun() -> do_header(Context) end}},
    Context1 = set_filename(Context),
    ?WM_REPLY(Stream, Context1).

do_header(Context) ->
    ContentType = z_context:get(content_type_mime, Context),
    {Id, _} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case z_notifier:first(#export_resource_header{id=Id, content_type=ContentType, dispatch=Dispatch}, Context) of
        undefined -> 
            {<<>>, fun() -> do_body(undefined, Context) end};
        {ok, Header, State} -> 
            {flatten_header(Header, ContentType, Context), fun() -> do_body(State, Context) end};
        {ok, Header} ->
            {flatten_header(Header, ContentType, Context), fun() -> do_body(undefined, Context) end}
    end.

flatten_header(Header, _ContentType, _Context) when is_binary(Header) ->
    Header;
flatten_header(Header, ContentType, Context) when is_list(Header) ->
    case ContentType of
        "text/csv" -> export_encode_csv:encode(Header, Context);
        _ -> iolist_to_binary(Header)
    end;
flatten_header(Header, _ContentType, _Context) ->
    z_convert:to_binary(Header).


do_body(State, Context) ->
    ContentType = z_context:get(content_type_mime, Context),
    {Id, _} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case z_notifier:first(#export_resource_data{id=Id, content_type=ContentType, dispatch=Dispatch, state=State}, Context) of
        undefined -> do_body_data([Id], State, Context);
        {ok, List} -> do_body_data(List, State, Context);
        {ok, List, NewState} -> do_body_data(List, NewState, Context)
    end.

do_body_data([], State, Context) ->
    do_footer(State, Context);
do_body_data(List, State, Context) ->
    {Data, NewState} = lists:foldl(
                                fun(D, {Acc, AccState}) ->
                                    {DEnc, AccState1} = do_body_encode(D, AccState, Context),
                                    {[Acc, DEnc], AccState1}
                                end,
                                {[], State},
                                List),
    DataBin = iolist_to_binary(Data), 
    case State of
        undefined -> {DataBin, fun() -> do_footer(undefined, Context) end};
        _ -> {DataBin, fun() -> do_body(NewState, Context) end}
    end.

do_body_encode(Item, State, Context) ->
    ContentType = z_context:get(content_type_mime, Context),
    {Id, _} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case z_notifier:first(#export_resource_encode{
                                id=Id,
                                dispatch=Dispatch,
                                content_type=ContentType,
                                data=Item,
                                state=State}, Context)
    of
        undefined -> {<<>>, State};
        {ok, Enc} -> {Enc, State};
        {ok, Enc, NewState} -> {Enc, NewState}
    end.

do_footer(State, Context) ->
    ContentType = z_context:get(content_type_mime, Context),
    {Id, _} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case z_notifier:first(#export_resource_footer{
                                id=Id,
                                dispatch=Dispatch,
                                content_type=ContentType,
                                state=State}, Context)
    of
        undefined -> {<<>>, done};
        {ok, Enc} -> {Enc, done}
    end.


set_filename(Context) ->
    ContentType = z_context:get(content_type_mime, Context),
    {Id, _} = get_id(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case z_notifier:first(#export_resource_filename{
                                id=Id,
                                dispatch=Dispatch,
                                content_type=ContentType}, Context)
    of
        undefined ->
            Cat = m_rsc:p_no_acl(Id, category, Context),
            Extension = case mimetypes:mime_to_exts(ContentType) of
                            undefined -> "bin";
                            Exts -> binary_to_list(hd(Exts))
                        end, 
            Filename = "export-"
                        ++z_convert:to_list(proplists:get_value(name, Cat))
                        ++"-"
                        ++z_convert:to_list(Id)
                        ++"."
                        ++Extension,
            z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename, Context);
        {ok, Filename} ->
            Filename1 = z_convert:to_list(Filename),
            z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename1, Context)
    end.

%% @doc Fetch the content type being served
get_content_type(Id, Dispatch, Context) ->
    case z_context:get(content_type, Context) of
        csv ->
            {ok, "text/csv"};
        ContentType when is_list(ContentType) -> 
            {ok, ContentType};
        undefined -> 
            case z_notifier:first(#export_resource_content_type{id=Id, dispatch=Dispatch}, Context) of
                undefined -> {error, no_content_type};
                Other -> Other
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
