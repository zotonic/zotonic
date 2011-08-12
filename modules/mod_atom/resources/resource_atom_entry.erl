%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-02
%% @doc Atom entry resource.

%% Copyright 2009 Arjan Scherpenisse
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

-module(resource_atom_entry).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    init/1,
    service_available/2,
    is_authorized/2,
    allowed_methods/2,
    encodings_provided/2,
	resource_exists/2,
	last_modified/2,
	expires/2,
	content_types_provided/2,
	charsets_provided/2,
	provide_content/2
]).


-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

%% Let cached versions expire in an hour.
-define(MAX_AGE, 3600).


init(DispatchArgs) ->
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_qs(Context1),
    ?WM_REPLY(true, Context2).


allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.


charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.


encodings_provided(ReqData, Context) ->
    {[
        {"identity", fun(X) -> X end}, 
        {"gzip", fun(X) -> zlib:gzip(X) end}
    ], ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"application/atom+xml;type=entry", provide_content},
      {"application/atom+xml", provide_content}],
     ReqData, Context}.


%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    try
        ?WM_REPLY(m_rsc:exists(get_id(ContextQs), ContextQs), ContextQs)
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    z_acl:wm_is_authorized(view, get_id(ContextQs), ContextQs).


last_modified(ReqData, Context) ->
    RD1 = wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData),
    Modified = m_rsc:p(get_id(Context), modified, Context),
    Context1 = z_context:set(last_modified, Modified, Context),
    {Modified, RD1, Context1}.


expires(ReqData, State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), ReqData, State}.


provide_content(ReqData, Context) ->
    Context1 = z_context:set_reqdata(ReqData, Context),
    Id = get_id(Context1),
    RscExport = m_rsc_export:full(Id, Context1),
    Content = atom_convert:resource_to_atom(RscExport),
    ?WM_REPLY(Content, Context1).


%% @doc Fetch the id from the request or the dispatch configuration.
%% @spec get_id(Context) -> int() | false
get_id(Context) ->
    ReqId = case z_context:get(id, Context) of
        undefined -> z_context:get_q("id", Context);
        ConfId -> ConfId
    end,
    case m_rsc:name_to_id(ReqId, Context) of
        {ok, RscId} -> RscId;
        _ -> false
    end.
