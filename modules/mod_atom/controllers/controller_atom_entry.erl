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

-module(controller_atom_entry).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    is_authorized/1,
    allowed_methods/1,
    content_encodings_provided/1,
	resource_exists/1,
	last_modified/1,
	expires/1,
	content_types_provided/1,
	charsets_provided/1,
	provide_content/1
]).


-include_lib("include/zotonic.hrl").

%% Let cached versions expire in an hour.
-define(MAX_AGE, 3600).


allowed_methods(Context) ->
    {[<<"HEAD">>, <<"GET">>], Context}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_encodings_provided(Context) ->
    {[<<"identity">>, <<"gzip">>], Context}.

content_types_provided(Context) ->
    {[{<<"application/atom+xml;type=entry">>, provide_content},
      {<<"application/atom+xml">>, provide_content}],
     Context}.

%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(Context) ->
    try
        {m_rsc:exists(get_id(Context), Context), Context}
    catch
        _:_ -> {false, Context}
    end.

%% @doc Check if the current user is allowed to view the resource. 
is_authorized(Context) ->
    z_acl:wm_is_authorized(view, get_id(Context), Context).

last_modified(Context) ->
    MaxAge = integer_to_binary(?MAX_AGE),
    Context1 = z_context:set_resp_header(<<"cache-control">>, <<"public, max-age=", MaxAge/binary>>, Context),
    Modified = m_rsc:p(get_id(Context), modified, Context1),
    Context2 = z_context:set(last_modified, Modified, Context1),
    {Modified, Context2}.

expires(Context) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), Context}.

provide_content(Context) ->
    Id = get_id(Context),
    RscExport = m_rsc_export:full(Id, Context),
    Content = atom_convert:resource_to_atom(RscExport, Context),
    Content1 = cowmachine_req:encode_content(Content, Context),
    {Content1, Context}.


%% @doc Fetch the id from the request or the dispatch configuration.
-spec get_id(#context{}) -> integer() | false.
get_id(Context) ->
    ReqId = case z_context:get(id, Context) of
        undefined -> z_context:get_q(<<"id">>, Context);
        ConfId -> ConfId
    end,
    case m_rsc:name_to_id(ReqId, Context) of
        {ok, RscId} -> RscId;
        _ -> false
    end.
