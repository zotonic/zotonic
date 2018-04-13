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
    forbidden/1,
    content_types_provided/1,
    charsets_provided/1,

    do_export/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

forbidden(Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:lager_md(Context2),
    Dispatch = z_context:get(zotonic_dispatch, Context2),
    case z_acl:is_allowed(use, mod_export, Context2) of
        true ->
            case z_notifier:first(#export_resource_visible{dispatch=Dispatch}, Context2) of
                undefined -> {false, Context2};
                true -> {false, Context2};
                false -> {true, Context2}
            end;
        false ->
            {true, Context2}
    end.

content_types_provided(Context) ->
    Dispatch = z_context:get(zotonic_dispatch, Context),
    case controller_export_resource:get_content_type(undefined, Dispatch, Context) of
        {ok, ContentType} ->
            {[{ContentType, do_export}], Context};
        {error, Reason} = Error ->
            lager:error("mod_export error when fetching content type for ~p ~p", [Dispatch, Reason]),
            throw(Error)
    end.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.


do_export(Context) ->
    ContentType = cowmachine_req:resp_content_type(Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    StreamOpts = [
        {dispatch, Dispatch},
        {is_query, z_convert:to_bool(z_context:get(is_query, Context))},
        {is_raw, z_convert:to_bool(z_context:get_q(raw, Context))}
    ],
    Stream = export_encoder:stream(undefined, ContentType, StreamOpts, Context),
    Context1 = set_filename(ContentType, Dispatch, Context),
    {Stream, Context1}.

set_filename(ContentType, Dispatch, Context) ->
    controller_export_resource:set_filename(undefined, ContentType, Dispatch, Context).

