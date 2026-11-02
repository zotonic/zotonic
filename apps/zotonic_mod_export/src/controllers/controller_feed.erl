%% @copyright 2026 Marc Worrell
%% @doc Shorthand feeds for resources, saved queries and request-defined searches.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(controller_feed).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "controller",
        "export_and_syndication", "export", "url"
    ]
}).
-moduledoc("
Serve `/feed`, `/feed/:type` and `/feed/:type/:id` with optional search arguments.
Types are atom, rss, jsonfeed, json (an alias for JSON Feed), and ics/ical.
Calendar feeds default to published, unfinished events ordered by start date.

An id selects a visible saved query, a collection's members, or one resource.
The bare `/feed` uses `query_feed`, falling back to articles, and accepts only
pagination. On typed routes without an id, request arguments define a normal
mod_search query. Without search terms or an id, Atom, RSS and JSON Feed routes
use the same default selection as `/feed`. Calendar routes instead select
`cat=event`, `is_published=true` and `unfinished=true`, sorted by `date_start`
ascending with ascending `id` as the tie-breaker. They do not use `query_feed`.
Pagination alone does not change the default selection. Both
bare search keys and q-prefixed keys are supported. Page length defaults to 20
and is capped at 100. Client search options are not passed to the search engine.
Explicit sorting and saved-query sorting are retained; normal search ACLs apply.

For example, `/feed/ics?page=2` returns the second page of upcoming or ongoing
published events. Link to it from a template with `{% url feed type='ics' %}`.
Calendar responses use `text/calendar` and the templates `ical/feed_start.tpl`
and `ical/entry.tpl`. Their resource links allow `mod_import_feed` to use native
resource import and optional resource WebSub subscriptions.

A selected resource must exist and be visible to the visitor. Responses are
served inline and support ETag and Last-Modified conditional requests on the
bounded page. See `mod_export` for bulk exports and format-specific behavior.
").

-export([
    malformed_request/1,
    resource_exists/1,
    forbidden/1,
    content_types_provided/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

malformed_request(Context) ->
    Context1 = z_context:ensure_qs(Context),
    Type = request_arg(type, Context1),
    case content_type(Type) of
        undefined ->
            {true, Context1};
        Mime ->
            try
                {Id, HasId, Query, IsDefault} = feed_request(Context1),
                {false, z_context:set([
                        {feed_id, Id},
                        {is_default_feed, IsDefault},
                        {feed_has_id, HasId},
                        {feed_query, Query},
                        {feed_content_type, Mime}
                    ], Context1)}
            catch
                error:badarg -> {true, Context1};
                throw:{error, _} -> {true, Context1}
            end
    end.

resource_exists(Context) ->
    Exists = case z_context:get(feed_has_id, Context) of
        true -> m_rsc:exists(z_context:get(feed_id, Context), Context);
        _ -> true
    end,
    {Exists, Context}.

forbidden(Context) ->
    Forbidden = case z_context:get(feed_id, Context) of
        undefined -> false;
        Id -> m_rsc:exists(Id, Context) andalso not m_rsc:is_visible(Id, Context)
    end,
    {Forbidden, Context}.

content_types_provided(Context) ->
    {[z_context:get(feed_content_type, Context)], Context}.

process(_Method, _AcceptedCT, ProvidedCT, Context) ->
    Id = z_context:get(feed_id, Context),
    Context1 = z_context:set(export_module, export_feed_query, Context),
    export_feed_http:response(Id, ProvidedCT, Context1).

feed_request(Context) ->
    Args = z_context:get_q_all_noz(Context),
    case z_context:get(is_default_feed, Context, false) of
        true ->
            % The default feed's selection is fixed; only pagination is accepted.
            Paging = [{K, V} || {K, V} <- Args,
                lists:member(K, [<<"page">>, <<"qpage">>, <<"pagelen">>, <<"qpagelen">>])],
            default_request(export_feed_query:request_query(Paging), Context);
        false ->
            RawId = request_arg(id, Context),
            HasId = RawId =/= undefined andalso RawId =/= <<>>,
            Query = export_feed_query:request_query(Args),
            case {HasId, maps:get(<<"q">>, Query)} of
                {false, []} -> default_request(Query, Context);
                _ -> {m_rsc:rid(RawId, Context), HasId, Query, false}
            end
    end.

default_request(Query, Context) ->
    Id = case request_arg(type, Context) of
        <<"ics">> -> undefined;
        <<"ical">> -> undefined;
        _ -> export_feed_query:default_id(Context)
    end,
    {Id, Id =/= undefined, Query, true}.

request_arg(Key, Context) ->
    z_context:get(Key, Context, z_context:get_q(atom_to_binary(Key), Context)).

content_type(<<"ics">>) -> {<<"text">>, <<"calendar">>, []};
content_type(<<"ical">>) -> {<<"text">>, <<"calendar">>, []};
content_type(<<"atom">>) -> {<<"application">>, <<"atom+xml">>, []};
content_type(<<"rss">>) -> {<<"application">>, <<"rss+xml">>, []};
content_type(<<"jsonfeed">>) -> {<<"application">>, <<"feed+json">>, []};
content_type(<<"json">>) -> {<<"application">>, <<"feed+json">>, []};
content_type(_) -> undefined.
