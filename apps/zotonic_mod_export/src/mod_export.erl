%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2026 Marc Worrell
%% @doc Generic export routines for data sources
%% @end

%% Copyright 2013-2026 Marc Worrell
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

-module(mod_export).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "integrator", "module", "export_and_syndication", "structured_data", "serialize", "export"
    ]
}).
-moduledoc(<<"
Provides a generic framework for exporting
[resources](/id/doc_glossary#term-resource), query results, and application-defined
data. An export consists of a data provider and an encoder:

* The data provider supplies a header and rows. It can be a module named in the
  dispatch rule, or a set of observers for the export notifications.
* The encoder turns those values into CSV, XLSX, JSON, Atom, RSS, JSON Feed, iCalendar, BERT, or
  UBF output.

The response is streamed by `controller_export_resource` or `controller_export`.
Most encoders can emit rows immediately. XLSX and BERT collect their rows and
create the final document in the footer phase.


Syndication feeds
-----------------

Atom and RSS entries include their canonical resource URI as the ID or GUID and
a JSON resource link with relation `https://zotonic.com/rel/resource`. The link points to
the `rsc_export` model API, allowing importers to recognize Zotonic resources and
use the native importer for properties, media and connections. Normal resource
visibility checks apply to the export endpoint.

JSON Feed 1.1 items use the same canonical URI as `id` and advertise the native
resource endpoint in `_zotonic.resource_url`. `mod_import_feed` consumes all three
formats and uses these explicit links for native resource imports.

Use `/export/query/rss/<id>` or `/export/query/jsonfeed/<id>` for a saved query,
and `/export/rsc/rss/<id>` or `/export/rsc/jsonfeed/<id>` for one resource.
Query feeds are served inline. JSON Feed uses `application/feed+json`; the existing
`json` export remains the generic `application/json` array format.

RSS supports category-specific `rss/entry.tpl` templates, full HTML content,
UTC publication dates and file enclosures with their MIME type and byte size.
JSON Feed streams its items and includes content, authors, dates and attachments.
Its textual fields are plain text, while `content_html` retains HTML markup.

Built-in Atom feeds use their canonical export URL as the feed ID and self link.
The title defaults to the site title followed by the exported resource's title;
an export header overrides the resource title. File attachments are enclosures,
with a single text or HTML content element for each entry.

The feed update time is the latest modification time of the exported resource
and its visible entries. It is emitted by `atom/feed_end.tpl`, after the entries,
so the response can still be streamed. An empty query uses the query resource's
modification time. Exports without any modification time use the Unix epoch.
This timestamp does not track removals from query results. Sites overriding the
Atom templates should keep the feed update element in the footer.


Conditional feed requests
-------------------------

Atom, RSS and JSON Feed responses on the `feed` dispatch include
`ETag` and `Last-Modified`. Clients should save both values and send them back as
`If-None-Match` and `If-Modified-Since`. An unchanged representation returns
HTTP 304 with no body. `If-None-Match` takes precedence over the date condition.

The weak ETag fingerprints the ordered resource IDs, modification dates and
versions, including the query resource. Changes in membership, ordering and
resource revisions are detected without rendering the body. The selected IDs
are reused by the encoder, preserving streaming and avoiding a second search.
Template changes and changes to related resources that do not update an
exported resource's version are not included in this fingerprint.

Only `controller_feed` performs this check, on at most 100 selected entries.
`controller_export` and `controller_export_resource`, including the
`export_rsc_query` dispatch, stream normally without precomputing resource
fingerprints or handling conditional feed requests.

`Last-Modified` records when a change was observed, rather than the newest
entry timestamp. Fingerprints are cached per URL, format, user and language
for 24 hours. A cache miss or a same-second change can force a full response
for date-only requests; ETag validation remains available. Responses use
`Cache-Control: private, no-cache` and still enforce normal access checks.

Conditional requests validate one URL's entire response. They do not request
only new entries, follow subsequent pages, or bypass the limits below.


Export limits
-------------

The site configuration `mod_export.query_max_rows` sets the maximum number of
results returned by `export_rsc_query` (and other exports using `is_query`).
It defaults to 50,000. Set it to a positive integer in Admin's configuration
editor, for example `mod_export` / `query_max_rows` / `5000`. Missing, invalid,
zero or negative values use the default. Request `page` selects a page and `pagelen` can lower its size, but cannot
exceed this setting.

The `feed` routes have a separate fixed cap of 100 items per page and default
to 20. Larger `pagelen` values are clamped to 100, including `qpagelen` and
requests using the default stored query. Changing `query_max_rows` does not
change this feed cap.


Atom, RSS and JSON Feed responses advertise the next page using Atom `rel=next`,
RSS `atom:link rel=next`, and JSON Feed `next_url`. Links preserve request filters
and replace `page`. A full page advertises a successor, so the last request can
return an empty page. For importing bulk query exports with `mod_import_feed`,
use `pagelen=100` (or less), matching its per-response entry limit.

Publication start descending and resource ID descending provide the fallback
ordering; explicit query sorts take precedence. Pagination currently uses page
numbers, so concurrent additions or deletions can shift page boundaries. Query
expansion and cursor-based pagination are deferred until the SPARQL work merges.


Admin interface
---------------

When [enabled](/id/doc_developerguide_modules#activating-modules), this module
adds export content types to the View menu on admin edit pages and adds an Export
block. Both a single resource and a
[query resource](/id/doc_developerguide_search#guide-query-resources) can be
exported. A query export contains at most `mod_export.query_max_rows` matching resource
ids (50,000 by default).


Built-in dispatch rules
-----------------------

The built-in rules are defined in `priv/dispatch/dispatch_export`:

```erlang
[
    {export_rsc, [\"export\", \"rsc\", type, id],
        controller_export_resource, []},
    {export_rsc_query, [\"export\", \"query\", type, id],
        controller_export_resource, [{is_query, true}]}
].
```

Short feed URLs are also available:

* `/feed` - the default Atom feed (saved query `query_feed`, or published articles).
* `/feed/rss?cat=article` - visible articles, newest publication first.
* `/feed/atom/news` - results from the visible saved query named `news`.
* `/feed/jsonfeed/news?text=Erlang` - the saved query with an extra text filter.
* `/feed/json?id=news&qcat=article` - JSON Feed, using query-string selection.

The bare `/feed` route uses the visible saved-query resource named `query_feed`.
If it is missing, is not a query, or is not visible, it searches the `article`
category with `is_published=true` instead (including publication start/end checks). Only pagination arguments are accepted on this fixed feed;
`id`, `type`, and search filters cannot replace its selection. Link to it with
`{% url feed %}`. The typed routes use the same default selection when no id or search terms
are supplied, including requests with only pagination arguments. Explicit ids
and search terms keep their request-defined selection.

The `feed` dispatch accepts `type` and an optional `id`. Supported types are
`atom`, `rss`, `jsonfeed`, and `json` (a JSON Feed alias on this route only).
An id selects saved-query results, collection members, or a single ordinary
resource. Additional search terms are combined with that selection. Missing ids
return not found and invisible resources are denied; neither becomes an
unrestricted search. Without an id or search terms, the feed uses `query_feed` or visible, published articles.

Feed searches use `mod_search` and accept its normal bare or q-prefixed query
arguments, including repeated terms. `page` defaults to 1 and `pagelen` to 20,
with at most 100 items per page. Without a saved query or explicit sort, results
sort by descending publication start. Saved queries retain their own sort terms.
The feed URL/self link includes the query string, and all shorthand feeds are
served inline. For template links, use `{% url feed type='rss' id=id %}` and add
search arguments as needed.

Choosing between `export_rsc_query` and `feed`
--------------------------------------------

Use `export_rsc_query` to export a saved query in bulk. Use `feed` for a
paginated syndication URL, a request-defined search, or the site's default feed.
They share the same encoders, but select and limit their results differently.

| Behavior | `export_rsc_query` | `feed` |
| --- | --- | --- |
| URL | `/export/query/<type>/<id>` | `/feed`, `/feed/<type>`, `/feed/<type>/<id>` |
| Resource selection | Requires an id; reads that resource's stored `query` property. Intended for saved queries. | Optional id: saved-query results, collection members, or one ordinary resource. |
| No id or search terms | No default selection; an id must be supplied in the path or query string. | Uses the visible saved query `query_feed`, falling back to published `article` resources when unavailable. |
| Request filters | Does not automatically merge request search arguments into the stored query. | Typed routes accept bare or q-prefixed search terms, combined with any id selection. |
| Pagination | Page 1 by default, up to `query_max_rows` results (default 50,000); `page` and a smaller `pagelen` are accepted. | Default page 1, 20 items; `page` and `pagelen` are accepted, with at most 100 items per page. |
| Sorting | Uses the stored query's sort terms. | Retains saved-query sorting; otherwise defaults to descending publication start unless an explicit sort is supplied. |
| Formats | All registered export formats, including CSV, XLSX, Atom, RSS, JSON Feed and generic JSON. | Atom, RSS and JSON Feed only. |
| Meaning of `json` | Generic JSON array (`application/json`); use `jsonfeed` for JSON Feed. | Alias for `jsonfeed` (`application/feed+json`). |
| Omitted type | HTTP content negotiation. | Bare `/feed` is Atom. Typed routes use their path type. |
| Conditional HTTP requests | No feed validator precomputation; results are streamed normally. | Supports `ETag`, `Last-Modified` and HTTP 304 on the bounded page. |
| Content disposition | Atom, RSS and JSON Feed are inline; other formats are attachments by default. | Always inline. |

For example, `/export/query/rss/news` exports up to the configured limit (default 50,000) from the
stored query `news`. `/feed/rss/news?page=2&pagelen=10` serves its second page
of ten results, and `/feed/rss/news?text=Erlang` adds a text filter.
`/feed/rss` uses the default site selection; `/feed/rss?cat=event` instead
searches events. `/feed/rss?page=2` keeps the default selection and changes
only the page.

Template links use the corresponding dispatch name:

```django
{% url export_rsc_query type='rss' id='news' %}
{% url feed type='rss' id='news' page=2 pagelen=10 %}
{% url feed type='rss' %}
{% url feed %}
```

Both routes search with the visitor's ACL context. Explicit missing or invisible
ids do not trigger the default feed fallback. `export_rsc_query` also supports
export visibility overrides through `export_resource_visible`; `feed` checks
resource visibility directly. A stored query can itself contain `qargs=true`
to read request arguments; this is behavior of the stored query, separate from
the automatic filter handling described above.

The export routes also have `/export/query/<id>` and `/export/query?id=<id>`
variants. Their `type` argument is an export extension such as `csv`, `xlsx`,
or `json`. Mod_export also registers the encoders as fallbacks for the normal
resource `id` controller.

Use `controller_export_resource` when the export belongs to a resource. It checks
that the resource exists and, by default, that it is visible. Use
`controller_export` for an export that has no resource id.

A custom export can bind all callbacks to one module:

```erlang
{my_export, [\"my\", \"export\", type, id],
    controller_export_resource,
    [{export_module, my_export}]}
```

A fixed-format export does not need a `type` path argument:

```erlang
{my_export_xlsx, [\"my\", \"export\", id],
    controller_export_resource,
    [{export_module, my_export}, {content_type, xlsx}]}
```

Useful dispatch options are:

* `export_module` - module implementing the export callbacks described below.
* `content_type` - a fixed extension atom, for example `xlsx`, or a MIME type.
* `is_query` - treat `id` as a query resource and export its result ids.
* `rsc_props` - resource properties or expressions used as columns by the
  tabular encoders.
* `header_template` and `row_template` - templates for the iCalendar header and
  event rows. Their defaults are `_vcalendar_header.tpl` and `_vevent.tpl`.

CSV and XLSX requests also accept `?raw=1`. Without it, textual values are
converted from HTML to plain text. Raw mode retains the original text while
still applying the escaping required by the selected format.


Export module callbacks
-----------------------

An `export_module` can export any subset of the following two-argument
functions. The function name is the notification record name, without an
`observe_` prefix. Every callback receives the record and the request context.
Missing callbacks return `undefined` and use the controller or encoder default;
they do not fall back to notification observers.

The callbacks are called in these phases:

1. `export_resource_visible/2` authorizes the request. Return `true`, `false`,
   or `undefined`. The resource controller defaults to resource visibility; the
   controller without a resource defaults to allowed.
2. `export_resource_content_type/2` can return `{ok, MimeType}`. A dispatch
   `content_type` takes precedence, followed by this callback and then the
   `type` request argument.
3. `export_resource_content_disposition/2` returns `{ok, <<\"attachment\">>}` or
   `{ok, <<\"inline\">>}`. The default is attachment.
4. `export_resource_filename/2` returns `{ok, Filename}`. Mod_export adds or
   corrects the extension for the selected encoder.
5. `export_resource_header/2` returns `{ok, Header}` or
   `{ok, Header, ExporterState}`. For tabular formats, `Header` is normally a
   list of column names.
6. `export_resource_data/2` returns `{ok, Rows}` or
   `{ok, Rows, NextExporterState}`. `Rows` is a list. Return the next state to
   fetch another batch; use `undefined` as the next state for the final batch.
   If this callback is absent, a resource export emits its resource id as the
   only row and an export without a resource emits no rows.
7. `export_resource_encode/2` is called once for each item returned by the data
   callback. Return `{ok, Row}` or `{ok, Row, NextExporterState}` to transform
   it into an encoder row. If absent, the item is sent directly to the encoder.
8. `export_resource_footer/2` is called after the last row and can return
   `{ok, Footer}`. It is also the place to clean up resources held in the
   exporter state.

The `dispatch`, `id`, and `content_type` fields identify the request. The data,
encode, and footer records also contain `state`. A small tabular exporter can
therefore look like this:

```erlang
-include_lib(\"zotonic_core/include/zotonic.hrl\").

export_resource_visible(#export_resource_visible{}, Context) ->
    z_acl:is_admin(Context).

export_resource_header(#export_resource_header{}, _Context) ->
    {ok, [<<\"Name\">>, <<\"Count\">>], first_page}.

export_resource_data(#export_resource_data{state = first_page}, _Context) ->
    {ok, [[<<\"Example\">>, 10]], undefined}.
```

Do not put authorization solely in templates or links. In particular,
`controller_export` has no resource whose visibility can be used as a default,
so a private export must implement `export_resource_visible/2`.


Notification-based data providers
---------------------------------

As an alternative to `export_module`, omit that dispatch option and listen for
the first-notification variants of the same phases. Observer functions have the
usual `observe_` prefix, for example:

```erlang
observe_export_resource_header(
        #export_resource_header{dispatch = my_export},
        _Context) ->
    {ok, [<<\"Name\">>, <<\"Count\">>], first_page};
observe_export_resource_header(#export_resource_header{}, _Context) ->
    undefined.

observe_export_resource_data(
        #export_resource_data{dispatch = my_export, state = first_page},
        _Context) ->
    {ok, [[<<\"Example\">>, 10]], undefined};
observe_export_resource_data(#export_resource_data{}, _Context) ->
    undefined.
```

The available notifications are `#export_resource_visible{}`,
`#export_resource_content_type{}`, `#export_resource_content_disposition{}`,
`#export_resource_filename{}`, `#export_resource_header{}`,
`#export_resource_data{}`, `#export_resource_encode{}`, and
`#export_resource_footer{}`. They are first notifications, so observers must
match their own dispatch and return `undefined` for exports they do not handle.
This style is useful when an existing module wants to augment exports without a
dedicated callback module.


Export formats
--------------

| Extension | MIME type | Row representation and notes |
| ---------- | --------- | ---------------------------- |
| `csv` | `text/csv` | Tabular data. A header list defines the columns. |
| `xlsx` | `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` | Tabular workbook preserving numbers and dates. Cells can have a background color. |
| `json` | `application/json` | JSON array. List rows are combined with header names; map rows remain objects. Duplicate or empty header names are made unique. Dates are rendered in ISO form. |
| `atom` | `application/atom+xml` | Atom feed. Integer rows are resource ids rendered with category-specific `atom/entry.tpl` templates. |
| `rss` | `application/rss+xml` | RSS 2.0, using category-specific `rss/entry.tpl` templates. |
| `jsonfeed` | `application/feed+json` | JSON Feed 1.1, with visible resource ids as rows. Downloads use `.json`. |
| `ics` | `text/calendar` | iCalendar stream rendered with `_vcalendar_header.tpl` and `_vevent.tpl`. |
| `bert` | `application/x-bert` | Erlang external term containing full resource maps for integer resource-id rows. |
| `ubf` | `text/x-ubf` | UBF stream containing resource maps for integer resource-id rows. |

For XLSX presentation metadata, wrap a value with
`export_encoder:cell(Value, #{background_color => <<\"#RRGGBB\">>})`. Encoders
without cell styling, including CSV and JSON, export the unwrapped value.

Adding a wire format requires an encoder module with `extension/0`, `mime/0`,
`init/2`, `header/3`, `row/3`, and `footer/3`, and adding the module to
`export_encoder:encoders/0`. Application export modules normally implement only
the data-provider callbacks above.


Accepted events
---------------

This module handles the following notifier callbacks:

* `observe_content_types_dispatch/3` adds encoder content types to the `id`
  controller as fallbacks.
* `observe_export_resource_content_disposition/2` returns `inline` for Atom, RSS and JSON Feed
  query feeds and `attachment` for other generated exports.
">>).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Export Data").
-mod_description("Exports data as CSV and other formats.").
-mod_prio(800).
-mod_depends([mod_base]).
-mod_config([
    #{
        key => query_max_rows,
        type => integer,
        default => 50000,
        description => "Maximum number of rows in query exports. Must be positive; "
                       "invalid values use 50000. Does not affect the feed page cap of 100."
    }
]).

-export([
    observe_content_types_dispatch/3,
    observe_export_resource_content_disposition/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Add extra content-type to the 'id' controller; as fallbacks for content-types
%% the API controller can't handle.
observe_content_types_dispatch(#content_types_dispatch{id=Id}, Acc, Context) ->
    Acc ++ export_encoder:content_types_dispatch(Id, Context).


%% @doc Get the content-disposition for the export
observe_export_resource_content_disposition(
        #export_resource_content_disposition{
            dispatch = export_rsc_query,
            content_type = ContentType
        },
        _Context) when ContentType =:= <<"application/atom+xml">>;
                      ContentType =:= <<"application/rss+xml">>;
                      ContentType =:= <<"application/feed+json">> ->
    {ok, <<"inline">>};
observe_export_resource_content_disposition(#export_resource_content_disposition{}, _Context) ->
    {ok, <<"attachment">>}.
