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
-moduledoc(<<"
Provides a generic framework for exporting
[resources](/id/doc_glossary#term-resource), query results, and application-defined
data. An export consists of a data provider and an encoder:

* The data provider supplies a header and rows. It can be a module named in the
  dispatch rule, or a set of observers for the export notifications.
* The encoder turns those values into CSV, XLSX, JSON, Atom, iCalendar, BERT, or
  UBF output.

The response is streamed by `controller_export_resource` or `controller_export`.
Most encoders can emit rows immediately. XLSX and BERT collect their rows and
create the final document in the footer phase.


Admin interface
---------------

When [enabled](/id/doc_developerguide_modules#activating-modules), this module
adds export content types to the View menu on admin edit pages and adds an Export
block. Both a single resource and a
[query resource](/id/doc_developerguide_search#guide-query-resources) can be
exported. A query export contains at most 50,000 matching resource ids.


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

The shorter variants without `type` and/or `id` are also present. The `type`
path argument is a filename extension such as `csv`, `xlsx`, or `json`. Without
a type, `controller_export_resource` uses HTTP content negotiation. Mod_export
also registers the encoders as fallbacks for the normal resource `id` controller.

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
* `observe_export_resource_content_disposition/2` returns `inline` for Atom
  query feeds and `attachment` for other generated exports.
">>).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Export Data").
-mod_description("Exports data as CSV and other formats.").
-mod_prio(800).
-mod_depends([mod_base]).

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
            content_type = <<"application/atom+xml", _/binary>>
        },
        _Context) ->
    {ok, <<"inline">>};
observe_export_resource_content_disposition(#export_resource_content_disposition{}, _Context) ->
    {ok, <<"attachment">>}.
