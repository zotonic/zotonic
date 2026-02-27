%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell
%% @doc Generates a sitemap.  For now rather crude version that will only work with smaller sites.

%% Copyright 2009-2022 Marc Worrell
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

-module(mod_seo_sitemap).
-moduledoc("
Creates a `sitemap.xml` file for your site, containing links to all publicly accessible pages.

mod\\_seo\\_sitemap creates a `sitemap.xml` file for which is used by the Google bot to index your site. By default, its
generated sitemap.xml lists all pages of the categories text, event, location, collection and person in the system which
are publicly viewable.

The sitemaps follow the specification at <https://www.sitemaps.org/protocol.html>



Maintaining the sitemap index
-----------------------------

The `seo_sitemap` model maintains an index of all pages (and their URLs) that are publicly readable.

There is a button in the System > Status menu to rebuild the sitemap index. This is needed if there are (big) changes in
the access control affecting the publicly viewable pages or if languages are changed.

If a resource is edited then all URLs of that resource are automatically added to the sitemap index tables.



Structure of the sitemap
------------------------

The generated sitemap is an index file with links to urlset files.

The urlsets links are generated from a combination of two sources:

1.   All resources and URLs in the sitemap index tables maintained by model `seo_sitemap`
2.   Urlsets for by the `#seo_sitemap_index{}` notification.

The `#seo_sitemap_index{}` is a map notification where all modules can return 0 or more maps with information about
their pages. The map should be like:


```erlang
#{
    source => <<\"wherefrom\">>,          % Unique code
    count => 1234,                      % Number of URLs
    size => 5000,                       % (optional) number of URLs per urlset
    lastmod => {{2021,5,12}, {0,0,0}}   % (optional) last modification date
}
```

The sitemap urlset is generated using the `#seo_sitemap_urlset{}` notification:


```erlang
#seo_sitemap_urlset, {
    source => <<\"wherefrom\">>,          % Unique code from the ``#seo_sitemap_index{}``
    offset => 1,                        % Offset for search, 1 based.
    limit => 50000                      % Number of URLs to be returned
}
```

This is a *first* notification and should return a list of maps. Each map contains a single URL:


```erlang
#{
    loc => <<\"https://example.com/...\", % The URL for the urlset
    lastmod => {{2021,5,12}, {0,0,0}}}, % (optional) last modification date
    priority => 0.5,                    % (optional) priority, between 0.0..1.0
    changefreq => <<\"weekly\">>          % (optional) change frequency, for recrawls
}
```



Creating a sitemap without using mod\\_seo\\_sitemap
--------------------------------------------------

If you have a site with mostly static URLS, it might be easier to create your own sitemap XML file. If you create a
dispatch rule like this:


```erlang
{sitemap_xml, [\"sitemap.xml\"], controller_template, [{template, \"mysitemap.tpl\"}, {content_type, \"text/xml\"}]}
```

You will be able to write your own sitemap.xml file (in a mysitemap.tpl template) without using mod\\_seo\\_sitemap.
This might be handy if you serve mostly static files.

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_rsc_pivot_done`: Handle `rsc_pivot_done` notifications using `m_seo_sitemap:update_rsc`.
- `observe_seo_sitemap_index`: Return the number of entries in the mod_seo_sitemap managed sitemaps using `m_seo_sitemap:count`.
- `observe_seo_sitemap_urlset`: Return the urlset belonging to the slice given using `m_seo_sitemap:slice`.

Delegate callbacks:

- `event/2` with `postback` messages: `sitemap_rebuild`.

").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("SEO Sitemap").
-mod_description("Generates sitemap for crawlers, enables better indexing of your site.").
-mod_prio(600).
-mod_depends([seo]).
-mod_provides([seo_sitemap]).
-mod_schema(3).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/seo_sitemap.hrl").

-export([
    observe_seo_sitemap_index/2,
    observe_seo_sitemap_urlset/2,
    observe_rsc_pivot_done/2,

    event/2,

    manage_schema/2
]).


%% @doc Return the number of entries in the mod_seo_sitemap managed sitemaps.
%% All sources in the seo_sitemap indices are grouped together.
observe_seo_sitemap_index(#seo_sitemap_index{}, Context) ->
    #{
        source => <<"seo_sitemap">>,
        count => m_seo_sitemap:count(Context)
    }.

%% @doc Return the urlset belonging to the slice given.
observe_seo_sitemap_urlset(#seo_sitemap_urlset{ source = <<"seo_sitemap">>, offset = Offset, limit = Limit }, Context) ->
    case m_seo_sitemap:slice(Offset, Limit, Context) of
        {ok, UrlSet} -> UrlSet;
        {error, _} -> []
    end;
observe_seo_sitemap_urlset(#seo_sitemap_urlset{}, _Context) ->
    undefined.

observe_rsc_pivot_done(#rsc_pivot_done{ id = Id }, Context) ->
    m_seo_sitemap:update_rsc(Id, Context).

event(#postback{ message = sitemap_rebuild }, Context) ->
    case z_acl:is_admin(Context)
        orelse z_acl:is_allowed(use, mod_seo_sitemap, Context)
        orelse z_acl:is_allowed(use, mod_seo, Context)
    of
        true ->
            m_seo_sitemap:rebuild_rsc(Context),
            z_notifier:notify(seo_sitemap_rebuild, Context),
            z_render:growl(?__("Rebuilding the sitemap in the background.", Context), Context);
        false ->
            z_render:growl(?__("You are not allowed to do this", Context), Context)
    end.

manage_schema(_, Context) ->
    m_seo_sitemap:install(Context).
