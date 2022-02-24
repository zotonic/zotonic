%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2021 Marc Worrell
%% Date: 2009-08-14
%% @doc Generates a sitemap.  For now rather crude version that will only work with smaller sites.

%% Copyright 2009-2021 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("SEO Sitemap").
-mod_description("Generates sitemap for crawlers, enables better indexing of your site.").
-mod_prio(600).
-mod_depends([seo]).
-mod_provides([seo_sitemap]).
-mod_schema(3).

-include_lib("zotonic.hrl").
-include("include/seo_sitemap.hrl").

-export([
    observe_seo_sitemap_index/2,
    observe_seo_sitemap_urlset/2,
    observe_rsc_update_done/2,

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

observe_rsc_update_done(#rsc_update_done{ id = Id, action = Action }, Context) when Action =/= delete ->
    m_seo_sitemap:update_rsc(Id, Context);
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    undefined.


event(#postback{ message = sitemap_rebuild }, Context) ->
    case z_acl:is_admin(Context)
        orelse z_acl:is_allowed(use, mod_seo_sitemap, Context)
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
