%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Model for tracking all URLs to be included in the sitemap.

%% Copyright 2021 Marc Worrell
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

-module(m_seo_sitemap).

-export([
    m_find_value/3
    ]).

-export([
    count/1,
    max_id/1,
    slice/3,
    update/2,
    delete_key/3,
    delete_before/3,
    delete_loc/2,

    update_rsc/2,
    rebuild_rsc/1,
    install/1
    ]).

-export([
    rebuild_rsc_task/2
    ]).

-include_lib("zotonic.hrl").
-include("../include/seo_sitemap.hrl").

m_find_value(urlsets, #m{ value = undefined }, Context) ->
    seo_sitemap:urlsets(Context);
m_find_value(urlset, #m{ value = undefined } = M, _Context) ->
    M#m{ value = urlset };
m_find_value(Source, #m{ value = urlset } = M, _Context) ->
    M#m{ value = {urlset, z_convert:to_binary(Source)} };
m_find_value(Offset, #m{ value = {urlset, Source} } = M, _Context) ->
    Offs = case Offset of
        undefined -> 1;
        _ ->
            try z_convert:to_integer(Offset)
            catch _:_ -> 1
            end
    end,
    Offs1 = erlang:max(1, Offs),
    M#m{ value = {urlset, Source, Offs1} };
m_find_value(Limit, #m{ value = {urlset, Source, Offset} }, Context) ->
    Limit1 = case Limit of
        undefined -> ?URLSET_SIZE;
        _ ->
            try z_convert:to_integer(Limit)
            catch _:_ -> ?URLSET_SIZE
            end
    end,
    Limit2 = erlang:min(?URLSET_SIZE, Limit1),
    seo_sitemap:urlset(Source, Offset, Limit2, Context).

%% @doc Return the number of sitemap entries
-spec count( z:context() ) -> non_neg_integer().
count( Context ) ->
    z_db:q1("select count(*) from seo_sitemap", Context).

-spec max_id( z:context() ) -> non_neg_integer().
max_id( Context ) ->
    z_db:q1("select max(id) from seo_sitemap", Context).


%% @doc Return a slice from the sitemap entries. Note that this might
%% give duplicate entries in sitemaps, as the ids might change during
%% sitemap discovery and consumption by the crawler.
%% All locations are made into absolute urls with the correct hostname.
%% As entries are filtered on publication_end, their language and resource
%% visibility, this might return less entries than the Limit amount requested.
-spec slice( pos_integer(), pos_integer(), z:context() ) -> {ok, list( map() )} | {error, empty}.
slice( Offset, Limit, Context ) ->
    Rows = z_db:assoc("
        select *
        from seo_sitemap
        order by id desc
        offset $1
        limit $2",
        [ Offset-1, Limit ],
        Context),
    case Rows of
        [] -> {error, empty};
        _ ->
            Now = calendar:universal_time(),
            AnonContext = z_context:new( z_context:site(Context) ),
            Langs = enabled_languages(AnonContext),
            LangsB = lists:map( fun z_convert:to_binary/1, Langs ),
            Rows1 = lists:filtermap(
                fun
                    (#{ publication_end := PubEnd }) when is_tuple(PubEnd), PubEnd < Now ->
                        false;
                    (#{ loc := Loc } = Url) ->
                        case is_visible(Url, LangsB, AnonContext) of
                            true ->
                                Url1 = Url#{
                                    loc => z_context:abs_url(Loc, AnonContext)
                                },
                                Url2 = maybe_add_category_attrs(Url1, AnonContext),
                                {true, Url2};
                            false ->
                                false
                        end
                end,
                lists:map(fun maps:from_list/1, Rows)),
            {ok, Rows1}
    end.

%% @doc Filter resources and urls that are not visible
is_visible(#{ rsc_id := RscId, language := undefined }, _Languages, Context) when is_integer(RscId) ->
    z_acl:rsc_visible(RscId, Context);
is_visible(#{ rsc_id := RscId, language := Lang }, Languages, Context) when is_integer(RscId) ->
    lists:member(Lang, Languages) andalso z_acl:rsc_visible(RscId, Context);
is_visible(#{ language := Lang }, Languages, _Context) when is_binary(Lang) ->
    lists:member(Lang, Languages);
is_visible(_, _, _Context) ->
    true.

%% @doc Optionally add priority and changefreq from the category definition
maybe_add_category_attrs(#{ category_id := undefined } = Map, _Context) ->
    Map;
maybe_add_category_attrs(#{ category_id := CatId } = Map, Context) ->
    Map1 = case Map of
        #{ priority := undefined } ->
            case m_rsc:p_no_acl(CatId, seo_sitemap_priority, Context) of
                undefined -> Map#{ priority := 0.5 };
                Prio ->
                    try
                        Map#{ priority := z_convert:to_float(Prio) }
                    catch
                        _:_ -> Map
                    end
            end;
        _ ->
            Map
    end,
    Map2 = case Map1 of
        #{ changefreq := undefined } ->
            Freq = case m_rsc:p_no_acl(CatId, seo_sitemap_changefreq, Context) of
                <<>> -> <<"weekly">>;
                undefined -> <<"weekly">>;
                CF -> CF
            end,
            Map1#{ changefreq := Freq };
        _ ->
            Map1
    end,
    Map2.


%% @doc Insert or update a location for the sitemap. The entry must have a key to be uniquely
%% identified. The loc may be a path or a complete url with hostname. If the hostname is omitted
%% then the default hostname for the site will be used. The source 'rsc' is reserved for
%% resources (pages), suggested is to use a site or module name as source.
-spec update( map(), z:context() ) -> ok | {error, term()}.
update(#{ loc := Loc, source := Source, key := Key } = Url, Context) ->
    case is_loc_acceptable(Loc) of
        true ->
            Url1 = Url#{ modified => calendar:universal_time() },
            UrlList = maps:to_list(Url1), % In 0.x the z_db routines need a list
            case z_db:q1("
                select id
                from seo_sitemap
                where source = $1
                  and key = $2
                ",
                [ Source, Key ],
                Context)
            of
                undefined ->
                    case z_db:insert(seo_sitemap, UrlList, Context) of
                        {ok, _} -> ok;
                        {error, _} = Error -> Error
                    end;
                Id ->
                    case z_db:update(seo_sitemap, Id, UrlList, Context) of
                        {ok, 1} -> ok;
                        {ok, 0} -> update(Url, Context);
                        {error, _} = Error -> Error
                    end
            end;
        false ->
            {error, loc}
    end.

is_loc_acceptable(<<"/", _/binary>>) -> true;
is_loc_acceptable(<<"https:", _/binary>>) -> true;
is_loc_acceptable(<<"http:", _/binary>>) -> true;
is_loc_acceptable(_) -> false.


%% @doc Delete a complete source (key 'all') or a specific source key from the sitemap.
-spec delete_key( binary(), binary() | all, z:context() ) -> ok | {error, term()}.
delete_key(Source, all, Context) ->
    case z_db:q("
        delete from seo_sitemap
        where source = $1",
        [ Source ],
        Context)
    of
        0 -> {error, enoent};
        _ -> ok
    end;
delete_key(Source, Key, Context) ->
    case z_db:q("
        delete from seo_sitemap
        where source = $1
          and key = $2",
        [ Source, Key ],
        Context)
    of
        0 -> {error, enoent};
        _ -> ok
    end.

%% @doc Delete all keys in a source that have been modified before a certain date.
%% This is useful to cleanup after batch updates, as the insert always updates
%% the modified date.
-spec delete_before( binary(), calendar:datetime(), z:context() ) -> ok | {error, term()}.
delete_before(Source, Modified, Context) ->
    case z_db:q("
        delete from seo_sitemap
        where source = $1
          and modified < $2",
        [ Source, Modified ],
        Context)
    of
        0 -> {error, enoent};
        _ -> ok
    end.

%% @doc Delete a specific location from the sitemap.
-spec delete_loc( binary(), z:context() ) -> ok | {error, term()}.
delete_loc(Loc, Context) ->
    case z_db:q("
        delete from seo_sitemap
        where loc = $1",
        [ Loc ],
        Context)
    of
        0 -> {error, enoent};
        _ -> ok
    end.


%% @doc Rebuild the seo sitemap for all resources. This needs to be done after major
%% access control changes, on the initial install of this module or when changing the language
%% configuration.
-spec rebuild_rsc( z:context() ) -> ok.
rebuild_rsc(Context) ->
    Key = <<"seo_sitemap_rebuild_rsc">>,
    {ok, _} = z_pivot_rsc:insert_task_after(10, ?MODULE, rebuild_rsc_task, Key, [ 1 ], Context),
    ok.

%% @doc Task for fetching the seo sitemap locs for the next 1000 ids. This task repeats
%% till all ids are inserted into the seo_sitemap index table.
-spec rebuild_rsc_task( m_rsc:resource_id(), z:context() ) -> ok | {delay, integer(), [ m_rsc:resource_id() ]}.
rebuild_rsc_task(FromId, Context) ->
    Ids = z_db:q("
        select id from rsc where id >= $1
        order by id asc
        limit 1000
        ", [ FromId ], Context),
    Ids1 = [ Id || {Id} <- Ids ],
    lists:foreach(
        fun(Id) ->
            update_rsc(Id, Context)
        end,
        Ids1),
    case Ids1 of
        [] -> ok;
        _ -> {delay, 1, [ lists:last(Ids1)+1 ]}
    end.

%% @doc Insert entries for the given resource.
-spec update_rsc( m_rsc:resource_id(), z:context() ) -> ok | {error, term()}.
update_rsc(Id, Context) ->
    AnonContext = z_context:new( z_context:site(Context) ),
    case m_rsc:exists(Id, AnonContext)
        andalso m_rsc:is_visible(Id, AnonContext)
        andalso not z_convert:to_bool( m_rsc:p_no_acl(Id, seo_noindex, AnonContext) )
    of
        true ->
            Langs = case m_rsc:p_no_acl(Id, language, Context) of
                undefined -> [ z_context:language(AnonContext) ];
                <<>> -> [ z_context:language(AnonContext) ];
                [] -> [ z_context:language(AnonContext) ];
                Ls -> Ls
            end,
            Enabled = editable_languages(AnonContext),
            Locs = lists:filtermap(
                fun(Lang) ->
                    case lists:member(Lang, Enabled) of
                        true ->
                            CLang = z_context:set_language(Lang, AnonContext),
                            case m_rsc:p(Id, page_url, CLang) of
                                undefined ->
                                    false;
                                PageUrl ->
                                    Url = iolist_to_binary(PageUrl),
                                    {true, {z_convert:to_binary(Lang), Url}}
                            end;
                        false ->
                            false
                    end
                end,
                Langs),
            Locs1 = lists:usort(Locs),
            Current = z_db:q("
                        select language, loc
                        from seo_sitemap
                        where rsc_id = $1
                          and source = 'rsc'",
                        [ Id ],
                        Context),
            New = Locs1 -- Current,
            Del = Current -- Locs1,
            z_db:transaction(
                fun(Ctx) ->
                    lists:foreach(
                        fun({_Lang, Loc}) ->
                            z_db:q(
                                "delete from seo_sitemap
                                 where rsc_id = $1
                                   and loc = $2
                                   and source = 'rsc'",
                                [ Id, Loc ],
                                Ctx)
                        end,
                        Del),
                    LastMod = m_rsc:p_no_acl(Id, modified, Context),
                    CatId = m_rsc:p_no_acl(Id, category_id, Context),
                    PubEnd = m_rsc:p_no_acl(Id, publication_end, Context),
                    Prio = case m_rsc:p_no_acl(Id, page_path, Context) of
                        <<"/">> -> 1.0;
                        undefined -> undefined;
                        <<>> -> undefined;
                        _ -> 0.8
                    end,
                    lists:foreach(
                        fun({Lang, Loc}) ->
                            z_db:q(
                                "insert into seo_sitemap
                                    (source, rsc_id, category_id, loc, lastmod, priority, publication_end, language)
                                values
                                    ('rsc', $1, $2, $3, $4, $5, $6, $7)",
                                [ Id, CatId, Loc, LastMod, Prio, PubEnd, Lang ],
                                Ctx)
                        end,
                        New),
                    case Current -- Del of
                        [] ->
                            ok;
                        _ ->
                            z_db:q("
                                update seo_sitemap
                                set category_id = $1,
                                    lastmod = $2,
                                    publication_end = $3,
                                    modified = now()
                                where rsc_id = $4
                                  and source = 'rsc'
                                  and (   category_id <> $1
                                       or lastmod <> $2
                                       or publication_end <> $3)",
                                [ CatId, LastMod, PubEnd, Id ],
                                Ctx)
                    end,
                    ok
                end,
                Context);
        false ->
            z_db:q("delete from seo_sitemap where rsc_id = $1 and source = 'rsc'", [ Id ], Context),
            maybe_insert_update_task(Id, Context)
    end.


%% @doc Insert an update task for if the resource is not visible now but has a
%% publication date in the future. The resource sitemap entries will be updated
%% after the publication start date has passed.
maybe_insert_update_task(Id, Context) ->
    case m_rsc:p_no_acl(Id, publication_start, Context) of
        undefined -> ok;
        Date ->
            case Date > calendar:universal_time() of
                true ->
                    Date1 = z_datetime:next_minute(Date),
                    Key = <<"seo_sitemap:", (z_convert:to_binary(Id))/binary>>,
                    z_pivot_rsc:insert_task_after(Date1, ?MODULE, update_rsc, Key, [ Id ], Context),
                    ok;
                false ->
                    ok
            end
    end.


install(Context) ->
    case z_db:table_exists(seo_sitemap, Context) of
        false ->
            [] = z_db:q("
                create table seo_sitemap (
                    id bigserial not null,
                    source character varying(32) not null default 'rsc',
                    key character varying(64) not null default '',
                    rsc_id integer,
                    category_id integer,
                    loc character varying(2000) not null,
                    lastmod timestamp with time zone,
                    changefreq character varying(10),
                    language character varying(32),
                    priority float,
                    publication_end timestamp with time zone NOT NULL DEFAULT '9999-06-01 00:00:00'::timestamp with time zone,
                    modified timestamp with time zone NOT NULL DEFAULT now(),

                    constraint seo_sitemap_pkey primary key(id),
                    constraint fk_seo_sitemap_rsc_id foreign key (rsc_id) references rsc(id)
                        on delete cascade
                        on update cascade,
                    constraint fk_seo_sitemap_category_id foreign key (category_id) references rsc(id)
                        on delete cascade
                        on update cascade
                )
                ", Context),
            Indices = [
                {"seo_sitemap_source_key_key", "source, key"},
                {"seo_sitemap_loc_key", "loc"},
                {"fki_seo_sitemap_rsc_id", "rsc_id"},
                {"fki_seo_sitemap_category_id", "category_id"}
            ],
            [ z_db:q("create index "++Name++" on seo_sitemap ("++Cols++")", Context) || {Name, Cols} <- Indices ],
            z_db:flush(Context),
            % Insert the rebuild task
            rebuild_rsc(Context),
            ok;
        true ->
            ok
    end.


enabled_languages(Context) ->
    case get_enabled_languages(Context) of
        [] -> [ z_context:language(Context) ];
        LangProps -> [ Lang || {Lang, _} <- LangProps ]
    end.

editable_languages(Context) ->
    case get_editable_languages(Context) of
        [] -> [ z_context:language(Context) ];
        LangProps -> [ Lang || {Lang, _} <- LangProps ]
    end.

%% Copied from mod_translation - in the 1.x we can use the z_language routines.
get_enabled_languages(Context) ->
    case z_memo:get('mod_translation$enabled_languages') of
        V when is_list(V) ->
            V;
        _ ->
            Languages = lists:filter(fun({_,Props}) -> proplists:get_value(is_enabled, Props) =:= true end,
                                     get_language_config(Context)),
            z_memo:set('mod_translation$enabled_languages', Languages)
    end.

get_editable_languages(Context) ->
    case z_memo:get('mod_translation$enabled_languages') of
        V when is_list(V) ->
            V;
        _ ->
            Languages = lists:filter(fun({_,Props}) ->
                                        proplists:get_value(is_editable, Props) =:= true
                                        orelse proplists:get_value(is_enabled, Props) =:= true
                                     end,
                                     get_language_config(Context)),
            z_memo:set('mod_translation$enabled_languages', Languages)
    end.

%% @doc Get the list of languages
get_language_config(Context) ->
    case m_config:get(i18n, language_list, Context) of
        undefined -> [];
        LanguageConfig -> proplists:get_value(list, LanguageConfig, [])
    end.
