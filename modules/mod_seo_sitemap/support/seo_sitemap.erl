-module(seo_sitemap).

-include_lib("zotonic.hrl").
-include("../include/seo_sitemap.hrl").

-export([
    urlsets/1,
    urlset/4,
    categories/1
    ]).


%% @doc Split the urlsets into sitemap files, depending on the url count in the urlsets.
-spec urlsets( z:context() ) -> list( map() ).
urlsets(Context) ->
    Sets = lists:flatten( z_notifier:map(#seo_sitemap_index{}, Context) ),
    Sets1 = lists:filter( fun erlang:is_map/1, Sets ),
    Fs = lists:foldl(
        fun(#{ source := Source, count := Count } = Set, Acc) ->
            PageSize = maps:get(size, Set, ?URLSET_SIZE),
            Parts = partition(Count, PageSize),
            Files = lists:map(
                fun(#{ from := PFrom, count := PCt }) ->
                    Loc = z_dispatcher:url_for(
                        sitemap_urlset_xml,
                        [
                            {s, Source},
                            {o, PFrom},
                            {c, PCt}
                        ],
                        Context),
                    Loc1 = z_html:unescape(Loc),
                    #{
                        loc => z_context:abs_url(Loc1, Context),
                        source => Source,
                        from => PFrom,
                        count => PCt,
                        lastmod => maps:get(lastmod, Set, undefined)
                    }
                end,
                Parts),
            [ Files | Acc ]
        end,
        [],
        Sets1),
    lists:reverse( lists:flatten(Fs) ).

%% @doc Fetch all urls in an urlset.
-spec urlset( binary(), pos_integer(), pos_integer(), z:context() ) -> list( map() ).
urlset(Source, Offset, Limit, Context) ->
    case z_notifier:first(#seo_sitemap_urlset{
            source = Source,
            offset = Offset,
            limit = Limit
        }, Context)
    of
        undefined -> [];
        Urls when is_list(Urls) -> Urls
    end.

%% @doc Return the categories to be added to the sitemap, ordered by seo_sitemap_priority
-spec categories( z:context() ) -> list( m_rsc:resource_id() ).
categories(Context) ->
    CatCat = m_rsc:rid(category, Context),
    Cats = z_db:q("select id from rsc where category_id = $1", [ CatCat ], Context),
    PIds = lists:filtermap(
        fun({Id}) ->
            case z_acl:rsc_visible(Id, Context) of
                true ->
                    Prio = case m_rsc:p(Id, seo_sitemap_priority, Context) of
                        undefined -> 0.5;
                        <<>> -> 0.5;
                        P ->
                            try
                                z_convert:to_float(P)
                            catch
                                _:_ -> 0.5
                            end
                    end,
                    case Prio > 0 of
                        true -> {true, {Prio, Id}};
                        false -> false
                    end;
                false ->
                    false
            end
        end,
        Cats),
    Sorted = lists:reverse( lists:sort(PIds) ),
    [ Id || {_, Id} <- Sorted ].


%% @doc Partition an URL set into smaller sets. We keep it smaller than the
%% max size of 50.000 files so that generating the file doesn't use to many resources.
partition(Count, PageSize) ->
    partition(1, Count, PageSize, []).

partition(Start, Count, _PageSize, Acc) when Start >= Count ->
    lists:reverse(Acc);
partition(Start, Count, PageSize, Acc) ->
    P = #{
        from => Start,
        count => PageSize
    },
    partition(Start + PageSize, Count, PageSize, [ P | Acc ]).


