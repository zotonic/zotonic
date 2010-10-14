%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2010-03-31
%% @doc Map zotonic "Query search-model" syntax onto solr's. See http://zotonic.com/documentation/761/the-query-search-model
-module(solr_search).

-include("zotonic.hrl").

-export([
         search/4,
         match/4,
         list_to_proplist/1
        ]).

%% @doc Execute a query on the solr instance.
search(Query, {Offset, PageLen}, Solr, Context) ->
    {Q, SearchOptions} = map_search(Query, Context),
    case Q of
        [] ->
            #search_result{result=[]};
        _ ->
            {ok, RespAttrs, Docs, Info} = esolr:search(Q, [{fields, "id"}, {start, Offset-1}, {rows, PageLen} | SearchOptions], Solr),

            %% Get the ids
            Ids = [Id || {doc, [{"id", Id}]} <- Docs],

            %% Map the extended info into a handy format
            Info1 = filter_empty(lists:flatten([map_info(I) || I <- Info])),

            %% Decide on result format
            Result = case Info1 of
                         [] ->
                             %% No extended info was returned. The result array is just a list of ids.
                             Ids;
                         _ ->
                             %% If extended info was returned, the document
                             %% ids are in the ".ids" subproperty, alongside
                             %% the other info (e.g. facetting, highlighting)
                             [{ids, Ids} | Info1]
                     end,

            %% Compute paging counters
            {"numFound", Total} = proplists:lookup("numFound", RespAttrs),
            Pages = mochinum:int_ceil(Total / PageLen),
            Page = mochinum:int_ceil(Offset / PageLen),
            Next = if Offset + PageLen < Total -> false; true -> Page+1 end,
            Prev = if Page > 1 -> Page-1; true -> 1 end,

            Result1 = case proplists:get_value(return_format, Query) of
                          ranked ->
                              %% FIXME how to retrieve the ranked value from solr?
                              [{Id, 1.0} || Id <- Result]; 
                          _ ->
                              Result
                      end,

            %% Construct the final search result
            #search_result{result=Result1, 
                           total=Total,
                           pages=Pages,
                           page=Page,
                           next=Next,
                           prev=Prev
                          }
    end.


%% @doc Given an rsc id, construct a list of matching rsc records. Uses Solrs "MoreLikeThis" feature.
match(Id, {_, Amount}, Solr, Context) ->
    Result = search([{morelikethis_id, [Id, Amount]}], {1,1}, Solr, Context),
    IdS = z_convert:to_list(Id),

    %% "More like this id" gives us a search result with 1 doc, but
    %% the interesting stuff is in the 'morelikethis' property of the
    %% search.
    Props = case proplists:get_value(morelikethis, Result#search_result.result) of
                {obj, []} ->
                    [{"numFound", "0"}, {"docs", []}];
                {obj, [{IdS, {obj, Props0}}]} ->
                    Props0
            end,

    %% Extract the matching ids and the total number found
    Ids = [DocId || {obj, [{"id", DocId}]} <- proplists:get_value("docs", Props)],
    {"numFound", Total} = proplists:lookup("numFound", Props),

    %% Construct the final result
    #search_result{result=Ids,
                   total=Total}.


%% @doc Map search results into terms / lucene syntax that solr and esolr understand.
map_search(Query, Context) ->
    Query1 = filter_empty(Query),
    L = [map_search_field(Part, Context) || Part <- lists:reverse(Query1)],
    {SolrQuery, SearchOptions} = lists:unzip(L),
    SolrQuery1 = lists:foldl(fun(X, Acc) -> [" ", X|Acc] end, [], SolrQuery),
    {lists:flatten(SolrQuery1), lists:flatten(SearchOptions)}.

%% cat=categoryname
%% Filter results on a certain category.
map_search_field({cat, C}, _Context) when C =:= [] orelse C =:= [[]]->
    {[], []};
map_search_field({cat, [Cats]}, _Context) when is_list(Cats) ->
    map_search_field({cat, Cats}, _Context);
map_search_field({cat, Cats}, _Context) when is_list(Cats) ->
    case z_string:is_string(Cats) of
        true ->
            {["+category:", as_category(Cats)], []};
        false ->
            case lists:filter(fun(X) -> not(empty_term(X)) end, Cats) of
                [] -> {[], []};
                Cats1 ->
                    {["+(category:", string:join([as_category(C) || C <- Cats1], ", category:"), ")"], []}
            end
    end;

%% cat=category1,category2
%% Filter results on a certain category.
map_search_field({cat, Cat}, _Context) ->
    ?DEBUG(Cat),
    {["+category:", as_category(Cat)],
     []};

%% cat_exclude=categoryname
%% Filter results outside a certain category.
map_search_field({cat_exclude, Cat}, _Context) ->
    {["-category:", as_category(Cat)],
     []};

%% hasobject=[id]
%% Give all things which have an outgoing edge to Id
map_search_field({hasobject, Id}, _Context) when is_integer(Id) ->
    map_search_field({hasobject, [Id]}, _Context);
map_search_field({hasobject, [Id]}, _Context) ->
    {["+o:", z_convert:to_list(Id)],
     []};

%% hasobject=[id,predicate]
%% Give all things which have an outgoing edge to Id with the given predicate
map_search_field({hasobject, [Id, Predicate]}, _Context) ->
    {["+o_", z_convert:to_list(Predicate), ":", z_convert:to_list(Id)],
     []};
map_search_field({hasobject, L}, _Context) when is_list(L) ->
    map_search_field({hasobject, [z_convert:to_integer(L)]}, _Context);

%% hassubject=[id]
%% Give all things which have an outgoing edge to Id
map_search_field({hassubject, Id}, _Context) when is_integer(Id) ->
    map_search_field({hassubject, [Id]}, _Context);
map_search_field({hassubject, [Id]}, _Context) ->
    {["+s:", z_convert:to_list(Id)],
     []};

%% hassubject=[id,predicate,[alias]]
%% Give all things which have an outgoing edge to Id with the given predicate
map_search_field({hassubject, [Id, Predicate]}, _Context) ->
    {["+s_", z_convert:to_list(Predicate), ":", z_convert:to_list(Id)],
     []};
map_search_field({hassubject, L}, _Context) when is_list(L) ->
    map_search_field({hassubject, [z_convert:to_integer(L)]}, _Context);

%% hasobjectpredicate=predicate
%% Give all things which have any outgoing edge with given predicate
map_search_field({hasobjectpredicate, Predicate}, _Context) ->
    {["+o_", z_convert:to_list(Predicate), ":[* TO *]"], []};

%% hassubjectpredicate=predicate
%% Give all things which have any incoming edge with given predicate
map_search_field({hassubjectpredicate, Predicate}, _Context) ->
    {["+s_", z_convert:to_list(Predicate), ":[* TO *]"], []};

%% is_featured or is_featured={false,true}
%% Filter on whether an item is featured or not.
map_search_field({is_featured, Bool}, _Context) ->
    {["+is_featured:", atom_to_list(z_convert:to_bool(Bool))],
     []};

%% upcoming
%% Filter on items whose end date lies in the future
map_search_field({upcoming, Boolean}, _Context) ->
    case z_convert:to_bool(Boolean) of
        true -> {["+date_start:[",
                  z_convert:to_isotime(erlang:localtime()), " TO *]"],
                 []};
        false -> {[], []}
    end;

%% authoritative={true|false}
%% Filter on items which are authoritative or not
map_search_field({authoritative, Bool}, _Context) ->
    {["+is_authoritative:", atom_to_list(z_convert:to_bool(Bool))],
     []};

%% query_id=<rsc id>
%% Get the query terms from given resource ID, and use those terms.
map_search_field({query_id, QueryId}, Context) ->
    Id = z_convert:to_list(QueryId),
    case m_category:is_a(m_rsc:p(Id, category_id, Context), 'query', Context) of
        true ->
            Q = z_convert:to_list(m_rsc:p(Id, 'query', Context)),
            map_search(search_query:parse_query_text(Q), Context);
        false ->
            throw({error, {invalid_query_id, Id}})
    end;

%% text=...
%% Perform a fulltext search
map_search_field({text, Text}, _Context) when Text =:= [] orelse Text =:= undefined orelse Text =:= <<>> ->
    {[], []};
map_search_field({text, Text}, _Context) ->
    {["+(", z_convert:to_list(Text),")"],
     []};

%% sort=..
%% Sort the result.
map_search_field({sort, Sort}, _Context) ->
    [FirstChar|F1] = z_convert:to_list(Sort),
    {Field, Order} = case FirstChar of
                         $- -> {F1, desc};
                         $+ -> {F1, asc};
                         _ -> {z_convert:to_list(Sort), asc}
                     end,
    {[],
     [{sort, [{Field, Order}]}]
    };

%%
%% Solr-specific search options start here.
%%

%% solrfield=field:value
map_search_field({solr_field, [_, Value]}, _Context)
  when Value =:= undefined orelse Value =:= [] ->
    {[], []};
map_search_field({solr_field, [Field, Value]}, _Context) ->
    {["+", z_convert:to_list(Field), ":", z_convert:to_list(Value)],
     []};

%% highlight=<field>
%% Highlight a particular field (solr only)
map_search_field({highlight, Field}, _Context) ->
    {[],
     [{highlight, z_convert:to_list(Field)}]};


%% facet=<field>
%% Facet on a particular field (solr only)
map_search_field({facet, Field}, _Context) ->
    {[],
     [{facet, z_convert:to_list(Field)}]};


%% morelikethis=Id
%% Give more documents which are similar to <Id>  (solr only)
map_search_field({morelikethis_id, [Id, Amount]}, _Context) ->
    {["id:", z_convert:to_list(Id), " -category:meta"],
     [{morelikethis, "o,match_title,summary,category", Amount}]};

%% Specifying the return format. Either 'default' (empty) or 'ranked'.
map_search_field({return_format, _}, _Context) ->
    {[], []}.



%%

%% Extended info results mapping
map_info({"highlighting", Dict}) ->
    {highlight, Dict};
map_info({"facet_counts", {obj, Dict}}) ->
    {"facet_fields", {obj, F}} = proplists:lookup("facet_fields", Dict),
    {facet_fields, {obj, [{K, list_to_proplist(V)} || {K,V} <- F]}};
map_info({"moreLikeThis", Dict}) ->
    {morelikethis, Dict};
map_info(_X) ->
    ?DEBUG("Unknown extended-info returned from solr!"),
    ?DEBUG(_X),
    {undefined, undefined}.



%% Helper functions
filter_empty(Q) ->
    lists:filter(fun({_, X}) -> not(empty_term(X)) end, Q).

empty_term(X) when X =:= [] orelse X =:= undefined orelse X =:= <<>> -> true;
empty_term([X, _]) ->
    empty_term(X);
empty_term(_) ->
    false.



%% @spec list_to_proplist([key, val, key2, val2]) -> [{key, val}, {key2, val2}].
list_to_proplist(L) ->
    {obj, list_to_proplist(L, [])}.
list_to_proplist([], Acc) ->
    lists:reverse(Acc);
list_to_proplist([K,V|Rest], Acc) ->
    list_to_proplist(Rest, [{z_convert:to_list(K),V}|Acc]).

as_category({Id}) -> as_category(Id);
as_category(Id) when is_integer(Id) ->
    integer_to_list(Id);
as_category(Cat) -> z_convert:to_list(Cat).
