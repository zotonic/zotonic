%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2010-03-31
%% @doc Store Zotonic rsc documents in Solr.
-module(solr_store).

-export([put/3]).

-include("zotonic.hrl").


put(Id, Context, Solr) ->
    case convert(Id, Context) of
        undefined ->
            nop;
        Doc ->
            Doc1 = lists:filter(fun({_,undefined})->false; (_)->true end, Doc),
            esolr:add([{doc, Doc1}], Solr)
    end.

%% See schema.xml to see which fields need to be put into Solr.
convert(Id, Context) ->
    All = m_rsc:get(Id, Context),

    StrVal = fun(Name) -> z_convert:to_list(first_trans(proplists:get_value(Name, All))) end,
    Bool = fun(Name) -> case proplists:get_value(Name, All) of
                            true -> "1";
                            false -> "0"
                        end
           end,
    Date = fun(Name) -> case proplists:get_value(Name, All) of
                            undefined -> undefined;
                            ?ST_JUTTEMIS -> "9999-08-17T12:00:00Z";
                            D -> z_convert:to_isotime(D)
                        end
           end,

    %% Gather all text in the rsc, using the pivot routines.
    {_ObjIds, _CatIds, [TA,TB,TC,TD]} = z_pivot_rsc:get_pivot_data(Id, Context),
    AllText = lists:flatten( 
                [
                 [ z_convert:to_list(X) || {_, X} <- TA ], " ",
                 [ z_convert:to_list(X) || {_, X} <- TB ], " ",
                 [ z_convert:to_list(X) || {_, X} <- TC ], " ",
                 [ z_convert:to_list(X) || {_, X} <- TD ]]),

    %% The returned document.
    []
        ++

        %% Regular rsc fields
        [{F, StrVal(F)} || F <- [id, version, uri, name, page_path]]
        ++
        [{F, Bool(F)} || F <- [is_authoritative, is_published, is_featured, is_protected]]
        ++
        [{F, Date(F)} || F <- [publication_start, publication_end, date_start, date_end]]
        ++
        
        %% rsc category
        [{category, C} || C <- categories(m_rsc:is_a(Id, Context))]
        ++
        
        %% Text fields
        [{F, StrVal(F)} || F <- [title, summary, body]]
        ++

        %% All combined text
        [{text, AllText}]
        ++

        %% Some more fields
        [{F, StrVal(F)} || F <- [first_name, surname, gender, street,
                                city, postcode, state, country, geocode]]
        .


%% @doc categories([foo,bar,baz]) -> ["foo", "foo/bar", "foo/bar/baz"]
categories(IsA) ->
    categories(lists:reverse(IsA), []).

categories([], Acc) ->
     Acc;
categories(L=[_|Cats], Acc) ->
    New = string:join([z_convert:to_list(X)||X<-lists:reverse(L)],"/"),
    categories(Cats, [New|Acc]).


%% @doc Get the first translation from a text for use in a string field.
first_trans({trans, []}) ->
    "";
first_trans({trans, [{_, Text}|_]}) ->
    Text;
first_trans(X) ->
    X.

