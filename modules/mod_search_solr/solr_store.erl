%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2010-03-31
%% @doc Store Zotonic rsc documents in Solr.
-module(solr_store).

-export([put/3, delete/3]).

-include("zotonic.hrl").


put(Id, Context, Solr) ->
    case convert(Id, Context) of
        undefined ->
            nop;
        Doc ->
            Doc1 = lists:filter(fun({_,undefined})->false; ({_,[]})->false; (_)->true end, Doc),
            %%?DEBUG(Doc1),
            esolr:add([{doc, Doc1}], Solr)
    end.


delete(Id, _Context, Solr) ->
    ok = esolr:delete({id, z_convert:to_list(Id)}, Solr).



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

    IsA = m_rsc:is_a(Id, Context),

    %% Module-based fields (starting with x_)
    ModuleProps = lists:flatten(z_notifier:foldl({solr_props, Id, IsA}, [], Context)),

    %% The returned document.
    []
        ++

        %% Regular rsc fields
        [{F, StrVal(F)} || F <- [id, version, uri, name, page_path, category_id, modifier_id, creator_id]]
        ++
        [{F, Bool(F)} || F <- [is_authoritative, is_published, is_featured, is_protected]]
        ++
        [{F, Date(F)} || F <- [modified, created, publication_start, publication_end, date_start, date_end]]
        ++

        %% rsc category name
        [{category, z_convert:to_list(C)} || C <- IsA]
        ++
        %% rsc category id
        [{category, z_convert:to_list(m_rsc:name_to_id_check(C, Context))} || C <- IsA]
        ++

        %% Text fields
        [{pivot_title, StrVal(title)}]
        ++
        [{F, StrVal(F)} || F <- [title, summary, body]]
        ++

        %% All combined text
        [{text, AllText}]
        ++

        %% Some more fields
        [{F, StrVal(F)} || F <- [first_name, surname, gender, street,
                                city, postcode, state, country, geocode]]
        ++

        %% Edges
        [{o, z_convert:to_list(C)} || C <- m_edge:objects(Id, Context)]
        ++
        [{s, z_convert:to_list(C)} || C <- m_edge:subjects(Id, Context)]
        ++
        lists:flatten(
          [[{list_to_atom("s_" ++ atom_to_list(Pred)), z_convert:to_list(Edg)}
            || Edg <- m_edge:subjects(Id, Pred, Context)]
           || Pred <- m_edge:subject_predicates(Id, Context)])
        ++
        lists:flatten(
          [[{list_to_atom("o_" ++ atom_to_list(Pred)), z_convert:to_list(Edg)}
           || Edg <- m_edge:objects(Id, Pred, Context)]
          || Pred <- m_edge:object_predicates(Id, Context)])
        ++

        %% Module-based solr_props
        ModuleProps
        .


%% @doc Get the first translation from a text for use in a string field.
first_trans({trans, []}) ->
    "";
first_trans({trans, [{_, Text}|_]}) ->
    Text;
first_trans(X) ->
    X.

