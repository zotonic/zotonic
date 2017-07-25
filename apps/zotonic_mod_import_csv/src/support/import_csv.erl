%% @doc Import a csv file according to the derived file/record definitions.
%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @author Marc Worrell <marc@worrell.nl>

%% Copyright 2010-2015 Marc Worrell, Arjan Scherpenisse
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


-module(import_csv).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    import/3
]).

-export([
    sort_props/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("include/import_csv.hrl").

-record(importresult, {seen=[], new=[], updated=[], errors=[], ignored=[], deleted=0}).
-record(importstate, {is_reset, name_to_id, managed_edges, to_flush=[], result, managed_resources}).


%% @doc The import function, read the csv file and fetch all records.
%% This function is run as a spawned process.  The Context should have the right permissions for inserting
%% or updating the resources.
-spec import(#filedef{}, boolean(), #context{}) -> list().
import(Def, IsReset, Context) ->
    StartDate = erlang:universaltime(),

    %% Read and parse all rows
    {ok, Device} = file:open(Def#filedef.filename, [read, binary, {encoding, utf8}]),
    Rows = parse_csv:scan_lines(Device, Def#filedef.colsep),
    file:close(Device),
    file:delete(Def#filedef.filename),

    %% Drop (optionally) the first row, empty rows and the comment rows (starting with a '#')
    Rows1 = case Def#filedef.skip_first_row of
                true -> tl(Rows);
                _ -> Rows
            end,
    State = import_rows(Rows1, 1, Def, new_importstate(IsReset), Context),

    %% Return the stats from this import run
    R = State#importstate.result,
    [ {file, filename:basename(Def#filedef.filename)},
      {date_start, StartDate},
      {date_end, erlang:universaltime()},
      {seen, R#importresult.seen},
      {new,  R#importresult.new},
      {updated, R#importresult.updated},
      {errors, R#importresult.errors},
      {deleted, 0},
      {ignored, R#importresult.ignored}].


new_importstate(IsReset) ->
    #importstate{
        is_reset=IsReset,
        name_to_id=gb_trees:empty(),
        managed_edges=gb_trees:empty(),
        managed_resources=sets:new(),
        result=#importresult{}
    }.

%%====================================================================
%% Import a record
%%====================================================================

%% @doc Import all rows.
import_rows([], _RowNr, _Def, ImportState, _Context) ->
    ImportState;
import_rows([[<<$#, _/binary>>|_]|Rows], RowNr, Def, ImportState, Context) ->
    import_rows(Rows, RowNr+1, Def, ImportState, Context);
import_rows([[]|Rows], RowNr, Def, ImportState, Context) ->
    import_rows(Rows, RowNr+1, Def, ImportState, Context);
import_rows([R|Rows], RowNr, Def, ImportState, Context) ->
    Zipped = zip(R, Def#filedef.columns, []),
    ImportState1 = import_parts(Zipped, RowNr, Def#filedef.importdef, ImportState, Context),
    import_rows(Rows, RowNr+1, Def, ImportState1, Context).


%% @doc Combine the field name definitions and the field values.
zip(_Cols, [], Acc) -> lists:reverse(Acc);
zip([_C|Cs], [''|Ns], Acc) -> zip(Cs, Ns, Acc);
zip([_C|Cs], [""|Ns], Acc) -> zip(Cs, Ns, Acc);
zip([_C|Cs], [undefined|Ns], Acc) -> zip(Cs, Ns, Acc);
zip([], [N|Ns], Acc) -> zip([], Ns, [{z_convert:to_list(N),<<>>}|Acc]);
zip([C|Cs], [N|Ns], Acc) -> zip(Cs, Ns, [{z_convert:to_list(N), C}|Acc]).


%% @doc Import all resources on a row
import_parts(_Row, _RowNr, [], ImportState, _Context) ->
    ImportState;
import_parts(Row, RowNr, [Def | Definitions], ImportState, Context) ->
    {FieldMapping, ConnectionMapping} = Def,
    try
        case import_def_rsc(FieldMapping, Row, ImportState, Context) of
            {S, ignore} ->
                import_parts(Row, RowNr, Definitions, S, Context);
            {S, {error, Type, E}} ->
                add_result_seen(Type, add_result_error(Type, E, S));

            {S, {new, Type, Id, Name}} ->
                State0 = add_managed_resource(Id, FieldMapping, S),
                State1 = add_name_lookup(State0, Name, Id),
                State2 = import_parts(Row, RowNr, Definitions, State1, Context),
                import_def_edges(Id, ConnectionMapping, Row, State2, Context),
                add_result_seen(Type, add_result_new(Type, State2));

            {S, {equal, Type, Id}} ->
                State0 = add_managed_resource(Id, FieldMapping, S),
                State1 = import_parts(Row, RowNr, Definitions, State0, Context),
                import_def_edges(Id, ConnectionMapping, Row, State1, Context),
                add_result_seen(Type, add_result_ignored(Type, State1));

            {S, {updated, Type, Id}} ->
                %% Equal
                State0 = add_managed_resource(Id, FieldMapping, S),
                State1 = import_parts(Row, RowNr, Definitions, State0, Context),
                add_result_seen(Type, import_def_edges(Id, ConnectionMapping, Row, State1, Context)),
                add_result_updated(Type, State1)
        end
    catch
        throw:{import_error, ImportError} ->
            lager:error("[import_csv] Error importing row #~p, error: ~p", [RowNr, ImportError]),
            lager:error("[import_csv] Row #~p was: ~p", [RowNr, Row]),
            lager:error("[import_csv] Row #~p import definition: ~p", [RowNr, Def]),
            ImportState
    end.


import_def_rsc(FieldMapping, Row, State, Context) ->
    {Callbacks, FieldMapping1} = case proplists:get_value(callbacks, FieldMapping) of
                                     undefined -> {[], FieldMapping};
                                     CB -> {CB, proplists:delete(callbacks, FieldMapping)}
                                 end,
    RowMapped = map_fields(FieldMapping1, Row, State),
    import_def_rsc_1_cat(RowMapped, Callbacks, State, Context).

import_def_rsc_1_cat(Row, Callbacks, State, Context) ->
    %% Get category name; put category ID in the record.
    {"category", CategoryName} = proplists:lookup("category", Row),
    {CatId, State1} = name_lookup_exists(CategoryName, State, Context),
    Row1 = [{"category_id", CatId} | proplists:delete("category", Row)],
    Name = case proplists:get_value("name", Row1) of
               undefined -> throw({import_error, {definition_without_unique_name}});
               N -> N
           end,
    {OptRscId, State2} = name_lookup(Name, State1, Context),
    RscId = case OptRscId of undefined -> insert_rsc; _ -> OptRscId end,
    NormalizedRow = sort_props(m_rsc_update:normalize_props(RscId, Row1, [is_import], Context)),
    case has_required_rsc_props(NormalizedRow) of
        true ->
           import_def_rsc_2_name(RscId, State2, Name, CategoryName, NormalizedRow, Callbacks, Context);
        false ->
            lager:info("import_csv: missing required attributes for ~p", [Name]),
            {State2, ignore}
    end.

import_def_rsc_2_name(insert_rsc, State, Name, CategoryName, NormalizedRow, Callbacks, Context) ->
    lager:debug("import_csv: importing ~p", [Name]),
    case rsc_insert(NormalizedRow, Context) of
        {ok, NewId} ->
            RawRscFinal = get_updated_props(NewId, NormalizedRow, Context),
            Checksum = checksum(NormalizedRow),
            m_import_csv_data:update(NewId, Checksum, NormalizedRow, RawRscFinal, Context),

            case proplists:get_value(rsc_insert, Callbacks) of
                undefined -> none;
                Callback -> Callback(NewId, NormalizedRow, Context)
            end,
            {flush_add(NewId, State), {new, CategoryName, NewId, Name}};
        {error, _} = E ->
            lager:warning("import_csv: could not insert ~p: ~p", [Name, E]),
            {State, {error, CategoryName, E}}
    end;
import_def_rsc_2_name(Id, State, Name, CategoryName, NormalizedRow, Callbacks, Context) when is_integer(Id) ->
    % 1. Check if this update was the same as the last known import
    PrevImportData = m_import_csv_data:get(Id, Context),
    PrevChecksum = get_value(checksum, PrevImportData),
    case checksum(NormalizedRow) of
        PrevChecksum when not State#importstate.is_reset ->
            lager:info("import_csv: skipping ~p (importing same values)", [Name]),
            {State, {equal, CategoryName, Id}};
        Checksum ->
            lager:info("import_csv: updating ~p", [Name]),

            % 2. Some properties might have been overwritten by an editor.
            %    For this we will update a second time with all the changed values
            %    (also pass any import-data from an older import module)
            RawRscPre = get_updated_props(Id, NormalizedRow, Context),
            Edited = diff_raw_props(RawRscPre,
                                    get_value(rsc_data, PrevImportData, []),
                                    m_rsc:p_no_acl(Id, import_csv_original, Context)),

            % Cleanup old import_csv data on update
            Row1 = [ {import_csv_original, undefined}, {import_csv_touched, undefined} | NormalizedRow ],
            case rsc_update(Id, Row1, Context) of
                {ok, _} ->
                    RawRscFinal = get_updated_props(Id, NormalizedRow, Context),
                    % Ensure edited properties are set back to their edited values
                    case Edited of
                        [] ->
                            ok;
                        _ when State#importstate.is_reset ->
                            ok;
                        _ ->
                            {ok, _} = m_rsc_update:update(Id, Edited, [{is_import, true}], Context)
                    end,
                    m_import_csv_data:update(Id, Checksum, NormalizedRow, RawRscFinal, Context),

                    case proplists:get_value(rsc_update, Callbacks) of
                        undefined -> none;
                        Callback -> Callback(Id, NormalizedRow, Context)
                    end,
                    {flush_add(Id, State), {updated, CategoryName, Id}};
                {error, _} = E ->
                    {State, {error, CategoryName, E}}
            end
    end.


%% @doc Check which properties are changed
diff_raw_props(Current, [], {props, OriginalProps}) ->
    % Old version of the csv import, do our best on the diff
    diff_raw_props(Current, sort_props(OriginalProps), undefined);
diff_raw_props(LastImport, LastImport, undefined) ->
    [];
diff_raw_props(Current, LastImport, undefined) ->
    % Return the fields that are changed in Current but that were
    % updated in the last import.
    % Bit primitive for now ....
    case LastImport -- Current of
        [] ->
            [];
        Diff ->
            Keys = [ K || {K, _} <- Diff ],
            [ {K, proplists:get_value(K, Current)} || K <- Keys ]
    end.

get_updated_props(Id, Row, Context) ->
    Raw = m_rsc:get_raw(Id, Context),
    sort_props([ {K, proplists:get_value(K, Raw)} || {K, _} <- Row ]).

sort_props(Props) ->
    Props1 = lists:sort(Props),
    [ sort_props_1(P) || P <- Props1 ].

sort_props_1({trans, Tr}) ->
    {trans, lists:sort(Tr)};
sort_props_1({K, [A|_] = L}) when is_list(A) ->
    L1 = [ sort_props(V) || V <- L ],
    {K, L1};
sort_props_1({K, [{A,_}|_] = L}) when is_atom(A) ->
    {K, sort_props(L)};
sort_props_1(V) ->
    V.

rsc_insert(Props, Context) ->
    case check_medium(Props) of
        {url, Url, PropsMedia} ->
            m_media:insert_url(Url, PropsMedia, [{is_import, true}], Context);
        none ->
            m_rsc_update:insert(Props, [{is_import, true}], Context)
    end.

rsc_update(Id, Props, Context) ->
    case check_medium(Props) of
        {url, Url, PropsMedia} ->
            m_media:replace_url(Url, Id, PropsMedia, [{is_import, true}], Context);
        none ->
            m_rsc_update:update(Id, Props, [{is_import, true}], Context)
    end.

check_medium(Props) ->
    case proplists:get_value(medium_url, Props) of
        undefined -> none;
        <<>> -> none;
        [] -> none;
        Url -> {url, Url, proplists:delete(medium_url, Props)}
    end.

get_value(K, L) ->
    get_value(K, L, undefined).

get_value(K, L, D) when is_list(L) ->
    proplists:get_value(K, L, D);
get_value(_K, undefined, D) ->
    D.

import_def_edges({error, _}, _, _, State, _Context) ->
    State;
import_def_edges(Id, ConnectionMapping, Row, State, Context) ->
    %% @todo Use a mapfoldl here to return the new state with name lookups from the edges
    EdgeIds = lists:flatten([import_do_edge(Id, Row, Map, State, Context) || Map <- ConnectionMapping]),
    case lists:filter(fun(X) -> not(X == fail) end, EdgeIds) of
        [] -> State;
        NewEdgeIds -> managed_edge_add(Id, NewEdgeIds, State)
    end.

import_do_edge(Id, Row, F, State, Context) when is_function(F) ->
    [ import_do_edge(Id, Row, E, State, Context) || E <- F(Id, Row, State, Context) ];
import_do_edge(Id, Row, {{PredCat, PredRowField}, ObjectDefinition}, State, Context) ->
    % Find the predicate
    case map_one_normalize(name, PredCat, map_one(PredRowField, Row, State)) of
        <<>> ->
            fail;
        Name ->
            case name_lookup(Name, State, Context) of
                {undefined, _State1} ->
                    lager:warning("Import CSV: ddge predicate does not exist: '~p'", [Name]),
                    fail;
                {PredId, State1} ->
                    import_do_edge(Id, Row, {PredId, ObjectDefinition}, State1, Context)
            end
    end;
import_do_edge(Id, Row, {Predicate, {ObjectCat, ObjectRowField}}, State, Context) ->
    % Find the object
    Name = map_one_normalize(name, ObjectCat, map_one(ObjectRowField, Row, State)),
    case Name of
        <<>> -> fail;
        Name ->
            case name_lookup(Name, State, Context) of
                %% Object doesn't exist
                {undefined, _State1} ->
                    fail;
                %% Object exists
                {RscId, _State1} ->
                    {ok, EdgeId} = m_edge:insert(Id, Predicate, RscId, Context),
                    EdgeId
            end
    end;
import_do_edge(Id, Row, {Predicate, {ObjectCat, ObjectRowField, ObjectProps}}, State, Context) ->
    Name = map_one_normalize(name, ObjectCat, map_one(ObjectRowField, Row, State)),
    case Name of
        <<>> ->
            fail;
        Name ->
            case name_lookup(Name, State, Context) of
                %% Object doesn't exist, create it using the objectprops
                {undefined, _State1} ->
                    lager:debug("Import CSV: creating object: ~p", [[{category, ObjectCat}, {name, Name} | ObjectProps]]),
                    case m_rsc:insert([{category, ObjectCat}, {name, Name} | ObjectProps], Context) of
                        {ok, RscId} ->
                            {ok, EdgeId} = m_edge:insert(Id, Predicate, RscId, Context),
                            EdgeId;
                        {error, _Reason} ->
                            fail
                    end;
                %% Object exists
                {RscId, _State1} ->
                    {ok, EdgeId} = m_edge:insert(Id, Predicate, RscId, Context),
                    EdgeId
            end
    end;
import_do_edge(_, _, Def, _State, _Context) ->
    throw({import_error, {invalid_edge_definition, Def}}).


%% Adds a resource Id to the list of managed resources, if the import definition allows it.
add_managed_resource(Id, Props, State=#importstate{managed_resources=M}) ->
    case proplists:get_value(import_skip_delete, Props) of
        true -> State;
        _ -> State#importstate{managed_resources=sets:add_element(Id, M)}
    end.

add_name_lookup(State=#importstate{name_to_id=Tree}, Name, Id) ->
    case gb_trees:lookup(Name, Tree) of
        {value, undefined} -> State#importstate{name_to_id=gb_trees:update(Name, Id, Tree)};
        {value, _} -> State;
        none -> State#importstate{name_to_id=gb_trees:insert(Name, Id, Tree)}
    end.

name_lookup(Name, #importstate{name_to_id=Tree} = State, Context) ->
    case gb_trees:lookup(Name, Tree) of
        {value, V} ->
            {V, State};
        none ->
            V = m_rsc:rid(Name, Context),
            {V, add_name_lookup(State, Name, V)}
    end.

name_lookup_exists(Name, State, Context) ->
    case name_lookup(Name, State, Context) of
        {undefined, _State1} -> throw({import_error, {unknown_resource_name, Name}});
        {Id, State1} -> {Id, State1}
    end.


%% @doc Maps fields from the given mapping into a new row, filtering out undefined values.
map_fields(Mapping, Row, State) ->
    Defaults = [
        {"is_protected", true},
        {"is_published", true}
    ],
    Mapped = [map_def(MapOne, Row, State) || MapOne <- Mapping],
    Type = case proplists:get_value("category", Mapped) of
               undefined -> throw({import_error, no_category_in_import_definition});
               T -> T
           end,
	% Normalize and remove undefined values
    P = lists:filter(fun({_K,undefined}) -> false; (_) -> true end,
					 [{K, map_one_normalize(K, Type, V)} || {K,V} <- Mapped]),
    add_defaults(Defaults, P).

map_def({K,F}, Row, State) ->
	{K, map_one(F, Row, State)};
map_def(K, Row, State) when is_atom(K); is_list(K) ->
	map_def({K,K}, Row, State).


map_one_normalize("name", _Type, <<>>) ->
    <<>>;
map_one_normalize("name", _Type, {name_prefix, Prefix, V}) ->
    CheckL = 80 - strlen(Prefix) - 1,
    Name = case strlen(V) of
               L when L > CheckL ->
                   % If name is too long, make a unique thing out of it.
                   z_string:to_name(z_convert:to_list(Prefix) ++ "_" ++ base64:encode_to_string(checksum(V)));
               _ ->
                   z_string:to_name(z_convert:to_list(Prefix) ++ "_" ++ z_string:to_name(V))
           end,
    z_convert:to_binary(Name);
map_one_normalize(_, _Type, V) ->
    V.


strlen(B) when is_binary(B) -> erlang:size(B);
strlen(L) when is_list(L) -> erlang:length(L);
strlen(A) when is_atom(A) -> erlang:length(atom_to_list(A)).

map_one(F, _Row, _State) when is_binary(F) -> F;
map_one(undefined, _Row, _State) -> undefined;
map_one(true, _Row, _State) -> true;
map_one(false, _Row, _State) -> false;
map_one(F, Row, _State) when is_atom(F) ->
    concat_spaces(proplists:get_all_values(z_convert:to_list(F), Row));
map_one(F, Row, _State) when is_list(F) ->
    concat_spaces(proplists:get_all_values(F, Row));
map_one(F, Row, _State) when is_function(F) ->
    F(Row);
map_one({prefer, Fields}, Row, State) ->
    map_one_prefer(Fields, Row, State);
map_one({html_escape, Value}, Row, State) ->
    z_html:escape(map_one(Value, Row, State));
map_one({concat, Fields}, Row, State) ->
    map_one_concat(Fields, Row, State);
map_one({surroundspace, Field}, Row, State) ->
    case z_string:trim(map_one(Field, Row, State)) of
        <<>> ->  <<" ">>;
        Val -> <<32, Val/binary, 32>>
    end;
map_one({name_prefix, Prefix, Rest}, Row, State) ->
    {name_prefix, Prefix, map_one(Rest, Row, State)};

map_one({datetime, F}, Row, State) ->
    z_convert:to_datetime(map_one(F, Row, State));

map_one({date, F}, Row, State) ->
    z_convert:to_date(map_one(F, Row, State));

map_one({time, F}, Row, State) ->
    z_convert:to_time(map_one(F, Row, State));

map_one({datetime, D, T}, Row, State) ->
    {map_one({date, D}, Row, State),
     map_one({time, T}, Row, State)};

map_one({if_then_else, Cond, If, Else}, Row, State) ->
    case prop_empty(map_one(Cond, Row, State)) of
        false -> map_one(If, Row, State);
        true -> map_one(Else, Row, State)
    end;

map_one(F, _, _) ->
    throw({import_error, {invalid_import_definition, F}}).



map_one_prefer([F], Row, State) ->
    map_one(F, Row, State);
map_one_prefer([F|Rest], Row, State) ->
    Prop = map_one(F, Row, State),
    case prop_empty(Prop) of
        false -> Prop;
        true -> map_one_prefer(Rest, Row, State)
    end.

map_one_concat(List, Row, State) ->
    concat([ map_one(F, Row, State) || F <- List]).


%% Concatenate multiple values, take into account that a value might be a tuple or atom
concat([]) ->
    <<>>;
concat([X]) when is_atom(X); is_tuple(X) ->
    X;
concat(L) ->
    iolist_to_binary(L).

concat_spaces([]) ->
    <<>>;
concat_spaces([X]) when is_atom(X); is_tuple(X) ->
    X;
concat_spaces([H|L]) ->
    iolist_to_binary(concat_spaces(L, [H])).

concat_spaces([], Acc) ->
    lists:reverse(Acc);
concat_spaces([C], Acc) ->
    lists:reverse([C,<<" ">>|Acc]);
concat_spaces([H|T], Acc) ->
    concat_spaces(T, [<<" ">>,H|Acc]).

has_required_rsc_props(Props) ->
            not(prop_empty(proplists:get_value(category_id, Props)))
    andalso not(prop_empty(proplists:get_value(name, Props)))
    andalso not(prop_empty(proplists:get_value(title, Props))).


add_defaults(D, P) ->
    add_defaults(D, P, P).

add_defaults([], _P, Acc) ->
    Acc;
add_defaults([{K,V}|Rest], P, Acc) ->
    case proplists:get_value(K, P) of
        undefined ->
            add_defaults(Rest, P, [{K,V}|Acc]);
        _ ->
            add_defaults(Rest, P, Acc)
    end.


increase(Key, Record) ->
    case proplists:get_value(Key, Record) of
        undefined ->
            [{Key, 1} | Record];
        N -> [{Key, N+1} | proplists:delete(Key, Record)]
    end.

add_result_ignored(Type, S=#importstate{result=R}) ->
    S#importstate{result=R#importresult{ignored=increase(Type, R#importresult.ignored)}}.

add_result_seen(Type, S=#importstate{result=R}) ->
    S#importstate{result=R#importresult{seen=increase(Type, R#importresult.seen)}}.

add_result_new(Type, S=#importstate{result=R}) ->
    S#importstate{result=R#importresult{new=increase(Type, R#importresult.new)}}.

add_result_updated(Type, S=#importstate{result=R}) ->
    S#importstate{result=R#importresult{updated=increase(Type, R#importresult.updated)}}.

add_result_error(_Type, Error, S=#importstate{result=R}) ->
    S#importstate{result=R#importresult{errors=[Error|R#importresult.errors]}}.

flush_add(Id, State=#importstate{to_flush=F}) ->
    State#importstate{to_flush=[Id|F]}.


managed_edge_add(Id, NewEdges, State=#importstate{managed_edges=Tree}) ->
    case gb_trees:lookup(Id, Tree) of
        {value, NewEdges} ->
            State;
        {value, undefined} ->
            State#importstate{managed_edges=gb_trees:update(Id, NewEdges, Tree)};
        {value, V} ->
            State#importstate{managed_edges=gb_trees:update(Id, sets:to_list(sets:from_list(NewEdges ++ V)), Tree)};
        none ->
            State#importstate{managed_edges=gb_trees:insert(Id, NewEdges, Tree)}
    end.


prop_empty(<<>>) -> true;
prop_empty(<<" ">>) -> true;
prop_empty(<<"\n">>) -> true;
prop_empty(undefined) -> true;
prop_empty({trans, [{_,<<>>}]}) -> true;
prop_empty({trans, []}) -> true;
prop_empty(_) -> false.


checksum(X) ->
    crypto:hash(sha, term_to_binary(X)).

