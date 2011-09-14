%% @doc Import a csv file according to the derived file/record definitions.
%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @author Marc Worrell <marc@worrell.nl>
%% Date: 2010-06-26

%% Copyright 2010-2011 Marc Worrell, Arjan Scherpenisse
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
    import/2
]).

-include_lib("zotonic.hrl").
-include_lib("../include/import_csv.hrl").

-record(importresult, {seen=[], new=[], updated=[], errors=[], ignored=[], deleted=0}).
-record(importstate, {name_to_id, managed_edges, to_flush=[], result, managed_resources}).


%% @doc The import function, read the csv file and fetch all records.
%% This function is run as a spawned process.  The Context should have the right permissions for inserting
%% or updating the resources.
import(Def, Context) ->
    StartDate = erlang:localtime(),
    %% Read all rows
    {ok, Device} = file:open(Def#filedef.filename, [read, binary]),
    Rows = parse_csv:scan_lines(Device, Def#filedef.colsep),
    file:close(Device),
    file:delete(Def#filedef.filename),
    %% Drop (optionally) the first row, empty rows and the comment rows (starting with a '#')
    Rows1 = case Def#filedef.skip_first_row of true -> tl(Rows); _ -> Rows end,
    Rows2 = lists:filter(fun([<<$#, _/binary>>|_]) -> false; ([]) -> false; (_) -> true end, Rows1),
    State = import_rows(Rows2, Def, new_importstate(), Context),

    %% @todo Delete all not-mentioned but previously imported resources
    DeleteIds = [],

    %% Return the stats from this import run
    R = State#importstate.result,
    [ {file, filename:basename(Def#filedef.filename)},
      {date_start, StartDate},
      {date_end, erlang:localtime()},
      {seen, R#importresult.seen},
      {new,  R#importresult.new},
      {updated, R#importresult.updated},
      {errors, R#importresult.errors},
      {deleted, length(DeleteIds)},
      {ignored, R#importresult.ignored}].


new_importstate() ->
    #importstate{
        name_to_id=gb_trees:empty(),
        managed_edges=gb_trees:empty(),
        managed_resources=sets:new(),
        result=#importresult{}
    }.

%%====================================================================
%% Import a record
%%====================================================================

%% @doc Import all rows.
import_rows([], _Def, ImportState, _Context) -> 
    ImportState;
import_rows([R|Rows], Def, ImportState, Context) ->
    Zipped = zip(R, Def#filedef.record, []),
    ImportState1 = import_parts(Zipped, Def#filedef.importdef, ImportState, Context),
    import_rows(Rows, Def, ImportState1, Context).

    
    zip(_Cols, [], Acc) ->
        lists:reverse(Acc);
    zip([], [N|Ns], Acc) ->
        zip([], Ns, [{N,<<>>}|Acc]);
    zip([_C|Cs], [''|Ns], Acc) ->
        zip(Cs, Ns, Acc);
    zip([C|Cs], [N|Ns], Acc) ->
        zip(Cs, Ns, [{N, C}|Acc]).


%% @doc Import all resources on a row
import_parts(_Row, [], ImportState, _Context) ->
    ImportState;
import_parts(Row, [Def | Definitions], ImportState, Context) ->
    {FieldMapping, ConnectionMapping} = Def,
    case import_def_rsc(FieldMapping, Row, ImportState, Context) of
        {S, {ignore}} ->
            import_parts(Row, Definitions, S, Context);
        {S, {error, Type, E}} ->
            add_result_seen(Type, add_result_error(Type, E, S));

        {S, {new, Type, Id, Name}} ->
            State0 = add_managed_resource(Id, FieldMapping, S),
            State1 = add_name_lookup(State0, Name, Id),
            State2 = import_parts(Row, Definitions, State1, Context),
            import_def_edges(Id, ConnectionMapping, Row, State2, Context),
            add_result_seen(Type, add_result_new(Type, State2));

        {S, {equal, Type, Id}} ->
            State0 = add_managed_resource(Id, FieldMapping, S),
            State1 = import_parts(Row, Definitions, State0, Context),
            import_def_edges(Id, ConnectionMapping, Row, State1, Context),
            add_result_seen(Type, add_result_ignored(Type, State1));

        {S, {updated, Type, Id}} ->
            %% Equal
            State0 = add_managed_resource(Id, FieldMapping, S),
            State1 = import_parts(Row, Definitions, State0, Context),
            add_result_seen(Type, import_def_edges(Id, ConnectionMapping, Row, State1, Context)),
            add_result_updated(Type, State1)
    end.


import_def_rsc(FieldMapping, Row, State, Context) ->
    {Callbacks, FieldMapping1} = case proplists:get_value(callbacks, FieldMapping) of
                                     undefined -> {[], FieldMapping};
                                     CB -> {CB, proplists:delete(callbacks, FieldMapping)}
                                end,
    %% Do the field mapping
    Props = map_fields(FieldMapping1, Row, State),

    %% Check if props mapping is valid (at least a title and a (unique) name)
    case has_required_rsc_props(Props) of
        true ->
            %% Get category name; put category ID in the record.
            Type = proplists:get_value(category, Props),
            %% Convert category name
            {CatId, State1} = name_lookup_exists(Type, State, Context),
            Props0 = [{category_id, CatId} | proplists:delete(category, Props)],

            %% Make row checksum
            PropK = proplists:get_keys(Props0),
            RowCS = import_checksum(Props0),

            %% Unique name of this rsc
            Name = case proplists:get_value(name, Props0) of
                       undefined -> throw({import_error, {definition_without_unique_name}});
                       N -> N
                   end,

            %% Lookup existing record
            case name_lookup(Name, State1, Context) of
                %% Not exists
                {undefined, State2} ->
                    Props1 = [{import_csv_original, Props0} | Props0],

                    %% .. insert record
                    case m_rsc_update:insert(Props1, false, Context) of
                        {ok, NewId} ->
                            StateAdd = flush_add(NewId, State2),
                            case proplists:get_value(rsc_insert, Callbacks) of
                                undefined -> none;
                                Callback -> Callback(NewId, Props1, Context)
                            end,
                            {StateAdd, {new, Type, NewId, Name}};
                        E ->
                            ?DEBUG(Type),
                            ?DEBUG(Props1),
                            ?DEBUG(Row),
                            ?DEBUG(E),
                            ?DEBUG(m_rsc:name_to_id(Name, Context)),
                            {State2, {error, Type, E}}
                    end;

                %% Exists? 
                {Id, State2} ->
                    %% Calculate resource checksum
                    RscCS = rsc_checksum(Id, PropK, Context),
                    %% checksum check
                    case RowCS of
                        RscCS ->
                            %% They stayed equal; do nothing.
                            {State2, {equal, Type, Id}};
                        _ ->
                            %% Resource has been enriched, or the import has changed.
                            {State3, Props1} = case m_rsc:p(Id, import_csv_original, Context) of
                                         undefined ->
                                             %% No import_csv_original record for this rsc; update all.
                                             {State2, Props0};

                                         OriginalProps ->
                                             case m_rsc:p(Id, is_protected, Context) of
                                                 false ->
                                                     %% Protected flag was unchecked; update the record.
                                                     {State2, [{import_csv_touched, {props, []}} | Props0]};
                                                 true ->
                                                     %% 3-way diff.
                                                     compare_old_new_props(Id, OriginalProps, Props0, State2, Context)
                                             end
                                     end,

                            case Props1 of 
                                [] ->
                                    {State, {equal, Type, Id}};
                                Props1 ->
                                    Props2 = [{import_csv_original, Props0} | Props1],
                                    %% .. update record and new checksum
                                    case m_rsc_update:update(Id, Props2, false, Context) of
                                        {ok, Id} ->
                                            StateAdd = flush_add(Id, State3),
                                            case proplists:get_value(rsc_update, Callbacks) of
                                                undefined -> none;
                                                Callback -> Callback(Id, Props2, Context)
                                            end,
                                            {StateAdd, {updated, Type, Id}};
                                        E ->

                                            {State3, {error, Type, E}}
                                    end
                            end
                    end

            end;
        false ->
            {State, {ignore}}
    end.



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
            %% Ignore
            fail;
        Name ->
            case name_lookup(Name, State, Context) of
                {undefined, _State1} ->
                    ?DEBUG("Edge predicate does not exist?"),
                    ?DEBUG(Name),
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
        <<>> -> fail;
        Name ->
            case name_lookup(Name, State, Context) of
                %% Object doesn't exist, create it using the objectprops
                {undefined, _State1} -> 
                    ?DEBUG([{category, ObjectCat}, {name, Name} | ObjectProps]),
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
        {value, _} -> State;
        none -> State#importstate{name_to_id=gb_trees:insert(Name, Id, Tree)}
    end.

name_lookup(Name, #importstate{name_to_id=Tree} = State, Context) ->
    case gb_trees:lookup(Name, Tree) of
        {value, V} -> {V, State};
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
    Defaults = [{is_protected, true}, {is_published, true}],
    Mapped = [map_def(MapOne, Row, State) || MapOne <- Mapping],
    Type = case proplists:get_value(category, Mapped) of 
               undefined -> throw({import_error, no_category_in_import_definition});
               T -> T
           end,
	% Normalize and remove undefined values
    P = lists:filter(fun({_K,undefined}) -> false; (_) -> true end, 
					 [{K, map_one_normalize(K, Type, V)} || {K,V} <- Mapped]),
    add_defaults(Defaults, P).

	map_def({K,F}, Row, State) ->
		{K, map_one(F, Row, State)};
	map_def(K, Row, State) when is_atom(K) ->
		map_def({K,K}, Row, State).


map_one_normalize(name, _Type, <<>>) ->
    <<>>;
map_one_normalize(name, _Type, {name_prefix, Prefix, V}) ->
    CheckL = 80 - strlen(Prefix) - 1,
    Name = case strlen(V) of
               L when L > CheckL ->
                   %% When name is too long, make a unique thing out of it.
                   z_string:to_name(z_convert:to_list(Prefix) ++ "_" ++ base64:encode_to_string(checksum(V)));
               _ -> 
                   z_string:to_name(z_convert:to_list(Prefix) ++ "_" ++ z_string:to_name(V))
           end,
    z_convert:to_binary(Name);

map_one_normalize(name, Type, V) ->
    CheckL = 80 - strlen(Type) - 1,
    Name = case strlen(V) of
               L when L > CheckL ->
                   %% When name is too long, make a unique thing out of it.
                   z_string:to_name(z_convert:to_list(Type) ++ "_" ++ base64:encode_to_string(checksum(V)));
               _ -> 
                   z_string:to_name(z_convert:to_list(Type) ++ "_" ++ z_string:to_name(V))
           end,
    z_convert:to_binary(Name);
map_one_normalize(_, _Type, V) ->
    V.


strlen(B) when is_binary(B) -> erlang:size(B);
strlen(L) when is_list(L) -> erlang:length(L);
strlen(A) when is_atom(A) -> erlang:length(atom_to_list(A)).

map_one(F, _Row, _State) when is_binary(F) -> F;
map_one(F, _Row, _State) when is_list(F) -> iolist_to_binary(F);
map_one(undefined, _Row, _State) -> undefined;
map_one(true, _Row, _State) -> true;
map_one(false, _Row, _State) -> false;
map_one(F, Row, _State) when is_atom(F) ->
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

compare_old_new_props(_RscId, Original, New, State, Context) -> 
    compare_old_new_props(_RscId, Original, New, State, [], Context).

compare_old_new_props(_RscId, _OriginalProps, [], State, Acc, _Context) -> 
    {State, Acc};
compare_old_new_props(RscId, OriginalProps, [{NewKey, NewValue} | NewProps], State, Acc, Context) ->
    case compare_old_new_props1(RscId, NewKey, proplists:get_value(NewKey, OriginalProps),NewValue, State, Context) of
        {_, false} -> compare_old_new_props(RscId, OriginalProps, NewProps, State, Acc, Context);
        {S, true} -> compare_old_new_props(RscId, OriginalProps, NewProps, S, [{NewKey, NewValue} | Acc], Context)
    end.


%% Check the original value and the new value of prop against the prop
%% in the resource.
compare_old_new_props1(Id, Prop, OrigVal0, NewVal, State, Context) ->
    OrigTouched = case m_rsc:p(Id, import_csv_touched, Context) of
                      undefined -> [];
                      {props, T} -> T;
                      [] -> []
                  end,
    %% Put surrounding paragraph tag around original value -- sometimes it is was not there yet.
    OrigVal = case Prop of
                  body ->
                      case z_convert:to_list(OrigVal0) of
                          [$<,$p,62|_] -> OrigVal0;
                          [$<,$P,62|_] -> OrigVal0;
                          Val -> list_to_binary("<p>" ++ Val ++ "</p>")
                      end;
                  _ -> OrigVal0
              end,
    DbVal = m_rsc:p(Id, Prop, Context),
    {NewTouched, Result} = 
        case {Prop, DbVal} of 
            %% special properties which are always overwritten
            {is_protected, false} ->
                {OrigTouched, true};

            {_, NewVal} ->
                %% The value in the database is equal to the to-be-updated value. 
                %% ignore this value.
                {OrigTouched, false};

            {_, OrigVal} ->
                %% The value in the database has not changed from its original value.
                %% (and is different from NewVal). Use this value.
                {lists:delete(Prop, OrigTouched), true};

            {_, DbVal} ->
                %% The value has been edited in the database.
                {case lists:member(Prop, OrigTouched) of
                     true -> OrigTouched;
                     false -> [Prop | OrigTouched]
                 end,
                 false}
        end,
    case NewTouched of
        OrigTouched ->
            % ignore
            {State, Result};
        _ ->
            m_rsc:update(Id, [{import_csv_touched, {props, NewTouched}}], Context),
            {flush_add(Id, State), Result}
    end.


has_required_rsc_props(Props) ->
            not(prop_empty(proplists:get_value(category, Props)))
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


prop_empty(<<>>) ->
    true;
prop_empty(<<" ">>) ->
    true;
prop_empty(<<"\n">>) ->
    true;
prop_empty(undefined) ->
    true;
prop_empty(_) ->
    false.


%% Calculate a checksum of the modified properties.
import_checksum(Props) ->
    checksum(lists:sort(Props)).

rsc_checksum(Id, PropKeys, Context) ->
    Values = [{K, m_rsc:p(Id, K, Context)} || K <- PropKeys],
    checksum(lists:sort(Values)).

checksum(X) ->
    crypto:sha(term_to_binary(X)).

