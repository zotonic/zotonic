
%% @doc Notifier to map a SPARQL predicate to a Zotonic property or edge.
%% The namespace was mapped using the #rdf_ns{} notification to a compact prefix.
%%
%% Returns:
%% - {ok, {column, TableName, Column}}
%% - {ok, {jsonb, TableName, Column, Selector}}
%% - {ok, {edge, Predicate}}
%% - {error, eacces | Reason}
%% - undefined
%%
%% The returned TableName _must_ join on an 'id' column with a resource id.
%%
%% TODO: add 'is_reversed' flag to returned edge
%%
-record(sparql_mapping, {
    ns :: binary(),
    ns_prefix :: binary(),
    predicate :: binary()
}).
