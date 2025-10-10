
%% @doc Notifier to map a SPARQL predicate to a Zotonic property or edge.
%% The namespace was mapped using the #rdf_ns{} notification to a compact prefix.
%% The prefix does not include ':'. If the namespace is unknown then ns_prefix
%% contains the full namespace.
%%
%% Returns:
%% - {ok, category}
%% - {ok, subclass}
%% - {ok, {column, TableName, Column}}
%% - {ok, {jsonb, TableName, Column, Selector}}
%% - {ok, {edge, Predicate, IsReversed}}
%% - {error, eacces | Reason}
%% - undefined
%%
%% The JSON selector is either a single binary, or a list of binaries (for a path).
%%
%% The returned TableName _must_ join on an 'id' column with a resource id.
%% The 'category' mapping checks the object category and all its sub-categories.
%% The 'subclass' mapping selects the sub-categories of the object category.
%%
-record(sparql_mapping, {
    ns :: binary(),
    ns_prefix :: binary(),
    predicate :: binary()
}).
