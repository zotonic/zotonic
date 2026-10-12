%% First notification, before any resource or medium is written.
%% Return undefined for the default importer, {ok, Id} after importing the item,
%% {ok, Id, #{SourceURI => DependentId}} to also track dependent resources,
%% {skip, Reason} to deliberately ignore it, or {error, Reason} to retry later.
%% item = #{uri, props, edges, medium}; edges = [{PredicateId, ObjectId | URI}].
%% dependents = [#{uri, props, medium}]. All properties are still untrusted.
-record(import_feed_item, {
    item :: map(),
    dependents = [] :: [map()],
    feed :: map(),
    raw :: term()
}).
