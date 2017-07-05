.. include:: includes/meta-edge_delete.rst

Example
"""""""

Perform some action when an edge is deleted::

    -include_lib("zotonic_core/include/zotonic.hrl").
    -export([observe_edge_delete/2]).

    observe_edge_delete(#edge_delete{edge_id = Id}, Context) ->
        %% Consult the edge_log table to get the late edge's details
        Edge = z_db:assoc_row("select * from edge_log where edge_id = $1", [Id], Context)),

        ?DEBUG(Edge),
        %% logged is when the deletion was logged; created is when the edge was
        %% originally created
        %% [{id,11},{op,<<"DELETE">>},{edge_id,25},{subject_id,341},{predicate_id,300},{predicate,<<"about">>},{object_id,338},{seq,1000000},{logged,{{2016,10,13},{10,23,21}}},{created,{{2016,10,13},{10,23,13}}}]

        %% Do something...

        ok.

