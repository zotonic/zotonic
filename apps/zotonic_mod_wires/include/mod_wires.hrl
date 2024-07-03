
-record(page_actions, {
    actions = [] :: list( {atom(), proplists:proplist()} ) | {atom(), proplists:proplist()}
}).
