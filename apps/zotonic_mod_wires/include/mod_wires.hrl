
-record(page_actions, {
    actions = [] :: list( {atom(), proplists:list()} ) | {atom(), proplists:list()}
}).
