
-record(menu_item, {
    id :: term(),
    parent :: term() | undefined,
    label :: binary() | string() | #trans{},
    url :: atom() | {atom()} | {atom(), list()} | string() | binary() | undefined,
    icon,
    visiblecheck,
    sort = 99999 :: non_neg_integer()
}).

-record(menu_separator, {
    parent :: term() | undefined,
    visiblecheck,
    sort = 99999 :: non_neg_integer()
}).
