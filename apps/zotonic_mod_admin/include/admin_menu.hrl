
-record(menu_item, {
    id :: atom(),
    parent :: atom() | undefined,
    label :: binary() | string() | #trans{},
    url :: atom() | {atom()} | {atom(), list()} | undefined,
    icon,
    visiblecheck,
    sort = 99999 :: non_neg_integer()
}).

-record(menu_separator, {
    parent :: atom() | undefined,
    visiblecheck,
    sort = 99999 :: non_neg_integer()
}).
