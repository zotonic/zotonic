
-type menu_visiblecheck() ::
      undefined
    | boolean()
    | {acl, z_acl:action(), z_acl:object()}
    | {allof, [menu_visiblecheck()]}
    | {anyof, [menu_visiblecheck()]}
    | [menu_visiblecheck()]
    | fun(() -> boolean())
    | fun((z:context()) -> boolean()).

-record(menu_item, {
    id :: term(),
    parent :: term() | undefined,
    label :: binary() | string() | #trans{},
    url :: atom() | {atom()} | {atom(), list()} | string() | binary() | undefined,
    icon,
    visiblecheck :: menu_visiblecheck(),
    sort = 99999 :: non_neg_integer()
}).

-record(menu_separator, {
    parent :: term() | undefined,
    visiblecheck :: menu_visiblecheck(),
    sort = 99999 :: non_neg_integer()
}).
