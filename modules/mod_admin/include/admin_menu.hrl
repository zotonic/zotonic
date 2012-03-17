
-record(menu_item,
        {id,
         parent,
         label,
         url,
         icon,
         visiblecheck}).

-record(menu_separator,
        {parent, visiblecheck}).
