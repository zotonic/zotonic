% Internal definitions for the search module.

-record(search_sql_term, {
        term = undefined,
        select = [ <<"rsc.id">> ],
        tables = #{ <<"rsc">> => <<"rsc">> },
        join_inner = #{},
        join_left = #{},
        where = [],
        sort = [],
        asort = [],
        zsort = [],
        cats = [],
        cats_exclude = [],
        cats_exact = [],
        extra = [],
        args = []
    }).
