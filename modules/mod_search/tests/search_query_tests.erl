-module(search_query_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

search_hassubject_test() ->
    #search_sql{
        where = "edge1.object_id = rsc.id AND edge1.subject_id = $1",
        args = [1]
    } = search([{hassubject, <<"1">>}]),
    
    #search_sql{
        where = "edge1.object_id = rsc.id AND edge1.subject_id = $1",
        args = [1]
    } = search([{hassubject, 1}]).
    
search_hassubject_with_predicate_test() ->
    About = about(),
    #search_sql{
        where = "edge1.object_id = rsc.id AND edge1.subject_id = $1 AND edge1.predicate_id = $2",
        args = [1, About]
    } = search([{hassubject, [<<"1">>, <<"about">>]}]),
    
    #search_sql{
        where = "edge1.object_id = rsc.id AND edge1.subject_id = $1 AND edge1.predicate_id = $2",
        args = [1, About]
    } = search([{hassubject, [1, about]}]).
    
search_hasobject_test() ->
    About = about(),
    #search_sql{
        where = "edge1.subject_id = rsc.id AND edge1.object_id = $1 AND edge1.predicate_id = $2",
        args = [1, About]
    } = search([{hasobject, [1, about]}]).

search_query_test() ->
    About = about(),
    {ok, QueryId} = m_rsc:insert(
        [
            {category, query},
            {is_published, true},
            {query, <<"cat=text\nis_published=true\nhassubject=[1, about]">>}
        ],
        z_acl:sudo(context())
    ),
    #search_sql{
        where = "rsc.is_published and rsc.publication_start <= now() and rsc.publication_end >= now() AND edge1.object_id = rsc.id AND edge1.subject_id = $1 AND edge1.predicate_id = $2",
        cats = [{"rsc",[<<"text">>]}],
        args = [1, About]
    } = search([{query_id, QueryId}]).

-spec search(proplists:proplist()) -> #search_sql{}.
search(Query) ->
    search_query:search(Query, context()).

-spec context() -> z:context().
context() ->
    z_context:new(testsandboxdb).

about() ->
    m_rsc:rid(about, context()).
