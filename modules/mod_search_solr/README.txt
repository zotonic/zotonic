= SOLR =

Most of the query options are similar to the query-search syntax:
http://zotonic.com/documentation/761/the-query-search-model, 
but you can do more nice things with Solr.


== Regular query syntax ==

You can refactor your existing queries (from m.search[{query ....}]):

    {% with m.search[{solr text="collage" cat="media" }] as r %}
        {% print r %}
    {% endwith %}

Works exactly the same, including paging using the pager scomp. Only
thing that is different are that there are no custom pivots, and some
sort fields might be named differently.



== Finding things which are similar to another thing ==

Using Solr's MoreLikeThis feature we implemented a simple
matcher. Following example finds 4 things which are closely related to
thing nr 332:

    {% for id in m.search[{match id=332 pagelen=4}] %}
    {{ m.rsc[id].title }},
    {% endfor %}


== Full-txt search and highlighting text ==

Fulltext search works out of the box. You can tell solr to highlight
the relevant sections of a document for you. Currently this does only
work for the title and the summary (the document body is only indexed
in solr, not stored).

Note that if you use highlighting you get an extended search result
format. Following query searches fulltext for the string "hello world"
in docs with the category "text" (or below). It tells solr to
highlight the found titles and summaries (with <em> tags):

    {% with m.search[{solr text="hello world" highlight="title,summary" cat="text"}] as r %}

The Ids are now in "r.result.ids":

    {% for id in r.result.ids %}

And to show the title highlighted:

    {{ r.result.highlight[id].title|default:m.rsc[id].title }}

..and the summary:

    {{ r.result.highlight[id].summary|default:m.rsc[id].summary }}


    {% endfor %}


== Facetting ==

Facetting is the subdivision of the search result into a couple of
relevant groups, like zotonic's categories for instance. With a single
search result, you can show the division of how many items are in a
certain group.

The following tells solr to facet on the rsc category:

    {% with m.search[{solr text="hello world" facet="category"}] as r %}

    {% for category_name,count in r.result.facet_fields.category %}
        <li>{{ category-name}} contains {{ count }} items.</li>
    {% endfor %}

    {% endwith %}

