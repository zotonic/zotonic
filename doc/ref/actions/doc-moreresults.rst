
Show more results of the current search query inline on the page.

The `moreresults` action is an alternative to using a next/previous pager to paginate through search results. Instead, `moreresults` lets you load more results from the current search, directly onto the same page. This feature is similar to Twitter's "more" button, Slashdot's "many more" button, and others.

Using it is quite simple. The only special thing you need is that every result item should go into its own template. The minimal example that I can come up with is the following::

   {% with m.search[{query cat="media" pagelen=10 }] as result %}
     <div id="results">
       {% for id in result %}
         {% include "test_item.tpl" %}
       {% endfor %}
     </div>
   
     {% button text="more..." action={moreresults result=result 
                                      target="results" 
                                      template="test_item.tpl"}
     %}
   {% endwith %}

The `moreresults` action has a `result` argument, which should point to the search result that you want to show more of. The `target` attribute denotes the container that the new items get appended to, and the `template` argument shows which template needs to be appended.

The number of items that get added is equal to the `pagelen` setting of the search.

When there are no more items, the `moreresults` button will get disabled automatically.

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/actions/doc-moreresults.rst>`_
