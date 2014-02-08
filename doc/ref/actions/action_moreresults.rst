
.. include:: meta-moreresults.rst

Show more results of the current search query inline on the page.

The `moreresults` action is an alternative to using a next/previous pager to paginate through search results. Instead, `moreresults` lets you load more results from the current search, directly onto the same page. This feature is similar to Twitter’s *more* button, Slashdot’s *many more* button, and others.

Using it is quite simple. The only special thing you need is that every result item should go into its own template. 
The minimal example is something like the following::

   {% with m.search[{query cat="media" pagelen=10 }] as result %}
     <div id="results">
       {% for id in result %}
         {% include "_item.tpl" %}
       {% endfor %}
     </div>
   
     {% button text="more..." action={moreresults result=result 
                                      target="results" 
                                      template="_item.tpl"}
     %}
   {% endwith %}

The `moreresults` action has a `result` argument, which should point to the search result that you want to show more of. The `target` attribute denotes the container that the new items get appended to, and the `template` argument shows which template needs to be appended.

The number of items that get added is equal to the `pagelen` setting of the search.

When there are no more items, the `moreresults` button will get disabled automatically.

Normally the template is called for every row in the search result. This is useful for lists.
Sometimes all results must be rendered together, for example when special grouping is needed.
In those case the argument `is_result_render` must be added. Example::

   {% with m.search[{query cat="media" pagelen=16 }] as result %}
     <div id="results">
        {% include "_items.tpl" %}
     </div>
   
     {% lazy action={moreresults result=result 
                                 target="results" 
                                 template="_items.tpl"
                                 is_result_render}
     %}
   {% endwith %}

Note that here we use the `lazy` scomp, which will perform the action if it is scrolled into view.
In this way the page loads automatically more results if the user is scrolls down.

Where `_items.tpl` displays the found pages in rows of four elements::

   {% for ids in result|chunk:4 %}
      <div class="row-fluid">
      {% for id in ids %}
          <div class="span4">
              <h3><a href="{{ id.page_url }}">{{ id.title }}</a></h3>
              <p>{{ id.summary }}</p>
          </div>
      {% endfor %}
      </div>
   {% empty %}
      <div class="row-fluid"></div>
   {% endfor %}

