
.. include:: meta-moreresults.rst

Show more results of the current search query inline on the page.

The moreresults action is an alternative to using a next/previous pager to paginate through search results.
Instead, moreresults lets you load more results from the current search, directly onto the same page.
This feature is similar to Twitter’s *more* button, Slashdot’s *many more* button, and others.

Using it is quite simple. The only special thing you need is that every result item should go into its own template.
The minimal example is something like the following::

   {% with m.search.query::%{ cat: "media", pagelen: 10 } as result %}
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

The number of items that get added is equal to the ``pagelen`` setting of the search.

When there are no more items, the ``moreresults`` button will get disabled automatically.

Arguments
---------

``result``
    Points to search result that you want to show more of.

``target``
    Container that the new items get appended to.

``template``
    Contains template that will be appended.

``catinclude``
    Render ``template`` through a :ref:`tag-catinclude`.

``is_result_render``
    Normally the template is called for every row in the search result. This is
    useful for lists. Sometimes all results must be rendered together, for
    example when special grouping is needed. Add ``is_result_render`` to render
    all results together in one template.

Example::

   {% with m.search.query::%{ cat: "media", pagelen: 16 } as result %}
     <div id="results">
        {% include "_items.tpl" %}
     </div>

     {% lazy action={moreresults result=result
                                 target="results"
                                 template="_items.tpl"
                                 is_result_render
                                 visible}
     %}
   {% endwith %}

Note that here we use the ``lazy`` scomp, which will perform the action if it is scrolled into view.
Because we are using the ``lazy`` scomp we have to add the ``visible`` argument so that the re-loaded
``moreresults`` action will be wired for visibility and not for a click. In this way the page loads
automatically more results if the user scrolls down.

The template ``_items.tpl`` displays the found pages in rows of four elements::

   {% for ids in result|chunk:4 %}
      <div class="row">
      {% for id in ids %}
          <div class="col-md-3">
              <h3><a href="{{ id.page_url }}">{{ id.title }}</a></h3>
              <p>{{ id.summary }}</p>
          </div>
      {% endfor %}
      </div>
   {% empty %}
      <div class="row"></div>
   {% endfor %}
