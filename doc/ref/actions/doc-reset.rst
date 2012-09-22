
Resets the enclosing form, a specifically targeted form or the closest form to an element.

Example::

   <form method="get" action="/search">
     <input type="text" name="q" value="" />
     {% button title="search" action={submit} %}
     {% button title="cancel" action={reset} %} 
   </form>

Another example::

   <form id="search-form" method="get" action="/search">
     <input type="text" id="q" name="q" value="" />
   </form>
   {% button title="search" action={submit closest="q"}
   {% button title="cancel" action={reset closest="q"} %}

Clicking on the button will reset the form `search-form` as it is the closest form to the element with id `q`.

The reset form action is mostly used in the result of event handlers.

This action takes two possible arguments, when neither is defined then the form enclosing the trigger element will be reset.

========  ===========================================================  =======
Argument  Description                                                  Example
========  ===========================================================  =======
target    The id of the form to be reset.                              target="search-form"
closest   The id of an element that is close to the form to be reset. 
          When no argument value is supplied then it defaults to 
          the id of the trigger element (for example the button 
          the action is coupled to).                                   closest
========  ===========================================================  =======

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/actions/doc-reset.rst>`_
