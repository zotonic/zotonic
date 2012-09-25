
.. include:: meta-submit.rst


Submits the enclosing form, a specifically targeted form or the closest form to an element.

Example::

   <form method="get" action="/search">
     <input type="text" name="q" value="" />
     {% button title="search" action={submit} %}
   </form>

Another example::

   <form id="search-form" method="get" action="/search">
     <input type="text" id="q" name="q" value="" />
   </form>
   {% button title="search" action={submit closest="q"} %}

Clicking on the button will submit the form `search-form` as it is the closest form to the element with id `q`.

The submit action is mostly used in the result of event handlers.

This action takes two possible arguments, when neither is defined then the form enclosing the trigger element will be submitted.

========  ===============================================================  =======
Argument  Description                                                      Example
========  ===============================================================  =======
target    The id of the form to be submitted.                              target="search-form"
closest   The id of an element that is close to the form to be submitted.
          When no argument value is supplied then it defaults to the id
          of the trigger element (for example
          the button the action is coupled to).                            closest
========  ===============================================================  =======
