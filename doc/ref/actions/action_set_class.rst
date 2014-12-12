
.. include:: meta-set_class.rst


Set the class of an element.

Example::

   <div id="x" class="not-inited"></div>
   {% button text="Init" action={set_class target="x" class="inited"} %}


This uses the jQuery `attr('class', class_name)` method to set the new class.
