
.. index:: tag; for
.. _tag-for:

for
===

Loop over multiple values in a list or search result.

``{% for %}`` loops over a list of values.  For example to loop over a list of colors::

   {% for color in ["bleu", "blanc", "rouge"] %}{{ color }}{% endfor %}

This will output “bleublancrouge”.

Or to show the latest ten news article titles::

   {% for id in m.search[{latest cat="news" pagelen=10}] %}
     {{ m.rsc[id].title }}
   {% endfor %}

The argument list can also contain tuples or sublists, in that case you can assign all parts of the list items to different variables at once::

   {% for nr,color in [ [1,"bleu"], [2,"blanc"], [3,"rouge"] ] %}
     {{ nr }}: {{ color }}
   {% endfor %}

This will output::

   1: bleu
   2: blanc
   3: rouge

The for loop sets a number of variables available in the loop:

+------------------------------+--------------------------------------------------+
|Variable                      |Description                                       |
+==============================+==================================================+
|forloop.counter               |The current iteration of the loop (1-indexed)     |
+------------------------------+--------------------------------------------------+
|forloop.counter0              |The current iteration of the loop (0-indexed)     |
+------------------------------+--------------------------------------------------+
|forloop.revcounter            |The number of iterations from the end of the loop |
|                              |(1-indexed)                                       |
+------------------------------+--------------------------------------------------+
|forloop.revcounter0           |The number of iterations from the end of the loop |
|                              |(0-indexed)                                       |
+------------------------------+--------------------------------------------------+
|forloop.first                 |True if this is the first time through the loop   |
+------------------------------+--------------------------------------------------+
|forloop.last                  |True if this is the last time through the loop    |
+------------------------------+--------------------------------------------------+
|forloop.parentloop            |For nested loops, this is the loop "above" the    |
|                              |current one                                       |
+------------------------------+--------------------------------------------------+

For ... empty
-------------

``{% for %}`` can take an optional ``{% empty %}`` clause that will be displayed if the given list is empty.

For example::

   {% for id in m.search[{latest cat="news" pagelen=10}] %}
     {{ m.rsc[id].title }}
   {% empty %}
     Sorry, there is no news.
   {% endfor %}

This will output “Sorry, there is no news.” when the “latest” search did not find any pages within the category “news”.

