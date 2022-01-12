
.. index:: tag; spaceless
.. _tag-spaceless:

spaceless
=========

Removes whitespace between HTML tags.

.. note:: spaceless does not remove non breaking spaces and other whitespace.

Example::

   {% spaceless %}
   <div>
       <p>Test test test</p>
   </div>
   {% endspaceless %}

After rendering::

   <div><p>Test test test</p></div>


.. versionadded:: 0.8
