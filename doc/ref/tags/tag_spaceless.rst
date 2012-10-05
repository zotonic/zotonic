
.. index:: tag; spaceless
.. _tag-spaceless:

spaceless
=========

Removes whitespace between HTML tags.

Example::

   {% spaceless %}
   <div>
       <p>Test test test</p>
   </div>
   {% endspaceless %}

After rendering::

   <div><p>Test test test</p></div>


.. note:: It will not remove other whitespace.

.. versionadded:: 0.8
