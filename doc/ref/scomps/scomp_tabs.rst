
.. include:: meta-tabs.rst

Make a HTML element into a tab set.

This is a simple interface to the jQuery UI tabs functionality.  It will show tabs to switch between different panes.

The only argument is “id” which is the id of the container of the tabs.

The following example will show an interface with three tabs::

   {% tabs id="tabs" %}
   <div id="tabs">
     <ul>
       <li><a href="#tabs-1">Nunc tincidunt</a></li>
       <li><a href="#tabs-2">Proin dolor</a></li>
       <li><a href="#tabs-3">Aenean lacinia</a></li>
     </ul>
     <div id="tabs-1">
       <p>Proin elit arcu, rutrum commodo, vehicula tempus.</p>
     </div>
     <div id="tabs-2">
       <p>Morbi tincidunt, dui sit amet facilisis feugiat.</p>
     </div>
     <div id="tabs-3">
       <p>Mauris eleifend est et turpis.</p>
     </div>
   </div>

.. note::
   There is no default styling for jQuery UI elements in the zotonic CSS files.
   See these two threads in the zotonic users mailinglist: 
   `does tabs scomp still work? <https://groups.google.com/d/topic/zotonic-users/mIxPpKSzugM/discussion>`_,
   and `playing with tabs scomp <https://groups.google.com/d/topic/zotonic-users/BnZtNvVWds0/discussion>`_.
   See also the `jQuery UI <http://jqueryui.com>`_ documentation.
