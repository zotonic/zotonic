
.. include:: meta-spinner.rst

Add an AJAX activity indicator.

Whenever an AJAX call is made the HTML element with the id ``#spinner`` will be shown during the call. It will not be shown during :term:`Comet` activity.

You can add a spinner element to your page yourself or use this tag to place it somewhere on your page.

Example::

   {% spinner %}

Outputs the HTML code::

   <div id="spinner" class="spinner" style="display: none">
     <img alt="activity indicator" src="/lib/images/spinner.gif" />
   </div>

The spinner tag accepts a single argument “image”.  The “image” argument must contain the URL for the image displayed. It defaults to “`/lib/images/spinner.gif </lib/images/spinner.gif>`_”.
