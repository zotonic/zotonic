
.. include:: meta-mask.rst


Places a mask over an element, useful for blocking user interaction during lengthy postbacks.

Example::

   <a id="{{ #fb_logon }}" href="#facebook"><img src="/lib/images/fb-login-button.png" width="154" height="22" alt="Facebook login button" /></a>
   {% wire id=#fb_logon 
      action={mask target="logon_outer" message="Waiting for Facebook ."} 
      action={redirect dispatch="facebook_authorize"} %}

In this example the `logon_outer` div will be masked while the browser is being redirected to Facebook.

Form postbacks are automatically masked.

Note that you need to include a css and a js file to have working masks::

   {% lib "css/jquery.loadmask.css" %}
   {% lib "js/modules/jquery.loadmask.js" %}

This action takes three possible arguments:

========  =================================================  =======
Argument  Description                                        Example
========  =================================================  =======
target    The id of the element to be masked.                target="search-form"
message   Message to show next to the spinner image.         message="Searching..."
delay     Delay (in milliseconds) before the mask is shown.  
          Only shows the mask during lengthy actions.        delay=200
========  =================================================  =======

.. seealso:: action :ref:`action-unmask`.
