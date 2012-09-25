
.. include:: meta-logoff.rst


This action logs off the current user and reloads the current page as the anonymous visitor.

Example::

   {% button title="Log off" action={logoff} %}

After clicking the button the page will reload and the current user will be signed out.

