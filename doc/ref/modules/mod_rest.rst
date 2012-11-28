
.. include:: meta-mod_rest.rst

Makes models accessible using a RESTful interface.

Currently there are endpoints implemented for :term:`resources
<resource>` only, at the following URLs::

  /rest/rsc/:id
  /rest/rsc/:format:id

`format` can be one of `json` for JSON encoding or `bert` for `BERT <http://bert-rpc.org/>`_.

The controller accepts GET, PUT and DELETE requests.

* GET returns the dump of the :term:`resource`
* PUT updates the resource with the information provided
* DELETE removes the resource.  

All of these methods use the standard Zotonic :ref:`authentication <manual-auth>` checks.
  
.. seealso:: :ref:`controller-rest_rsc`

.. todo:: Add more documentation
