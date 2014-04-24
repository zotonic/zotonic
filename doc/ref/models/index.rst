.. _models:

Models
======

Models are Erlang modules which are the main accessor for retrieving data in a template. Models are Erlang modules which are prefixed with `m_`; in the templates they are accessible using ``m.`` For instance, the model to access :term:`resources <resource>` is called ``m_rsc.erl``; in the template this model lets you access resources by id as ``{{ m.rsc[id] }}``.

Below is the list of all models that are available in Zotonic.

.. toctree::
   :maxdepth: 1
   :glob:

   model_*

