.. Zotonic Glossary
   part of Documentation project 2012

   19-9-2012 Arjan Scherpenisse


Glossary
========

.. glossary::
   :sorted:

   Template
      A snippet of ErlyDTL code which is used to render a HTML template.

   Dispatch rule
      A dispatch rule maps URL patterns to controllers. Dispatch rules are defined in files in the .dispatch. folder of a Zotonic module.

   Controller
      referenced from a dispatch rule, a controller is the main entry point where a request is handled. Commonly used controller is controller_template, which serves a template on the URL at which the controller listens.

   Context
      is the current request context. It contains all the request data, the current site, the handle to the database and the results (scripts or templates) you will be sending back. The context is commonly passed along in Zotonic as the last argument of a function.

   Session
      The session is an Erlang process. It is connected to the session cookie id on the browser. The session contains the id of the current user and more key/value pairs, called session variables. The session is also linked to page processes. For every open page on the browser we have a process on the server. This page process is used for the communication between the server and the browser.

   Category
      Not yet described.

   Page
      another word for .resource.; used in the admin.

   Page connection
      another word for .edge.; used in the admin.

   Edge
      Not yet described.

   Resource
      For simplicity of communication, a resource is often referred to as a page. As every resource can have their own page on the web site.

   Zotonic module
      (just .module., for short) - A zotonic module is a collection of related functionality like scomps, filters, dispatch rules, controllers, templates, etc. Zotonic modules live in folders under the modules/ directory and, by convention, are prefixed with mod_.

   Erlang module
      not to be confused with a Zotonic module, an erlang module is a single .erl file which contains Erlang functions.
