.. Zotonic Glossary
   part of Documentation project 2012

   19-9-2012 Arjan Scherpenisse, First concept


Glossary
========

.. glossary::
   :sorted:

   Template
      A snippet of ErlyDTL code which is used to render a piece of content (usually HTML). Templates live under the templates/ folder of a module. The template is meant to express presentation logic.

   Filter
      A template mechanism which is used inside a template to transform data before it is output. For instance: the .lower. filter transforms its input to lowercase. Filters are implemented as Erlang modules, exporting a single filter function. 

   Action
      An action is functionality that can be attached to a HTML element or event. Actions are wired to an element or event. Think of showing dialogs, posting forms, hiding elements etc.

      See also the section on :ref:`manual-actions` in the templates manual.

   Tag
      The template systems provides tags which function as simple programming constructs. For instance, the if tag can be used for boolean tests and the for tag allows looping. The zotonic templating system compiles the tags found in a template to Erlang byte code which will be called when the template is rendered. This is very efficient.

   Scomp
      A scomp (from .Screen COMPonent.) is a custom template tag, implemented by an Erlang module named after the scomp name, prefixed with `scomp_`. Scomps usually generate HTML. Zotonic modules can implement their own scomp in the module.s scomps/ folder.

   Wire
      Connects actions and events to a HTML element. The wire scomp is the basis for most Ajax interaction on web pages. It allows to connected actions to HTML elements. Examples of actions are showing/hiding elements or postbacks to the server. ###(hmm. It is scomp - it is also a often used function in the Erlang code z_render:wire/2)###

   Postback
      An AJAX or Websocket request from the browser to the server. It is handled on the server by event/2 Erlang functions. A postback is normally sent to the controller that generated the page, but can be changed by specifying a delegate, which must be the name of an Erlang module.

   Model
      An Erlang module which is the main accessor for retrieving data. The erlang modules are prefixed with `m_`; in the templates they are accessible using .m... For instance, the model to access resources is called m_rsc.erl; in the template a certain resource can be accessed through the model as {{ m.rsc[id] }}.

   Service
      Provides a generalized way to create API calls. These calls automatically use the authentication mechanism (session id or OAuth) to perform access checks.

   Validator
      A validator is used to check input fields in a HTML form. A validator has two parts: the client side javascript and a server side check. You add validators to a form with the {% validate %} template tag. A validated query argument can be accessed on the server using z_context:get_q_validated/2.

   Dispatch rule
      A dispatch rule maps URL patterns to controllers. Dispatch rules are defined in files in the .dispatch. folder of a Zotonic module. The dispatch rule definitions are also used to generate the urls for resources and other pages.

   Controller
      A controller is the main entry point where a request is handled. Controllers are referenced from a dispatch rule. Commonly used controller is controller_template, which serves a template on the URL for which the controller configured. 

   Context
      The context is the current request context. It contains all the request data, the current site, the handle to the database and the results (scripts or templates) you will be sending back. The context is commonly passed along in Zotonic as the last argument of a function.

   Session
      The session is an Erlang process. It is connected to the session cookie id on the browser. The session contains the id of the current user and more key/value pairs, called session variables. The session is also linked to page processes. For every open page on the browser we have a process on the server. This page process is used for the communication between the server and the user-agent (browser).

   Category
      The data model has a hierarchical tree for the categorization of resources. Every resource is part of one category. The categorization is used amongst others to decide which template to show when displaying a resource. A category is a resource of the category .category..

   Page
      Another word for .resource.; used in the admin.

   Media
      Media are files, embed codes etc. They are attached to a resource. Every resource can hold a single medium. The resource is usually within the category .media..

   Page connection
      Another word for .edge.; used in the admin.

   Edge
      Resources are able to form connections to each other. These connections are called edges. Edges contain no information other than where they are linked to and from, and what their predicate is. Edges have a single direction, from the subject to the object.

   Predicate
      Each edge has a .label. attached to it to determine what the meaning of the edge is. For instance, when an article is linked to a person, the predicate (label) might read .author., to indicate that that person is the author of the article. A predicate is a resource of the category .predicate..

   Property
      A field in a resource. Examples are title and summary. Properties are dynamically defined. Although some property names are reserved, you can set any other property, which will be stored in the resource.

   Data model
      The general data model of (categorized) resources which connect to other resources using labelled edges. This data model is loosely based on the semantic web.

   Resource
      The main building block of the data model. For simplicity of communication, a resource is often referred to as a page. Every resource usually has its own page on the web site.

   Translation
      There are two kinds of translations. Texts in the templates and Erlang modules; and translations of resources. Templates and Erlang modules are translated using gettext. Resources are translated in the admin, any resource can have an arbitrary number of translations. Zotonic selects the shown language based on the preferred language of the visitor and the available languages of a resource.

   Zotonic module
      A zotonic module (just .module., for short) is a collection of related functionality like scomps, filters, dispatch rules, controllers, templates, etc. Zotonic modules are located in folders under the modules/ directory and, by convention, are prefixed with `mod_`.

   Zotonic site
      A zotonic site is a collection of scomps, filters, dispatch rules for one website. It is a special kind of zotonic module with has its own config file which allows one to set the hostname, admin password, database connection parameters. It often has a set of site specific modules. The config file contains site wide settings. Zotonic uses the settings to start the site on the right port and connect it to the right database. A zotonic system can run multiple sites.

   Erlang module
      Not to be confused with a Zotonic module, an erlang module is a single .erl file which contains Erlang functions.

   Non Informational URI
      The non informational uri is the base url of a resource. It always redirects to a representation of the resource. Think of a HTML page, image or JSON download. The chosen representation depends on the .Accept. HTTP request header. The non informational uri of a resource is always like http://example.com/id/1234

