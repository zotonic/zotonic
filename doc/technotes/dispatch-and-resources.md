Dispatch and resources
======================

A dispatch rule can be associated with a resource.

This can be an implicit or explicit association.


Implicit associaton
-------------------

Dispatch rule which references the resource id in the dispatch options.
The id will be passed to the controller and the controller will allow to
proceed if the resource exists or not.

The dispatch rule defines the path for matching URLs.


Explicit association
--------------------

The path is defined by the page path of the resource, or a default
page path if the resource does not have a page path.

Instead of a path the dispatch rule defines the name of the resource.

    [
      {search, page_search, controller_template, [ {template, "..."} ]},
      {search, [ "search" ], controller_template, [ {id, page_search}, {template, "..." ]}
    ].

On dispatching a path like '/zoeken' we find the resource matching the
page_path. We then need to find the controller and controller options.
This is done by finding the dispatch rule mentioning the name of the resource.
After this the controller and the controller options are returned.

The 404 handler in mod_base provides the matching on page_path and currently
returns the resource id. The dispatcher should match the resource id to the dispatch
rules and return the appropriate controller and controller options.
If not matched then we can do the internal rewrite to the default page_url and
take the matching controller and controller options from there.


QUESTIONS
---------

1. Should the base template be defined with the resource, then it is selectable
   by an editor.
   Problems:
   - choice of controller
   - mixing representation and content

2. Should we just remove the name of the dispatch rule and instead use the name
   of the resource?
   This makes it less flexible.