.. highlight:: django
.. _manual-tags:

Builtin tags and custom tags
============================

Tags add logic and flexibility to your templates.

Zotonic tags are in general compatible with Django tags, though there
are some differences in which arguments are accepted. Not all Django
tags are implemented.

Zotonic also adds some new tags and `Filters` unknown to Django. For
example the tags :ref:`tag-image` and :ref:`tag-trans`.

The general syntax for a tag is the following::

  {% tagname param1=value param2=value %}

Some tags are "block" tags and therefore consist of a start and an end
tag. The name of the end tag is always `end` plus the name of the
opening tag::

  {% tag %}
     ...
  {% endtag %}


There are two different kinds of tags: builtin tags and custom
tags. The main difference between these is that custom tags can be
implemented by :ref:`manual-modules`, and builtin tags are implemented
inside the ErlyDTL template system.

  
Builtin tags
------------

Builtin tags are the tags that are the building blocks of the template
system. For instance, with combining the :ref:`tag-for` tag with the
:ref:`tag-if` tag, you can create a "striped" table::

   {% for x in list %}
   <tr class="{% if forloop.counter|is_even %}even{% else %}odd{% endif %}">
       data...
   </tr>
   {% endfor %}

For a full list of builtin tags, see: :ref:`tags`.


.. _manual-scomps:

Custom tags
-----------

Custom tags, internally called `scomps`, are module-defined tags,
which are used when including templates is not enough and more
programming is needed.

Custom tags add logic to templates or generate HTML/Javascript
constructs that are too difficult for templates. A good example is the
:ref:`scomp-menu` scomp which implements the menu, including sub menus
and highlighting of the current menu item.

Scomps have the same syntax as tags::

   {% scompname arg=value ... %}.

For a full list of custom tags that ship with Zotonic, see:
:ref:`scomps`.
   

Implementing a custom tag
.........................

.. highlight:: erlang

A scomp module lives in a module under the ``scomps/`` folder, and has
the following naming convention: ``scomp_modulename_scompname``.

So a fictitiuos scomp called ``likebutton`` in the ``mod_facebook``
module would defined in the Erlang module called
``scomp_facebook_likebutton.erl``.
               
You can implement scomps by using the `scomp` behaviours. An example
scomp that does `absolutely nothing` is implemented as follows::

  -module(scomp_facebook_likebutton).
  -behaviour(gen_scomp).
  -export([vary/2, render/3]).
  -include("zotonic.hrl").
  
  vary(_Params, _Context) -> nocache.
  render(Params, _Vars, Context) ->
      %% FIXME: render like button here
      {ok, Context}.


Scomp internals
---------------
.. highlight:: none

During the evaluation of scomps we are able to:

- wire actions
- add scripts
- add validators
- add context variables (accessible only by other scomps)

After rendering a template it is inspected to find all scripts,
actions and validators.  They are placed as #context records in the
resulting iolist, the #contexts should be replaced by the 'render'
record-attribute.  There will be a special 'script' atom to signify
the place where all collected scripts will be inserted.  When the
script atom is not found, then the result of the template is a
context, unless there are no collected scripts.

Process state diagram, how all processes work together to render a scomp::

  Controller        Template                        Scomp server        Scomp       Page/Session/User
    |
    |------------>  render(Template,ReqContext)
                        |
                        | ------------------------------- lookup missing var ---------->|
                                                                                        |
                        |<------------------------------------- value ------------------|
                        |
                        |------------------> render(Scomp,ReqContext)
                                                        |
                                                        |---------> render(ReqContext)
                                                        :               |
                                                    (if cached)         |
                                                        :               |
                                                        |<--ReqContext--|
                                                        |
                                                        |
                                                        |
                                                        |
                                                        |
                        |<------------ Output ----------|
                        |
                    Filter scripts
                        |
    |<---- Output ------|
    |
  reply user agent


The scripts/actions/validators are similar to the ones defined with
Nitrogen, though the record structure is redefined to accomodate easy
construction by the template compiler.

  
.. seealso:: a listing of all :ref:`scomps`.


.. seealso:: listing of all :ref:`tags`.
