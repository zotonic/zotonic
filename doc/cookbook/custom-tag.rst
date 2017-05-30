.. _cookbook-custom-tag:

Create a custom tag
===================

Custom tags, internally called *scomps*, are module-defined tags, which are used
when the logic is too complex to be executed in templates.

Custom tags add logic to templates or generate HTML/Javascript constructs that
are too difficult for templates. A good example is the :ref:`scomp-menu` scomp
which implements the menu, including sub menus -and highlighting of the current
menu item.

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
  -include_lib("zotonic_core/include/zotonic.hrl").

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
