.. _manual-module-structure:
Module structure
================

A module groups related functions together into a single directory.
It contains an `Erlang module` (from here on called the `module code`)
and subdirectories for templates, actions, scomps, dispatch rules and
more.

The generic structure is::

  mod_example/
    mod_example.erl
    templates/
    actions/
    etcetera.../

The module code
---------------

The name of the module code is an Erlang file that *must* be the same
as the name of the module’s directory. Zotonic scans this file for
metadata about the module and uses it to start the module.

The code of the smallest possible module is below::

  -module(mod_example).
  -author("Nomen Nescio <nomen@example.com>").
  -mod_title("Your module title").
  -mod_description("Description what this module does.").
  -mod_prio(500).

In this case, the module code only consists of some metadata
properties, there is no real code in there. This is fine for a lot of
modules: since Zotonic already provides so much functions, often it is
not needed to write custom code.

The `mod_title` and `mod_description` properties describe your module
in natural language: these properties will be visible on the admin
modules page. The `priority` defines the how important the module
is. The highest priority is 1, the default is 500. Modules with higher
priority are checked first for templates, actions, custom tags,
etc. Modules with the same priority are sorted by ascending module
name.

When you need to execute code when the module starts, you can export
an optional ``init/1`` function. This function will be called when the
module is started. The parameter is a context record initialized for
the site the module will be running in. This is useful when you need
to initialize the database or other data structures for which you
don’t need a running process.

When you do need a running process, read about those in the next
topic, :ref:`manual-modules-gen_server`.

Module subdirectories
---------------------

Besides the module code file, a module ususally has one or more
subdirectories. These are specially named; different parts of Zotonic
scan through different folders.

This section describes what each of the module folders hold.

actions/
........

This directory holds the :ref:`actions <manual-actions>` defined by the
module. Every action name must be prefixed with the word “action” and
the module name (without the `mod_`). For example the filename for the
action ``dialog_open`` in the module ``mod_base`` will be
``action_base_dialog_open.erl``

.. seealso:: :ref:`manual-actions`

dispatch/
.........

This directory contains files with :ref:`dispatch rules
<manual-dispatch>`. You can name your files like you want, though
don't give them the extension ``.erl``, because then the Makefile will
try to compile them.

.. seealso:: :ref:`manual-dispatch`

lib/
....

The `lib` (short for `library`) directory contains static images, css
and javascript files. These files will be served with via the
:ref:`tag-lib` tag. the `lib` dispatch rule. The usual layout of the
lib directory is::

  lib/css/
  lib/images/
  lib/js/
  lib/misc/

.. seealso:: the :ref:`tag-lib` template tag.


scomps/
.......

Any custom tags that you define yourself go into the ``scomps/``
directory.

Scomps are prefixed in the same way as actions, except that the word
"scomp" is used. For example the scomp ``button`` in the module
``mod_base`` has as file name ``scomp_base_button.erl``.

.. seealso:: :ref:`manual-scomps`

controllers/
............

This directory contains Erlang modules which define controllers which
are called from the dispatch system to handle incoming HTTP requests.

Controllers must have unique names, as they are compiled and loaded in
the Erlang system. The convenstion is to prefix every controller with
``controller_`` and the name of the module, for example
``controller_admin_edit.erl``.

.. seealso:: :ref:`manual-controllers`

models/
.......

This directory contains Erlang modules, each of which is a :ref:`model
<manual-models>`.

The module name of a model always starts with ``m_``, for example
``m_comment``. This model is then to be used in the templates as
``m.comment``.  Be careful to give your models an unique name to
prevent name clashes with other models and Erlang modules.

.. seealso:: :ref:`manual-models`

templates/
..........

This directory contains all :ref:`manual-templates`. Templates do not
have any prefix in their name, as they are not (directly) compiled as
Erlang modules. 

The following naming conventions for templates are used:

- All templates have the extension “.tpl”
- Templates used as a complete page can have any name: ”my_special_page.tpl”
- Templates used as the base of other templates, using the
  :ref:`tag-extends` tag, have the word “base” in them: ”base.tpl”;
  "email_base.tpl".
- Templates only used by including them in other templates start their
  name with an underscore: “_example.tpl“ 
- The template for the home page of a site is called "home.tpl"
- Templates for displaying resources are called "page.tpl"

.. seealso:: :ref:`manual-templates`

filters/
........

This directory holds Erlang modules, each of which defines a
:ref:`template filter <manual-filters>`.

Each filter must have an unique name, reflecting the filter’s
name. For example, the filter “tail” resides in the Erlang module
``filter_tail.erl`` and exports the function ``tail/1``.  Filters are
added in the filters directory.  The template compiler will insert
references to the correct modules into the compiled templates.  A
missing filter will result in a crash of the compiled template.

.. seealso:: :ref:`manual-filters`


validators/
...........

This directory holds Erlang modules, each of which defines a
:ref:`validator <manual-validators>`.

Validators are prefixed in the same way as actions and scomps, except
that the word “validator” is used. For example the validator “email”
in the module “mod_base” has as file name: “validator_base_email.erl”

.. seealso:: :ref:`manual-validators`

services/
.........

The services folder holds Erlang modules, each of which functions as
an API method that you can use to access Zotonic from another
application. These are invoked by :ref:`controller-api`.

Services are named a bit differently: the name of the module is
*always* used in the service name: The service ``base/export`` will be
found in the file ``mod_base/services/service_base_export.erl``. This
particular service can then be found at
``http://yoursite.com/api/base/export``.

.. seealso:: :ref:`controller-api`


Changing / recompiling files
----------------------------
Changes to the Erlang files in a module are visible after issuing the
``zotonic update`` CLI command, or ``z:m().`` from the Zotonic
shell. Any new lib or template files, or changes in the dispatch rules
are visible after the module indexer has rescanned all modules. You
can do this with the “rescan modules” button on the modules page in
the admin. Changes to templates are directly visible.
