.. _guide-modules:

Modules
=======

Modules are the building blocks of Zotonic. They add functionality to your
Zotonic website such as:

* the admin web interface
* embedding videos in your content
* search engine optimization (SEO)
* social media integration.

Structurally, a module is a directory containing the module’s Erlang code,
:ref:`templates <guide-templates>`, :ref:`controllers
<guide-controllers>`, :ref:`dispatch rules <guide-dispatch>` and
more.

.. seealso::

    * The reference for a list of :ref:`all modules <ref-modules>`.
    * The `Zotonic Module Index <http://modules.zotonic.com>`_ lists
      third-party modules.
    * You can also :ref:`roll your own <cookbook-custom-module>`.

.. _activating-modules:

Activating modules
------------------

Before you can use a module, you need to activate it. You can do so in two ways.

1. For testing, you can enable the module in the
   :ref:`admin interface <mod_admin_modules>`, under System > Modules.
2. If you decide to use the module in your site, it’s best to declare so in your
   :ref:`site configuration <site-configuration-modules>`. This ensures that
   the module is activated not only for you but also for other developers and
   on other servers that the website may run on (e.g. a production server). Add
   the module name to the :file:`sites/yoursite/config` file, under the
   ``install_modules`` key::

    [
        % ...
        {install_modules, [
            % ...,
            mod_example
        ]}
        %...
    ].

   Then :ref:`restart the site <ref-status-site>` for the changes to be picked
   up.

.. _dev-configuration-parameters:

Module configuration
--------------------

Some modules can be configured to influence their behaviour. The module’s
documentation will tell you about its configuration parameters.

.. _guide-module-structure:

Directory structure
-------------------

A module groups related functions together into a single directory.
It contains an Erlang module (from here on called the ‘module file’)
and subdirectories for templates, actions, tags, dispatch rules and
more.

The generic structure is::

    mod_example/
        mod_example.erl
        actions/
        templates/
        etcetera.../

.. _module-file:

The module file
^^^^^^^^^^^^^^^

At the very minimum, a Zotonic module must have a module file. The name of the
module file is an Erlang file that must be the same as the name of the module’s
directory. Zotonic scans this file for metadata about the module and uses it to
start the module::

    -module(mod_example).
    -author("Nomen Nescio <nomen@example.com>").
    -mod_title("Your module title").
    -mod_description("Description what this module does.").
    -mod_prio(500).

In this case, the module code only consists of some metadata
properties, there is no real code in there. This is fine for a lot of
modules: since Zotonic already provides so many functions, there is
often little need to write custom code.

The ``mod_title`` and ``mod_description`` properties describe your
module in natural language: these properties will be visible on the
admin modules page. The ``mod_prio`` property defines the
:ref:`priority <module-priority>` of the module.

In cases where you need to execute code when the module starts, you
can export an optional ``init/1`` function. The parameter is a context
record initialized for the site the module will be running in. This is
useful when you need to initialize the database or other data
structures for which you don’t need a running process. When you also
need to execute code when a module stops you can export an optional
``terminate/2`` function. This function will be called when the module
terminates. The first parameter is a Reason parameter which indicates
why the module stopped. The second a context record similar to the one
in the ``init/1`` function.

When you do need a running process, read about those in the next
topic, :ref:`guide-modules-gen_server`.

Module subdirectories
^^^^^^^^^^^^^^^^^^^^^

Besides the module code file, a module usually has one or more
subdirectories. These are specially named; different parts of Zotonic
scan through different folders.

This section describes what each of the module folders hold.

actions/
""""""""

This directory holds the :ref:`actions <guide-actions>` defined by the
module. Every action name must be prefixed with the word “action” and
the module name (without the ``mod_``). For example the filename for the
action ``dialog_open`` in the module ``mod_base`` will be
``action_base_dialog_open.erl``

.. seealso:: :ref:`guide-actions`

dispatch/
"""""""""

This directory contains files with :ref:`dispatch rules
<guide-dispatch>`. You can name your files however you want, just
don't give them the extension ``.erl``, because then the Makefile will
try to compile them.

.. seealso:: :ref:`guide-dispatch`

lib/
""""

The `lib` (short for `library`) directory contains static images, css
and javascript files. These files will be served with via the
:ref:`tag-lib`. The usual layout of the lib directory is::

    lib/css/
    lib/images/
    lib/js/
    lib/misc/

.. seealso:: the :ref:`tag-lib` template tag.

scomps/
"""""""

Any custom tags that you define yourself go into the ``scomps/``
directory.

Scomps are prefixed in the same way as actions, except that the word
"scomp" is used. For example the scomp ``button`` in the module
``mod_base`` has as file name ``scomp_base_button.erl``.

.. seealso:: :ref:`guide-tags`

controllers/
............

This directory contains Erlang modules which define controllers which
are called from the dispatch system to handle incoming HTTP requests.

Controllers must have unique names, as they are compiled and loaded in
the Erlang system. The convention is to prefix every controller with
``controller_`` and the name of the module, for example
``controller_admin_edit.erl``.

.. seealso:: :ref:`guide-controllers`

models/
.......

This directory contains Erlang modules, each of which is a :ref:`model
<guide-models>`.

The module name of a model always starts with ``m_``, for example
``m_comment``. This model is then to be used in the templates as
``m.comment``.  Be careful to give your models a unique name to
prevent name clashes with other models and Erlang modules.

.. seealso:: :ref:`guide-models`

templates/
..........

This directory contains all :ref:`templates <guide-templates>`. Templates do not
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

.. seealso:: :ref:`guide-templates`

.. _module-directory-filters:

filters/
........

This directory holds Erlang modules, each of which defines a
:ref:`template filter <guide-filters>`.

Each filter must have an unique name, reflecting the filter’s
name. For example, the filter “tail” resides in the Erlang module
``filter_tail.erl`` and exports the function ``tail/1``.  Filters are
added in the filters directory.  The template compiler will insert
references to the correct modules into the compiled templates.  A
missing filter will result in a crash of the compiled template.

.. seealso:: :ref:`guide-filters`


validators/
...........

This directory holds Erlang modules, each of which defines a
:ref:`validator <guide-validators>`.

Validators are prefixed in the same way as actions and scomps, except
that the word “validator” is used. For example the validator “email”
in the module “mod_base” has the file name: “validator_base_email.erl”

.. seealso:: :ref:`guide-validators`

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

.. seealso:: :ref:`guide-services` and :ref:`controller-api`


Changing / recompiling files
----------------------------
Changes to the Erlang files in a module are visible after issuing the
``zotonic update`` CLI command, or ``z:m().`` from the Zotonic
shell. Any new lib or template files, or changes in the dispatch rules
are visible after the module indexer has rescanned all modules. You
can do this with the “rescan modules” button on the modules page in
the admin. Changes to templates are directly visible.


.. _module-priority:

Priority
--------

The :dfn:`module priority` is a number defined in the module’s code and is
usually a number between 1 and 1000; the default is 500. A lower number gives a
higher priority. Modules with higher priority are checked first for
:ref:`templates <guide-lookup-system>`, actions, custom tags etc.

The priority is defined by ``mod_prio`` in the :ref:`module file <module-file>`.
For a site, the priority is usually set to 1, to make sure that its templates
etc override the ones from the Zotonic mouules.

When two modules have the same priority then the modules are sorted by
their name.  That means that, given the same priority number,
``mod_aloha`` has higher priority than ``mod_hello``.

.. _guide-module-dependencies:

Dependencies
------------

Modules can have dependencies on other modules. These are expressed
via the module’s metadata, as follows::

    -mod_depends([mod_admin]).

This states that the current module is dependent on ``mod_admin`` to
be activated.

Sometimes, explicitly depending on a module name is not a good idea:
there might be more modules that perform the same functions but are
competing in implementation. In that case, such modules can export a
``mod_provides`` meta tag, so that dependent modules can depend on
what one of these modules provides.

Example: ``mod_a`` and ``mod_b`` both provide some functionality, ``foo``::

    -module(mod_a).
    -mod_provides([foo]).

and::

    -module(mod_b).
    -mod_provides([foo]).

Now, another module, ``mod_bar``, needs the "foo" functionality::

    -module(mod_bar).
    -mod_depends([foo]).

Now, the module manager will require either (or both!) of the
``mod_a`` and ``mod_b`` modules to be activated, before ``mod_bar``
can be activated.

A module automatically provides its own module name, as well as its
name minus the ``mod_``. So, ``mod_bar`` has implicitly the
following provides constructs::

    -module(mod_bar).
    -mod_provides([mod_bar, bar]).

These two provides are there even when a module adds its own
provides clauses.

.. _guide-module-startup-order:

Module startup order
--------------------

Note that when a site starts, its modules are started in order of
module dependency, in such a way that a module's dependencies are
always started before the module itsef starts.

.. _guide-modules-versioning:

Module versioning
-----------------

Modules can export a ``-module_schema()`` attribute which contains an
integer number, denoting the current module’s version. On module
initialization, ``Module:manage_schema/2`` is called which handles
installation and upgrade of data.

Minimal example::

    -module(mod_twitter).
    -mod_title("Twitter module").
    -mod_schema(3).  %% we are currently at revision 3

    -export([manage_schema/2]).
    .... more code here...

    manage_schema(install, Context) ->
    % .. code to install your stuff here, for instance:
    #datamodel{categories=
                 [
                  {tweet,
                   text,
                   [{title, <<"Tweet">>}]}
                 ]};

    manage_schema({upgrade, 2}, Context) ->
    %% code to upgrade from 1 to 2
    ok;

    manage_schema({upgrade, 3}, Context) ->
    %% code to upgrade from 2 to 3
    ok.

Note that the install function should always be kept up-to-date
according to the latest schema version. When you install a module for
the first time, no upgrade functions are called, but only the
``install`` clause. The upgrade functions exist for migrating old
data, not for newly installing a module.

Data model notification
^^^^^^^^^^^^^^^^^^^^^^^

In the ``#datamodel`` record you can manage categories, predicates, resources,
media and edges. You can also set the ``data`` property, which will send out
a first :ref:`notification <guide-notification>`. To subscribe to that
notification, export ``observe_manage_data/2`` in your site or module.

Using categories defined by other modules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When your site needs to add resources which are defined by other
module's ``manage_schema`` functions, you need to make sure that those
modules manage functions are called first. This can be realised by
adding a dependency to those modules, as explained in
:ref:`guide-module-startup-order`.

For instance, when you want to create a custom menu for your site::

    manage_schema(install, _Context) ->
        #datamodel{
            resources=[
                {help_menu, menu, [
                    {title, "Help"},
                    {menu, [...]}
                ]}
            ]
        }.

You also need to make sure that you add a :ref:`dependency <guide-module-dependencies>`
to ``mod_menu``, which creates the ``menu`` category for you::

    -mod_depends([mod_menu]).


.. _guide-modules-gen_server:

gen_server based modules
------------------------

When you need a running process, i.e., a module that does something in the
background, then it is possible to implement your module as a
`gen_server`_. A gen_server is a standard way to implement a reliable
Erlang worker process.

In that case you will need to add the behaviour and gen_server
functions. You also need to change the ``init/1`` function to accept
a property list, which contains the site definition and a ``{context,
Context}`` property.

This server module will be started for every site in a Zotonic system
where the module is enabled, so it can’t be a named server.

.. seealso:: `gen_server`_ in the Erlang documentation.

A minimal example
^^^^^^^^^^^^^^^^^

.. literalinclude:: ex_module_gen_server.erl

As you can see, this code is almost identical to the standard Erlang
``gen_server`` boilerplate, with the exception of the metadata on top.

You also see that the ``start_link/1`` function is already
implemented. Note that in this function the gen_server is started
without registering the server under a name: this is done because the
module can be started multiple times; once for each site that needs
it.

The ``init/1`` function contains some more boilerplate for getting the
``context{}`` argument from the arguments, and storing this context
into the server’s state. This way, you'll always have access to the
current context of the site in the rest of the gen_server’s functions.

.. _gen_server: http://erlang.org/doc/design_principles/gen_server_concepts.html
