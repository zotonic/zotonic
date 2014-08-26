Module versioning
=================

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


Using categories defined by other modules
-----------------------------------------

When your site needs to add resources which are defined by other
module's ``manage_schema`` functions, you need to make sure that those
modules manage functions are called first. This can be realised by
adding a dependency to those modules, as explained in
:ref:`manual-module-startup-order`.

For instance, when you want to create a custom menu for your site::

  manage_schema(install, _Context) ->
    #datamodel{
       resources=[
                  {help_menu, menu,
                   [
                    {title, "Help"},
                    {menu, [...]}
                   ]
                  }]}.

You also need to make sure that you add a dependency to ``mod_menu``,
which creates the ``menu`` category for you::

  -mod_depends([mod_menu]).
