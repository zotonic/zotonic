Module versioning
=================

Modules can export a ``-module_schema()`` attribute which contains an
integer number, denoting the current module's version. On module
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
``install`` clause.

