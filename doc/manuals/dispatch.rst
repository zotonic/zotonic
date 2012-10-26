.. _manual-dispatch:

The URL dispatch system
=======================

Dispatch rules map URLs to controllers, and vice versa.

A dispatch rule contains a pattern that is matched against the `path`
of an incoming request URL. They are also used for the reverse action
of generating request URLs in Zotonic.

When an URL is requested by the web browser, the dispatch system looks
at that URL and matches it agains all dispatch rules that are
loaded. Based on the match, it will call a :ref:`controller` to handle
the request.


Defining dispatch rules
-----------------------

Dispatch rules are defined in a `dispatch` file. The dispatch file
must be placed inside the ``dispatch/`` directory of a module or your
site.

A module or site can have multiple dispatch files, and they can have
`any` filename. Nota bene: the file may not have the extension
“.erl”. Otherwise it will be compiled by the Emakefile, which will
result in errors as a dispatch file is not a valid Erlang module.

The content of a dispatch file is an Erlang list of dispatch rules.

An example ``dispatch`` file looks like this::

  %% Example dispatch rules
  [
    {home,      [],                         controller_page,  [ {template, "home.tpl"}, {id, page_home} ]},
    {features,  ["features"],               controller_page,  [ {template, "features.tpl"}, {id, page_features} ]},
    {collection,["collection", id, slug],   controller_page,  [ {template, "collection.tpl"} ]},
    {category,  ["category", id, slug],     controller_page,  [ {template, "category.tpl"} ]},
    {documentation, ["documentation", id, slug], controller_page, [ {template, "documentation.tpl"} ]}
    ].

The module indexer will load all dispatch files. They can be reloaded
with the “rescan” button in the admin modules page. Illegal dispatch
files are skipped, showing an error message in the Zotonic shell.

When your dispatch rules don't work, check first if there are any
typos, then check if your dispatch rules are not overruled by a module
that loads earlier. Modules are loaded on priority first, then on
module name.


Anatomy of a dispatch rule
--------------------------

A single dispatch rule looks like::

  {page, ["page", id], controller_page, [{some_option,true}]}

Where the elements are:

1. a name identifying the dispatch rule (used by {% url %})
2. the path matching the request URL's path
3. the name of the controller
4. a property list with optional arguments to the resource module

URL match pattern
.................

Every element in the URL pattern list matches to a “directory level”
in the request URL. In the example, the pattern will match a URL like
"page/1234" but not “pages/1234” and also not “page/1234/x”.

The possible path elements are:

- Strings: fixed parts that must match with the request url
- atoms: bind to the text at that position
-  '*': a special atom binding to the remaining part of the request URL, this must be the last element of the path

URL matching using regular expressions
......................................

Some developers need very particular control of dispatch in order for
their applications to function as they want them to.

Say you want to only accept numerical arguments as an id in::

  {foo, ["foo", id], controller_foo, []} 

The you can use a dispatch rule with a regular expression test::

  {foo, ["foo", {id, "^[0-9]+$"}], controller_foo, []} 

or, you can specify http://erldocs.com/R14B02/stdlib/re.html?i=14&search=re:#run/3 some extra options::

  {foo, ["foo", {id, "1?2?", [notempty]}], controller_foo, []}

(In this case, the id must contain a 1 or a 2, amongst any other characters)


URL matching using callback modules
...................................

When all else fails, there is another option when you are, really,
really, desperate for a specific check.  You can call a module::

  {foo, ["foo", {id, {foo_module, foo_check}}], controller_foo, []}

Though note that this is (currently) an extremely expensive operation,
because it is called in the ``z_sites_dispatcher`` gen_server which
handles the matching of all incoming requests for all sites in one
single process.

When matching against "foo/bar", the module is called as::

  foo_module:foo_check("bar", Context).


Dispatch rule troubleshoooting
------------------------------

**Check the Syntax:** Load your dispatch file in from the EShell with
file:consult/1 and see if it returns errors.  

**Dispatch Rules are Order-sensitive:** Dispatch rules are processed
top-to-bottom in the file.  Are any rules above your rule capturing
the cases you are trying to match.  If so, move your rule up, but bear
in mind that you don't want to break those rules either.


.. _manual-dispatch-rewriting:

URL rewriting
-------------

Before URLs are matched, they first can be `rewritten` to match
something else. This is a powerful mechanism that allows you do
anything you like with URLs.

:ref:`mod_translation` uses this mechanism to prefix each URL with the
language code of the currently selected language.

.. todo:: document this fully, using mod_translation example



Dispatch rule BNF
-----------------

A dispatch rule is built up as follows::

  {RuleName, UrlPattern, ControllerModule, ControllerArgs}
  RuleName = atom()
  PathSpec = [PathSegmentSpec]
  PathSegmentSpec = StaticMatch | Wildcard | Variable
  StaticMatch = string()
  Wildcard = '*'
  PathVariable = atom()
  ResourceModule = atom()
  ResourceArgs = [{Key,Value}]

All `PathVariables` in the matching rule are made available to the
resource through ``z_context``. The `ResourceArgs` proplist is passed
to ``ControllerModule:init/1``.

`PathVariables` are part of the request-scope configuration of
`ControllerModule` . Things like the ID, name or category of a page being
requested can be gathered effectively here. Judicious use of
PathVariables can substantially reduce the number of dispatch rules
while making them easier to read.

`ControllerArgs` is the rule-scope configuration of
ControllerModule. It makes it possible to reuse a well-designed
resource module in many dispatch rules with different
needs. ControllerArgs is effective for establishing implementation
details like the template to be used, whether or not to do caching and
where to load static resources from.

Zotonic dispatch rules are identical to Webmachine's with the addition
of RuleName. Webmachine's dispatch rules are described in detail at
http://webmachine.basho.com/dispatcher.html .
