
Template Best Practices and Pitfalls
------------------------------------

This chapter lists some preferred solutions to common tasks and
pitfalls you may encounter while developing with templates.


Variable Naming
^^^^^^^^^^^^^^^

Name your variables for what they represent.  If you are searching for
articles, name the search result variable `article` to make things clear.

In particular, if you are iterating over a list or other iterable
variable called images, then your item variable should be named `image`.
This makes generative templates easier to follow.

Pitfalls
^^^^^^^^

Using 'm' as a template variable blocks model access
....................................................

Avoid using the name ``m`` for a variable.  It has a special meaning
in templates for accessing models like :ref:`model-site` and
:ref:`model-rsc` as `m.site` and `m.rsc`.  Effectively. all Erlang
modules with names starting with `m_` are made available in templates
through the ``m`` variable.

It is particularly important since using m as a variable name will
disable model module access for the entire scope within which that
variable is defined.  This can lead to very confusing template errors.

Using 'id' as a template variable blocks access to the current page
...................................................................

When rendering a page through :ref:`controller-page`, Zotonic sets the
`id` variable based on the :term:`resource` (page) being rendered.  It
is also conventionally used by dispatch rules to supply the id of the
page to render.

Note, however, that there are legitmate cases for using id as a
template variable. It is a good idea to reuse another template to
render a section for one page and treating related content as the
"current page" for that template by assigning id in a for loop or with
context.  Using id otherwise will likely confuse other developers
trying to read your templates.

