.. highlight:: django
.. include:: meta-replace_args.rst

Replace ``$N`` placeholders in string from a list of replacement
values.

The input string is searched for any ``$N`` tags, which is replaced
with the `N`'th value from the list of arguments supplied to the filter.

Example usage::

  {{ "Replace $1 and give it some $2."|replace_args:["item", "meaning"] }}

Will result in: "Replace item and give it some meaning."

The ``$N`` tag may be escaped by \\ to avoid replacement.

`N` may be in the range [1..9]. If `N` is out of range of the provided
args, the `$N` tag is left as-is.

The `$N` tags may come in any order, any number of times.

When replacing a single tag, the replacement argument may be given
directly::

  {{ "John is $1"|replace_args:"the one" }}

Outputs: "John is the one"

.. versionadded:: 0.8

