
.. highlight:: django
.. include:: meta-pickle.rst

Pickle an Erlang value so that it can be safely submitted with a form.

The Erlang value is encoded using `erlang:term_to_binary/1` and signed with the site’s secret key.
In Erlang the value can be unpacked using `z_utils:depickle/2`

Usage::

  <input type="hidden" name="confirm" value="{{ 'Hello world' | pickle }}" />

This will generate something like::

  <input type="hidden" name="confirm" value="duZTXcXaxuruD3dhpt-rxWokrhuDbQAAAAtIZWxsbyB3b3JsZA" />

