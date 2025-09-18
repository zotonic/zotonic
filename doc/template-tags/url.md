Generate the URL for a named dispatch rule. In this way it is possible to automatically change the generated URLs when the dispatch rules are modified.

For example to generate the URL to the admin for editing a [page](/id/doc_glossary#term-page), use:


```erlang
{% url admin_edit_rsc id=myid %}
```

Assuming myid is 42 then this will generate (on most Zotonic sites) the URL “/admin/edit/42”. The name “admin\_edit\_rsc” can be found in the [dispatch](/id/doc_dispatch_dispatch_mod_admin_dispatch#dispatch-mod-admin-dispatch) rules of [mod\_admin](/id/doc_module_mod_admin). Which [dispatch rules](/id/doc_developerguide_dispatch_rules) are available depends on which [Modules](/id/module#ref-modules) are enabled.

When the dispatch rule named in the first argument is unknown then an empty string is returned. There is no error message. This is to prevent breaking the web site when modules are enabled or disabled.

Arguments not named in the path of the dispatch rule are added to the query string of the returned URL:


```erlang
{% url admin_edit_rsc id=42 foo="bar" %}
```

Returns the URL “/admin/edit/42?foo=bar”.

Please note that the best way to generate the URL of a page (resource) is to use:


```erlang
{{ m.rsc[myid].page_url }}
```



Generate absolute URLs
----------------------

By default, the `{% url %}` tag generates relative URLs. Add the argument `absolute_url` to generate absolute URLs that include the scheme and hostname:


```erlang
{% url admin_edit_rsc id=42 foo="bar" absolute_url %}
```

will return a URL like “<http://example.com/admin/edit/42?foo=bar>”.



Finding out the current dispatch rule
-------------------------------------

The name of the current dispatch rule is always available in a template under the name `zotonic_dispatch`.

Check [Global template variables](/id/doc_reference_global_variables#ref-global-variables) for a full overview of variables that are always available in the templates.