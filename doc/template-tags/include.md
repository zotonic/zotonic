See also

[all include](/id/doc_template_tag_tag_all_include) and [catinclude](/id/doc_template_tag_tag_catinclude).

Include another template. The include tag is replaced with the contents of the included template file. You can give arguments to the included template, they will be assigned as variables in the context of the included template.

Note

For compatibility with DTL we accept the optional with keyword between the template name and the arguments:


> \{% include "\_hello.tpl" with name="Peter" %\}

Example:


```django
{% include "_hello.tpl" name="Peter" %} world.
```

If \_hello.tpl contains the text:


```django
Hello {{ name }}’s
```

Then this will output the text `Hello Peter’s world.`.

If the template name is a string literal then the template will be inlined. When it is an expression then the template will be included during runtime.



Optional
--------

If the included template is not required, a optional keyword may be used:


```django
{% optional include "might-not-exist.tpl" %}
```



Unique ids
----------

Automatically generated ids (`{{ #name }}`) are unique within an included template and do not clash with similarly named ids in the including template.



Caching of the included template
--------------------------------

See also

[cache](/id/doc_template_tag_tag_cache)

The output of the included template can be cached. This is useful when rendering the template takes considerable time, for example when the template shows a list of recent news items, which comprises a query, fetching and rendering a list of news items. To cache such a list:


```django
{% include "recent_news_items.tpl" max_age=3600 %}
```

Caching is enabled by defining one of the caching arguments:

| Argument  | Description                                                                      | Example        |
| --------- | -------------------------------------------------------------------------------- | -------------- |
| max\\_age | The maximum time the output can be cached, in seconds. Specifying `0` for the maximum age does not cache the output but does protect agains slam dunks, multiple requests rendering the same template at the same time will share the output of the rendering. | `max_age=3600` |
| vary      | Dependency keys for the cached output. If a cache key with the same name is flushed or invalidated then the cached output of this template is also invalidated. You can use category names here. | `vary="news"`  |
| sudo      | If supplied then access control is disabled whilst rendering the included template. This will show any content not visible for the current user. Use with care. | `sudo`         |
| anondo    | Include the template as an anonymous user. Any [ACL checks](/id/doc_model_model_acl) in the included template will be executed as if the visitor was not logged in. | `anondo`       |
| runtime   | If supplied then the included template is not inlined but included during evaluation of the calling template. Only the supplied arguments are available as variables in the included template. | `runtime`      |