See also

tags [catinclude](/id/doc_template_tag_tag_catinclude) and [all include](/id/doc_template_tag_tag_all_include).

Include a template for all a resource’s categories from all modules.

This is an extension on the [catinclude](/id/doc_template_tag_tag_catinclude) tag. It will include all templates with the given name, instead of the first one found. Templates are defined in modules, because of that multiple modules can define a template with the same name.

This catinclude extension will include all available templates in the same order as defined in [catinclude](/id/doc_template_tag_tag_catinclude). Where the templates per category will be rendered in the order of their module’s defined priority. This is the order in which they are listed in the module admin page.

Examples of this mechanism can be found in [mod\_admin](/id/doc_module_mod_admin), for example the main menu and the category specific editing fields on the edit page.

An example usage:


```django
{% all catinclude "hello.tpl" id arg="val" %}
```

Includes all templates with the base name hello.tpl for the id’s category hierarchy.

For example, in the case of a *news* article:

*   all templates with the name .hello.news.tpl.
*   all templates with the name .hello.article.tpl.
*   all templates with the name .hello.text.tpl.
*   all templates with the name .hello.tpl.