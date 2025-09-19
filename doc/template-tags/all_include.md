See also

tag [include](/id/doc_template_tag_tag_include).

Call all modules to include a certain template.

*   `{% all include "foobar.tpl" %}` will include all templates named `foobar.tpl`.
*   `{% include "foobar.tpl" %}` only includes the highest priority `foobar.tpl`.

Exactly the same [module priority](/id/doc_developerguide_modules) is also valid for all files in the `lib/` directory of modules.

This allows any module to change the static css, javascript, images, favicon.ico, robots.txt and other static files with its own version.

This is an extension on the [include](/id/doc_template_tag_tag_include) tag. It will include all templates with the given name, instead of the first one found. Templates are defined in modules, because of that multiple modules can define a template with the same name.

For example when you have two modules (mod\_a and mod\_b), both with the template \_name.tpl. When the template in mod\_a is defined as:


```django
this is mod_a's {{ hello }}
```

and in mod\_b as:


```django
this is mod_b's {{ hello }}
```

then the tag:


```django
{% all include "_name.tpl" hello="world" %}
```

Will output:


```django
this is mod_a's world
this is mod_b's world
```

The modules will be called in the order of their defined priority. This is the order in which they are listed in the module admin page.

Examples of this mechanism can be found in [mod\_admin](/id/doc_module_mod_admin), for example the main menu and the category specific editing fields on the edit page.

Another example is the \_html\_head.tpl template which is included from the [base.tpl](../templates/template_base.html#template-base) template and allows all modules to add HTML to the head of a generated HTML page.