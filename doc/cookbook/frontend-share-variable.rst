.. highlight:: django

Share variable binding across blocks
====================================

How to avoid having to call the same query inside several blocks of the same page

Why
---

In some situations, you may have to use the same query result inside
several blocks of the same template. For instance, you may need to use
it in the body of a page and in its JavaScript block. Intuitively, you
would program your template as shown in these scripts::

    {# base.tpl #}
    <html>
    <head>...</head>
    <body>
    {% block html_body %}
        <!-- This is the default body -->
    {% endblock %}
    </body>

    <script>
    {% block js %}
        // This is the default js
    {% endblock %}
    </script>

    </html>

And a page::

    {# mypage_1.tpl #}
    {% extends "base_1.tpl" %}

    {# THIS won’t WORK BECAUSE %with% AND %block% TAGS CANNOT BE NESTED THIS WAY #}
    {% with m.mymodule.myquery as myresult %}

    {% block html_body %}
        Here is your result: {{ myresult|escape }}
    {% endblock %}

    {% block js %}
        alert('Do something with your result: {{ myresult|escapejs }}');
    {% endblock %}

    {% endwith %}

Unfortunately, this doesn't work, because Zotonic doesn't allow you to place a ``{% with %}`` tag around all ``{% block %}`` tags of `mypage_1.tpl`.

One solution consists in duplicating the ``{% with %}`` tag and moving it inside each block::

    {% extends "base_1.tpl" %}

    {% block html_body %}
        {% with m.mymodule.myquery as myresult %}
            Here is your result: {{ myresult|escape }}
        {% endwith %}
    {% endblock %}

    {% block js %}
        {% with m.mymodule.myquery as myresult %} {# AGAIN THE SAME QUERY #}
        alert('Do something with your result: {{ myresult|escapejs }}');
        {% endwith %}
    {% endblock %} 

However, this has a severe drawback: the same assignment will be done
twice, and if it comes from an "expansive" database query, this query
will be performed twice (or the query result will have to be cached,
which increases the complexity of your system significantly).

Assumptions
-----------

Readers are assumed to be comfortable with the template syntax and with template inheritance, specifically the ``{% extends %}`` and ``{% block %}`` tags.

How
---

The solution is to move both blocks to a new template, ``_html_body_and_js.tpl``::

    {# _html_body_and_js.tpl #}
    
    <body>
    {% block html_body %}
        <!-- This is the default body -->
    {% endblock %}
    </body>

    <script>
    {% block js %}
        // This is the default js
    {% endblock %}
    </script>


Our base template includes the new template so its contents will be drawn on the page::

    {# base.tpl #}

    <html>

    <head>...</head>

    {% block html_body_and_js %}
        {% include "_html_body_and_js.tpl" %}
    {% endblock %}

    </html>


Now we make sure that ``_html_body_and_js.tpl`` gets data. In a base subtemplate ``smart_base.tpl`` we override the block with the query::

    {# smart_base.tpl #}

    {% extends "base.tpl" %}

    {% block html_body_and_js %}
        {% with m.mymodule.myquery as myresult %}
            {% include "_html_body_and_js.tpl" %}
        {% endwith %}
    {% endblock %}


The page that needs the data extends ``smart_base.tpl``. Variable ``myresult`` is now accessible and can be used in both blocks::

    {# mypage.tpl #}
    {% extends "smart_base.tpl" %}

    {% block html_body %}
        Here is your result: {{ myresult|escape }}
    {% endblock %}

    {% block js %}
        alert('Do something with your result: {{ myresult|escapejs }}');
    {% endblock %} 



