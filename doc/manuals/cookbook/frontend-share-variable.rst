Share variable binding across blocks
====================================

How to avoid having to call the same query inside several blocks of the same page

Why
---

In some situations, you may have to use the same query result inside
several blocks of the same template. For instance, you may need to use
it in the body of a page and in its javascript block. Intuitively, you
would program your template as shown in these scripts:

**base_1.tpl**::

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

**mypage_1.tpl**::

  {% extends "base_1.tpl" %}
  
  {# THIS WON'T WORK BECAUSE %with% AND %block% TAGS CANNOT BE NESTED THIS WAY #}
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

Readers are assumed to be comfortable with the template syntax and with template inheritance (the {% extends %} and {% block %} tags).

How
---

The solution consists in placing the ``{% with %}`` tag in a different
file from the one that contains the surrounded "html_body" and "js"
``{% block %}`` tags.

The base template is modified as follows::

  <html>
  
  <head>...</head>
  
  {% block html_body_and_js %}
  {% include "_html_body_and_js.tpl" %}
  {% endblock %}
  
  </html>

The ... and ... tags are moved to a new inclusion file, _html_body_and_js.tpl, which contains the "html_body" and "js" blocks with their default contents::

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

The template mypage.tpl contains the same "html_body" and "js" blocks, with the desired content::

  {% extends "mypage_wrapper.tpl" %}
  
  {% block html_body %}
  Here is your result: {{ myresult|escape }}
  {% endblock %}
  
  {% block js %}
  alert('Do something with your result: {{ myresult|escapejs }}');
  {% endblock %} 

The {% with %} tag is placed in a new template, mypage_wrapper.tpl, which comes between the base template and mypage.tpl in term of inheritance::

  {% extends "base_2.tpl" %}
  
  {% block html_body_and_js %}
  
  {% with m.mymodule.myquery as myresult %}
  
  {% include "_html_body_and_js.tpl" %}
  
  {% endwith %}
  
  {% endblock %} {# html_body_and_js #}
