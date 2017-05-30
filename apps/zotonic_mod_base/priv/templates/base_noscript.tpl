<!DOCTYPE html>
<html lang="{{ z_language }}">
  <head>
    <meta charset="utf-8" />
    <title>{% block title %}Zotonic{% endblock %}</title>

    <link rel="icon" href="/favicon.ico" type="image/x-icon" />
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />

    <meta name="viewport" content="width=device-width, initial-scale=1.0" />

    {% block _html_head %}
    {% endblock %}
  </head>

  <body>
    {% block content_area %}
       {% block content %}
          {% block main %}{% endblock %}
       {% endblock %}
    {% endblock %}

    {% block _js_include %}
    {% endblock%}
  </body>
</html>
