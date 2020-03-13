<!DOCTYPE html>
<html lang="{{ z_language }}" class="environment-{{ m.site.environment }}">

<head>
    <meta charset="utf-8" />
    <title>{% block title %}Zotonic{% endblock %}</title>

    <link rel="icon" href="/favicon.ico" type="image/x-icon" />
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />

    <meta name="viewport" content="width=device-width, initial-scale=1.0" />

    {% block _html_head %}
    {% endblock %}
</head>

<body class="{% block page_class %}{% endblock %}" data-cotonic-pathname-search="{% cotonic_pathname_search %}">
    <div class="container">
        {% block content_area %}
           {% block content %}
              {% block main %}{% endblock %}
           {% endblock %}
        {% endblock %}
    </div>

    {% all include "_html_body.tpl" %}

    {% block _js_include %}
    {% endblock%}

</body>

</html>
