<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
    <head>
        <meta charset="utf-8">
        <title>{% block title %}{% endblock %}{% if error_code /= 200 %} ({{ error_code }} Error){% endif %}</title>

        <link rel="icon" href="/favicon.ico" type="image/x-icon">
        <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon">

        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta name="author" content="Arjan Scherpenisse">

        {% all include "_html_head.tpl" %}

        {% lib
            "bootstrap/css/bootstrap.min.css"
            "css/jquery.loadmask.css"
            "css/z.icons.css"
            "css/zotonic-status.css"
        %}

        {% block html_head_extra %}{% endblock %}
    </head>
    <body class="{% block page_class %}{% endblock %}">
        <div class="header">
            {% block navbar %}
                {% include "_navbar.tpl" %}
            {% endblock %}
        </div>
        <div class="content">
            <div class="container">
                <div class="row content-area">
                    {% block content_area %}
                        {% block content %}{% endblock %}
                        {% block sidebar %}{% endblock %}
                    {% endblock %}
                </div>
            </div>
        </div>
        {#
            <div class="footer">
                {% include "_footer.tpl" %}
            </div>
        #}
	{% include "_js_include.tpl" %}
</body>
</html>
