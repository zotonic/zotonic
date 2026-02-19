<!DOCTYPE html>
<html {% include "_language_attrs.tpl" class=false language=z_language %} class="zotonic-admin environment-{{ m.site.environment }}">
    <head>
        <meta charset="utf-8">
        <title>{% block title %}{_ Admin _}{% endblock %} &mdash; {{ m.site.title|default:"Zotonic" }} Admin</title>

        <link rel="icon" href="/favicon.ico" type="image/x-icon" />
        <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
        <link rel="manifest" href="{% url manifest_json %}" />

        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta name="description" content="">
        <meta name="robots" content="noindex,nofollow">

        {% lib
            "css/admin-bootstrap3.css"
            minify
        %}

        {% lib
            "css/zp-menuedit.css"
            "css/z.modal.css"
            "css/z.icons.css"
            "css/z.bridge.css"
            "css/logon.css"
            "css/jquery.loadmask.css"
            "css/zotonic-admin.css"
            "css/zotonic-search-view-admin.css"
            "css/prism.css"
            minify
        %}

        {% all include "_html_head_admin.tpl" no_prism %}

        {% block head_extra %}
        {% endblock %}
    </head>
    <body id="body" class="{% block bodyclass %}{% endblock %}"{% block bodyattr %}{% endblock %} data-cotonic-pathname-search="{% cotonic_pathname_search %}">

    {% block navigation %}
        {% include "_admin_menu.tpl" %}
    {% endblock %}

    {% block container %}
        <div class="admin-container">
            {% block content %}{% endblock %}
        </div>
    {% endblock %}

    {% block footer %}
        {% include "_admin_footer.tpl" %}
    {% endblock %}

    {% include "_bridge_warning.tpl" %}

    {% include "_admin_js_include.tpl" %}
    {% block js_extra %}{% endblock %}

    {% block html_body_admin %}{% all include "_html_body_admin.tpl" %}{% endblock %}

    {% block editor %}{% endblock %}

    {% script %}

    {% optional include "_fileuploader_worker.tpl" %}

</body>
</html>
