{% overrules %}

{% block canonical %}{{ m.rsc[id].page_url_abs with z_language = m.translation.query_language }}{% endblock %}
{% block shortlink %}{% with m.translation.query_language as z_language %}{% inherit %}{% endwith %}{% endblock %}
