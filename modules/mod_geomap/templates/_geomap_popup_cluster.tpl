{% block content %}
	{% print q.ids %}
{% endblock %}

{% javascript %}
    GeoMap._popup.updateSize();
{% endjavascript %}
