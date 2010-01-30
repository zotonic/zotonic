{% if id and m.rsc[id].title %}
<link rel="alternate" type="application/atom+xml;type=entry" href="{% url atom_entry id=id %}"/>
{% endif %}
