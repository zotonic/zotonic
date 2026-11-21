{% for link in m.agent_discovery.links[id] %}
<link rel="{{ link.rel|escape }}" type="{{ link.type|escape }}" href="{{ link.href|escape }}">
{% endfor %}
