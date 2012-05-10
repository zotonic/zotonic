{% extends "base.tpl" %}

{% block title %}{_ Map of _} {{ id.title }}{% endblock %}

{% block content %}
    <h1>{_ Where is _} {{id.title }}?</h1>
    
    <p><a href="{{ id.page_url }}">{_ Back to _} {{ id.title }}</a></p>

    {% include "_address.tpl" no_link_map %}

    {% if id.computed_location_lat %}
    <div class="maps">
        {% block map %}
            {% if not id.location_lat and not id.address_street_1 %}
                {# imprecise #}
                <div class="map">
                {% if id.address_city %}
                    {% include "_geomap_static_simple.tpl" zoom=8 %}
                {% else %}
                    {% include "_geomap_static_simple.tpl" zoom=5 %}
                {% endif %}
                </div>
            {% else %}
                {# precise #}
                <div class="map">
                    {% include "_geomap_static_simple.tpl" zoom=9 %}
                    <p class="caption">{{ id.address_city|default:_"City" }}</p>
                </div>
                <div class="map">
                    {% include "_geomap_static_simple.tpl" zoom=13 %}
                    <p class="caption">{{ id.address_street_1|default:_"Street" }}</p>
                </div>
            {% endif %}
        {% endblock %}
    </div>
    {% endif %}
{% endblock %}
