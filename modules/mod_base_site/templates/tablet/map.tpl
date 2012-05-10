{% extends "base_sidebar.tpl" %}

{% block title %}{_ Map of _} {{ id.title }}{% endblock %}

{% block main %}
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
                    {% geomap_static id=id zoom=8 %}
                {% else %}
                    {% geomap_static id=id zoom=5 %}
                {% endif %}
                </div>
            {% else %}
                {# precise #}
                <div class="map">
                    {% geomap_static id=id zoom=9 %}
                    <p class="caption">{{ id.address_city|default:_"City" }}</p>
                </div>
                <div class="map">
                    {% geomap_static id=id zoom=13 %}
                    <p class="caption">{{ id.address_street_1|default:_"Street" }}</p>
                </div>
            {% endif %}
        {% endblock %}
    </div>
    {% endif %}
{% endblock %}
