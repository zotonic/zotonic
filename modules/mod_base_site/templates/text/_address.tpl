{% if id.address_country or id.phone or id.website %}
<p class="well">
    {% if id.address_street_1 %}{{ id.address_street_1 }}<br/>{% endif %}
    {% if id.address_street_2 %}{{ id.address_street_2 }}<br/>{% endif %}
    {% if id.address_city %}{{ id.address_city }}, {{ id.address_postcode }}<br/>{% endif %}
    {% if id.address_state %}{{ id.address_state }}<br/>{% endif %}
    {% if id.address_country %}{{ m.l10n.country_name[id.address_country] }}<br/>{% endif %}
    {% if id.phone %}<a href="tel:{{ id.phone }}">{{ id.phone }}</a><br/>{% endif %}
    {% if id.email %}<a href="mailto:{{ id.email }}">{{ id.email }}</a><br/>{% endif %}
    {% if id.website %}<a href="{{ id.website }}">{{ id.website }}</a><br/>{% endif %}

    {% if not no_link_map and id.computed_location_lat %}
        <br/><a href="{% url map id=id %}">{_ Show on a map _} &raquo;</a>
    {% endif %}
</p>
{% elseif id.computed_location_lat %}
    <p class="btn"><a href="{% url map id=id %}">{_ Show on a map _}</a></p>
{% endif %}
