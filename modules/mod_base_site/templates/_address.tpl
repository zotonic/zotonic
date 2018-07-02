{% if id.address_country or id.phone or id.website %}
<p class="address">
    {% if id.address_street_1 %}{{ id.address_street_1 }}<br/>{% endif %}
    {% if id.address_street_2 %}{{ id.address_street_2 }}<br/>{% endif %}
    {% if id.address_city %}{{ id.address_city }}, {{ id.address_postcode }}<br/>{% endif %}
    {% if id.address_state %}{{ id.address_state }}<br/>{% endif %}
    {% if id.address_country %}{{ m.l10n.country_name[id.address_country] }}<br/>{% endif %}
    {% if id.phone %}<a href="tel:{{ id.phone }}"><i class="icon-comment"></i> {{ id.phone }}</a><br/>{% endif %}
    {% if id.email %}<a href="mailto:{{ id.email }}"><i class="icon-envelope"></i> {{ id.email }}</a><br/>{% endif %}
    {% if id.website %}<a href="{{ id.website }}"><i class="icon-bookmark"></i> {{ id.website }}</a><br/>{% endif %}

    {% if not no_link_map and id.computed_location_lat %}
        <br/><a href="{% url map id=id %}">{_ Show map _} &raquo;</a>
    {% endif %}
</p>
{% elseif id.computed_location_lat %}
    <p class="btn"><a href="{% url map id=id %}">{_ Show map _}</a></p>
{% endif %}
