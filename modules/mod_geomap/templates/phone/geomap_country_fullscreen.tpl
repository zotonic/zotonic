{% extends "geomap_fullscreen.tpl" %}


{% block content %}
    <div class="row-fluid map" id="map"></div>
    {% lib 
        "js/openlayers/OpenLayers.js"
        "js/geomap.country.js"
    %}
    {% javascript %}window.GeoMapCountry.init();{% endjavascript %}
{% endblock %}

