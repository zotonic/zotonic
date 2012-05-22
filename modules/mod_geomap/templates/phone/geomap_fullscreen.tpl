{% extends "base_fullscreen.tpl" %}

{% block title %}{% if q.id %}{_ Map of _} {{ m.rsc[q.id].title }}{% else %}{_ Map _}{% endif %}{% endblock %}

{% block html_head_extra %}
    {% lib
            "css/openlayers/theme/default/style.tidy.css" 
            "css/geomap.css" 
    %}
    <style type="text/css">
    div#map {
        height: 100%;
        width: 100%;
        padding: 0;
    }
    div.olControlZoom {
        top: 90px;
        left: 16px;
    }
    </style>
{% endblock %}

{% block content %}
    <div class="row-fluid map" id="map"></div>
    {% lib 
        "js/openlayers/OpenLayers.js"
        "js/geomap.js"
    %}
    {% javascript %}window.GeoMap.init();{% endjavascript %}
{% endblock %}


