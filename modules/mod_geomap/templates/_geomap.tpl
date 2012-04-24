{# Experimental, needs better parametrization (like resource id and div size) #}
<div id="{{ element_id|default:#map }}" style="width: 700px; height: 480px;"></div>

{% javascript %}
OpenLayers.ImgPath = '/lib/images/'
setTimeout(function() {
    var marker;

    var map = new OpenLayers.Map('{{ element_id|default:#map }}');
    var layer = new OpenLayers.Layer.OSM("OpenStreetMap");
    map.addLayer(layer);
    var markers = new OpenLayers.Layer.Markers("{_ Markers _}");
    map.addLayer(markers);

    var marker_size = new OpenLayers.Size(21,25);
    var marker_offset = new OpenLayers.Pixel(-(marker_size.w/2), -marker_size.h);
    var marker_icon = new OpenLayers.Icon('/lib/images/marker.png', marker_size, marker_offset);

    {% if longitude|is_defined and latitude|is_defined %}
        var map_location = new OpenLayers
                            .LonLat({{longitude}}, {{latitude}})
                            .transform(
                                new OpenLayers.Projection("EPSG:4326"),
                                map.getProjectionObject());
        map.setCenter(map_location, 15);
        marker_icon.setOpacity(0.8);
        markers.addMarker(new OpenLayers.Marker(map_location, marker_icon));
    {% else %}
        var map_location = new OpenLayers.LonLat(0, 0);
        map.setCenter(map_location, 2);
    {% endif %}
}, 500);
{% endjavascript %}
