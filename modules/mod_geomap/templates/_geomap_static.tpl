{# Used by the geomap_static scomp to generate a static map #}

<div class="geomap-static" style="position: relative">
    <img src="/lib/images/marker.png" width="21" height="25" 
         style="position: absolute; z-index: 100; left: {{ marker_px[1]-10 }}px; top: {{ marker_px[2]-25 }}px"/>
{% for row in tiles %}
    <div class="geomap-row">{% for x,y,z in row %}<img class="geomap-tile" width="{{size}}" height="{{size}}" src="http://tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png" />{% endfor %}</div>
{% endfor %}
</div>
