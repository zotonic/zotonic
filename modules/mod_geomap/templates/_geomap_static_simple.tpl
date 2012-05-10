{# Use OpenStreetMap to generate a location's image. #}
{% if id.computed_location_lat %}
<div class="geomap-static-simple"><img alt="{_ Map _}" src="http://staticmap.openstreetmap.de/staticmap.php?center={{id.computed_location_lat}},{{id.computed_location_lng}}&amp;markers={{id.computed_location_lat}},{{id.computed_location_lng}},ol-marker&amp;zoom={{zoom|default:14}}&amp;size={{width|default:220}}x{{height|default:220}}&amp;maptype=mapnik" /></div>
{% endif %}
