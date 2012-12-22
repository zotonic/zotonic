{% if id.category_id.feature_show_geodata|if_undefined:`true` %}
{% include "_geomap_admin_location.tpl" %}
{% endif %}
