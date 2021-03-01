
{% optional include "_translation_init_languages.tpl" %}

{% catinclude "_admin_edit_basics.tpl" id show_header %}
{% all catinclude "_admin_edit_content.tpl" id %}

{% if id.category_id.is_feature_show_address|if_undefined:true %}
    {% catinclude "_admin_edit_content_address.tpl" id %}
{% endif %}

{% if id.category_id.is_feature_show_geodata
         |if_undefined:(id.category_id.is_feature_show_address)
         |if_undefined:true
      or id.location_lat
      or id.location_lng %}
    {% optional include "_geomap_admin_location.tpl" %}
{% endif %}

{% if id.is_a.media or id.medium %}
    {% include "_admin_edit_content_media.tpl" %}
{% endif %}

{% catinclude "_admin_edit_body.tpl" id show_header %}
{% catinclude "_admin_edit_blocks.tpl" id %}
{% catinclude "_admin_edit_depiction.tpl" id %}

{% include "_admin_edit_content_advanced.tpl" %}
{% include "_admin_edit_content_seo.tpl" show_header %}
