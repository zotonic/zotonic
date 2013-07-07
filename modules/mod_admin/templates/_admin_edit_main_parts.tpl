{% block admin_edit_form_top %}{% endblock %}

{% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}
{% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}

{% if id.category_id.feature_show_address|if_undefined:`true` %}
    {% catinclude "_admin_edit_content_address.tpl" id is_editable=is_editable languages=languages %}
{% endif %}

{% if r.is_a.media or r.medium %}
    {% include "_admin_edit_content_media.tpl" %}
{% endif %}

{% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}
{% catinclude "_admin_edit_blocks.tpl" id is_editable=is_editable languages=languages %}
{% catinclude "_admin_edit_depiction.tpl" id is_editable=is_editable languages=languages %}

{# {% catinclude "_admin_edit_haspart.tpl" id is_editable=is_editable languages=languages %} #}

{% include "_admin_edit_content_advanced.tpl" %}
{% include "_admin_edit_content_seo.tpl" %}