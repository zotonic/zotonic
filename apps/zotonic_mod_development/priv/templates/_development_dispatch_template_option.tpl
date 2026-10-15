{% if m.acl.is_allowed.use.mod_development %}
    {% wire id=#templates type="click" postback={template_matches template=template_option} delegate=`z_development_template` %}
    <button id="{{ #templates }}" type="button" class="btn btn-link btn-inline" title="{_ Show matching templates _}">{{ template_label|default:template_option|pprint }}</button>
{% else %}
    {{ template_label|default:template_option|pprint }}
{% endif %}
