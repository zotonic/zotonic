{% if m.rsc[resource_option].exists %}
    <a href="{% url admin_edit_rsc id=m.rsc[resource_option].id %}" class="btn btn-link btn-inline" title="{_ Edit resource in admin _}">{{ resource_option|pprint }}</a>
{% else %}
    <span class="label label-danger">{{ resource_option|pprint }} — {_ Resource does not exist _}</span>
{% endif %}
