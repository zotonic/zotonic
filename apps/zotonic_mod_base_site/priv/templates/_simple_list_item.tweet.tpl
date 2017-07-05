{% if id.is_visible and id.is_published and not id|member:exclude %}
<li {% include "_language_attrs.tpl" id=id %}>
    {{ id.body }}
</li>
{% endif %}
