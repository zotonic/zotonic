{% if language %}
    {{ m.translation.localized_name[language]|default:language|escape }}
{% else %}
    {_ Unknown language _}
{% endif %}
