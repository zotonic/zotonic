
{% if text %}
    {% if category_id %}{_ Category _}: {{ m.rsc[category_id].title }}.
    {% elseif predicate %}{_ Pages for _} “{{ m.rsc[predicate].title }}”.
    {% endif %}
    {_ Matching _} “{{ text|escape }} …”<br>
{% else %}
    {% if category_id %}{_ Category _}: {{ m.rsc[category_id].title }}.
    {% elseif predicate %}{_ Pages for _} “{{ m.rsc[predicate].title }}”.
    {% endif %}
    {_ Click to preview. _}
{% endif %}
