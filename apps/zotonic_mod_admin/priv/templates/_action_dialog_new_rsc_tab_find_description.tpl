
{% if text %}
    {% if category_id %}{_ Category _}: <b>{{ m.rsc[category_id].title }}</b>.
    {% elseif predicate %}{_ Pages for _} “<b>{{ m.rsc[predicate].title }}</b>”.
    {% endif %}
    {_ Matching _} “<b>{{ text|escape }} …</b>”<br>
{% else %}
    {% if category_id %}{_ Category _}: <b>{{ m.rsc[category_id].title }}</b>.
    {% elseif predicate %}{_ Pages for _} “<b>{{ m.rsc[predicate].title }}</b>”.
    {% endif %}
    {_ Click to preview. _}
{% endif %}
