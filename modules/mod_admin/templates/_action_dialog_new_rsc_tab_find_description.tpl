
{% if text %}
    {% if category_id %}{{ m.rsc[category_id].title }},
    {% elseif predicate %}{_ Valid for _} “{{ m.rsc[predicate].title }}”,
    {% else %}{_ Existing pages _},
    {% endif %}
    {_ matching _} <b>{{ text|escape }} …</b><br>
{% else %}
    {% if category_id %}{{ m.rsc[category_id].title }}.
    {% elseif predicate %}{_ Pages for _} “{{ m.rsc[predicate].title }}”.
    {% else %}{_ Existing pages _}.
    {% endif %}
    {_ Click to view. _}
{% endif %}
