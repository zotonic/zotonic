{% with recipient.props as r %}
<p>
    {% if r.name_first or r.name_surname %}
    {_ Dear _}{% if r.name_first %} {{ r.name_first }}{% endif %}{% if r.name_surname %} {{ r.name_surname_prefix }} {{ r.name_surname }}{% endif %},
    {% else %}
    {_ Hello, _}
    {% endif %}
</p>
{% endwith %}
