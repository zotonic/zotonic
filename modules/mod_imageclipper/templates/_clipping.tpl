{% with 100,100 as w,h %}
{% if new %}
    {% image m.rsc[id].medium width=w height=h extent class="new" %}
{% else %}
    {% image m.rsc[id].medium width=w height=h extent %}
{% endif %}
{% endwith %}
