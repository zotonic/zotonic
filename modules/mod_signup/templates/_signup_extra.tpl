{% if xs_props %}
    <p>{_ You can also _} <a href="{% url signup %}">{_ sign up for a username and password _}</a>.</p>
{% else %}
    {#
        Add other login options
    #}
    <ul class="z-logon-extra">
        {% all include "_logon_extra.tpl" %}
        <li class="text-muted z-logon-extra-separator">{_ or _}</li>
    </ul>
{% endif %}
