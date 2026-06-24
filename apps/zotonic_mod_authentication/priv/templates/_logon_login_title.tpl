{% if q.authuser %}

    {% if q.options.is_user_local %}
        <h2 class="z-logon-title">{_ Confirm to connect _}</h2>
        <p>
            {% trans "Log in with your existing {site} account to connect the accounts. You only have to do this once." site=m.site.title %}
        </p>
    {% else %}
        <h2 class="z-logon-title">{_ Use different method _}</h2>
        <p>
            {_ Next time, enter your email address first to log in faster. _}
        </p>
    {% endif %}

{% else %}
    <h2 class="z-logon-title">{_ Log in to _} {{ m.site.title|default:"Zotonic" }}</h2>
{% endif %}
