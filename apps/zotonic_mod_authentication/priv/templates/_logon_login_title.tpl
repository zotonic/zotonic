{% if q.authuser %}
    <h2 class="z-logon-title">{_ Confirm to connect _}</h2>
    <p>{% trans "Log in with your existing {site} account to connect the accounts. You only have to do this once." site=m.site.title %}</p>
{% else %}
    <h2 class="z-logon-title">{_ Log in to _} {{ m.site.title|default:"Zotonic" }}</h2>
{% endif %}
