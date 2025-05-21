{#
Add other login options
#}
{% if not q.options.is_username_checked %}
    <ul class="z-logon-extra">
        {% all include "_logon_extra.tpl" %}
        <li class="text-muted z-logon-extra-separator"><span>{_ or _}</span></li>
    </ul>
{% else %}
    <ul class="z-logon-extra">
    </ul>
{% endif %}