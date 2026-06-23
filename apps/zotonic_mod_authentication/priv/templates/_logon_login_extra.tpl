{#
Add other login options
#}
{% if not q.options.is_username_checked %}
    <ul class="z-logon-extra">
        <li class="text-muted z-logon-extra-separator -first"><span>{_ or _}</span></li>
        {% all include "_logon_extra.tpl" %}
    </ul>
{% else %}
    <ul class="z-logon-extra" style="display: none">
    </ul>
{% endif %}
