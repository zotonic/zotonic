{#
Add other login options
#}
{% if not q.options.is_username_checked %}
    <div class="z-logon-external">
        <div class="text-muted z-logon-extra-separator -first"><span>{_ or _}</span></div>
        {% all include "_logon_extra.tpl" %}
    </div>
{% else %}
    <div class="z-logon-extra" style="display: none">
    </div>
{% endif %}
