{#
Add other login options
#}
<ul class="z-logon-extra">
    {% all include "_logon_extra.tpl" %}
    <li class="text-muted z-logon-extra-separator">{_ or _}</li>
</ul>

{# Highlight the most recently used authentication method (if any) #}
{% if m.persistent.auth_method %}
    {% javascript %}
        $('.z-logon-extra li').css('opacity', '0.6');
        $('#logon_{{ m.persistent.auth_method }}').css('opacity', '1');
    {% endjavascript %}
{% endif %}
