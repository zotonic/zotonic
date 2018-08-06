{% if email and not m.email_status.is_valid[email] %}
    <a href="#" id="{{ #status }}" class="text-error email-status-flag" title="{_ There are problems with this email address. _}">
        <span class="glyphicon glyphicon-envelope"></span>
    </a>
    {% wire id=#status action={dialog_open title=email|escape template="_dialog_email_status.tpl" id=id} %}
{% endif %}
