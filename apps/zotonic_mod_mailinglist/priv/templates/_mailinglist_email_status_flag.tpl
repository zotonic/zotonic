{% if email %}
    <a href="#" id="{{ #status }}"
    {% if m.email_status.is_valid[email] %}
        class="btn btn-default btn-xs email-status-flag pull-right" title="{_ Email Status _}"
    {% else %}
        class=" btn btn-danger btn-xs email-status-flag pull-right"
           title="{_ There are problems with this email address. _}"
    {% endif %}
        style="margin-right: 4px">
        <span class="glyphicon glyphicon-envelope"></span> {_ Email status _}
    </a>
    {% wire id=#status action={dialog_open title=email|escape template="_dialog_email_status.tpl" email=email} %}
{% endif %}
