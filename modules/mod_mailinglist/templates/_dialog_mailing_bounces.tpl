{% with m.search[{mailinglist_bounced list_id=mid mailing_id=id}] as r %}
{% if r %}
<p>{_ On this page you see the addresses that have bounced. They have
    been disabled by default. Please correct each address and check the
    checkbox in front of the address to re-enable it. If an address is invalid, you can delete it. _}</p>

<table class="table table-striped do_adminLinkedTable">
    <tr>
        <th width="5%"><input type="checkbox" id="doall" /></td>
        <th width="35%">{_ Email _}</th>
        <th width="60%"></th>
    </tr>
    
    {% for rid, email, is_enabled in r %}
        <tr id="{{ #rcpt.rid }}">
            <td>
                <input id="{{ #check.rid }}" type="checkbox" class="bounce-toggle" {% if is_enabled %}checked="checked"{% endif %} style="margin:0" />
                {% wire id=#check.rid postback={recipient_is_enabled_toggle recipient_id=rid} delegate="controller_admin_mailinglist_recipients" %}
            </td>

            <td>
                <input class="form-control" id="{{ #change.rid }}" type="text" name="email" value="{{ email|escape }}" style="width: 240px;" />
                {% validate id=#change.rid name="email" type={presence} type={email} %}
                {% wire type="change" id=#change.rid postback={recipient_change_email recipient_id=rid} delegate="controller_admin_mailinglist_recipients" %}
            </td>
            <td>
                <div class="pull-right buttons">
                    {% button text=_"edit" class="btn btn-default btn-xs" %}
                    {% button text=_"delete" class="btn btn-default btn-xs" postback={recipient_delete recipient_id=rid target=#rcpt.rid} delegate="controller_admin_mailinglist_recipients" %}
                </div>
                
                {% with m.mailinglist.bounce_reason[email] as log %}
                    <span style="color: #999">{{ log.mailer_host }} {{ log.mailer_message }}</span>
                {% endwith %}
            </td>
        </tr>
    {% endfor %}
</table>

{% javascript %}
    $("#doall").click(function() {
    var filter = $(this).is(':checked') ? ':not(:checked)' : ':checked';
    $("input.bounce-toggle"+filter).click();
    });
{% endjavascript %}
<div class="modal-footer">
    {% button text=_"send mailing again to corrected addresses" class="btn btn-primary"
        postback={resend_bounced list_id=mid id=id} delegate="mod_mailinglist"
    %}
</div>
{% else %}
<p>{_ There are no addresses eligible for re-sending. You seem to have already processed all bouncing addresses. _}</p>
{% endif %}
{% endwith %}

