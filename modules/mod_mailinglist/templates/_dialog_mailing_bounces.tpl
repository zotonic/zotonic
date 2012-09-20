{% with m.search[{mailinglist_recipients id=mid is_bounced=1}] as r %}
{% if r %}
<p>{_ On this page you see the addresses that have bounced. They have
    been disabled by default. Please correct each address and check the
    checkbox in front of the address to re-enable it. If an address is invalid, you can delete it. _}</p>
<ul class="short-list">
    <li class="headers clearfix">
        <span class="zp-15">{_ Enabled _}</span>
        <span class="zp-75">{_ Email _}</span>
        <span class="zp-10"></span>
    </li>
    {% for rid, email, is_enabled in r %}
    <li id="recipient-{{ rid }}" class="clearfix">
        <span class="zp-15">
            <input id="{{ #check.rid }}" type="checkbox" class="do_fieldreplace" {% if is_enabled %}checked="checked"{% endif %} />
            {% wire id=#check.rid postback={recipient_is_enabled_toggle recipient_id=rid} delegate="controller_admin_mailinglist_recipients" %}
        </span>
        <span class="zp-75">
            <input id="{{ #change.rid }}" type="text" name="email" value="{{ email|escape }}" style="width: 240px;" />
            {% validate id=#change.rid name="email" type={presence} type={email} %}
            {% wire type="change" id=#change.rid postback={recipient_change_email recipient_id=rid} delegate="controller_admin_mailinglist_recipients" %}
        </span>
        <span class="button-area">
            {% button text=_"edit" %}
            {% button text=_"delete" postback={recipient_delete recipient_id=rid} delegate="controller_admin_mailinglist_recipients" %}
        </span>
    </li>
    {% endfor %}
</ul>
{% button text=_"send mailing again to corrected addresses" 
   postback={resend_bounced list_id=mid id=id} delegate="mod_mailinglist"
%}
{% else %}
<p>{_ There are no addresses eligible for re-sending. You seem to have already processed all bouncing addresses. _}</p>
{% endif %}
{% endwith %}

