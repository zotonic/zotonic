<p>
    {_ Enter the e-mail address of the recipient. The name of the recipient is optional. _}
    {% if m.rsc[id].mailinglist_private %}
    {_ This is a "private", externally managed list. Recipients will not receive any subscribe/unsubscribe messages. _}
    {% else %}
    {_ Tick the checkbox if you want the recipient to receive a welcome e-mail message.  _}
    {% endif %}

</p>

{% mailinglist_subscribe id=id recipient_id=recipient_id in_admin action=action %}
