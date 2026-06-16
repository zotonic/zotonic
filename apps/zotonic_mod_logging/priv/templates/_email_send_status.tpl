{# Live updating view of the current send-status of an email message.
 # Needs mod_mqtt for the live updating scomp.
 #}
{% if message_nr %}
    {% live template="_email_send_status_view.tpl"
            topic="bridge/origin/model/log_email/event/email_send_status/"++message_nr
            message_nr=message_nr
    %}
{% endif %}
