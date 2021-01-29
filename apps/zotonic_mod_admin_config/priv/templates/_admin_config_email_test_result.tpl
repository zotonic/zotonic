{# Show status of sent email #}
<p></p>
<div class="alert alert-info">
    <p>{_ Sent an email to _} <b>&lt;{{ email|escape }}&gt;</b></p>

    <p>
        <a href="{% url admin_log_email message_nr=msg_nr severity=4 %}">{_ Show log for message _} {{ msg_nr|escape }}</a>
    </p>
</div>
