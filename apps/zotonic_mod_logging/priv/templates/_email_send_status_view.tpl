{# The current status of an email, used as a live template in _email_send_status.tpl #}
{% fragment show_status %}
    {% if mailer_status != 'received' %}
        <span  class="
            {% if severity <= 1 %}text-danger
            {% elseif severity == 2 %}text-warning
            {% endif %}
        ">
            <span class="glyphicon glyphicon-envelope"></span>

            {% if mailer_status == 'sending' %}
                {_ Sending… _} <img src="/lib/images/spinner.gif" style="height: 1em;">
            {% elseif mailer_status == 'sent' %}
                {_ The email has been sent. _}
            {% elseif mailer_status == 'relayed' %}
                {_ The email has been relayed. _}
            {% elseif mailer_status == 'bounce' %}
                {_ The email bounced, and could not be delivered. Check if the address is correct. _}
            {% elseif mailer_status == 'failed' %}
                {_ The email is not yet sent. We are retrying. _} <img src="/lib/images/spinner.gif" height="16">
            {% elseif mailer_status == 'blocked' %}
                {_ The email is not sent because the address is blocked by us. _}
            {% endif %}
        </span>
    {% endif %}
{% endfragment %}

{% if is_live_update %}
    {% use show_status mailer_status=q.mailer_status severity=q.severity %}
{% elseif m.log_email.email_status[message_nr] as status %}
    {% use show_status mailer_status=status.mailer_status severity=status.severity %}
{% else %}
    <span>
        <span class="glyphicon glyphicon-envelope"></span> {_ Waiting… _} <img src="/lib/images/spinner.gif" height="16">
    </span>
{% endif %}
