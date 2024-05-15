{#
Params:
- username: set by controller_logon
#}

{% if q.error == `ratelimit` %}
    <h2 class="z-logon-title">{_ Sorry, too many retries _}</h2>

    <p>
        {% with m.ratelimit.timeout as seconds %}
            {% if seconds == 3600 %}
                {_ Please try again in an hour _}.
            {% elseif seconds > 3600 %}
                {% trans "Please try again in {n} hours." n=((seconds+3599)/3600)|round %}
            {% else %}
                {% trans "Please try again in {n} minutes." n=(seconds / 60)|round %}
            {% endif %}
        {% endwith %}
    </p>
{% else %}
    <form id="password_change" method="post" class="z_logon_form"
          data-onsubmit-topic="model/auth-ui/post/form/change">

        {% include form_fields_tpl %}

    </form>
{% endif %}
