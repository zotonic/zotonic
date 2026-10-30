<p class="text-danger">
    {_ Too many retries. _}
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
