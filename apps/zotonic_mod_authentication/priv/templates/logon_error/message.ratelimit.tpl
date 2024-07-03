<p class="text-danger">
    {_ Too many retries. _}
    {_ Please try again in _}
    {% with m.ratelimit.timeout as seconds %}
        {% if seconds == 3600 %}{_ an hour _}.
        {% elseif seconds > 3600 %}{{ ((seconds+3599)/3600)|round }} {_ hours _}.
        {% else %}{{ (seconds / 60)|round }} {_ minutes _}.
        {% endif %}
    {% endwith %}
</p>
