<table class="table table-condensed">
    <thead><tr><th>{_ Option _}</th><th>{_ Value _}</th></tr></thead>
    <tbody>
    {% for option in options %}
        <tr>
            {% if option[1] %}
                <th>{{ option[1]|escape }}</th>
                <td>{% if option[1] == `template` %}
                    {% include "_development_dispatch_template_option.tpl" template_option=option[2] %}
                {% elseif option[1] == `id` %}
                    {% include "_development_dispatch_id_option.tpl" resource_option=option[2] %}
                {% else %}{{ option[2]|pprint }}{% endif %}</td>
            {% else %}
                <th>{{ option|escape }}</th><td>{_ Yes _}</td>
            {% endif %}
        </tr>
    {% endfor %}
    </tbody>
</table>
