{% if cert.certificate %}
    <table class="table">
        <tr>
            <th>{_ Hostname _}</th>
            <td>{{ cert.certificate.common_name|escape }}</td>
        </tr>
        <tr>
            <th>{_ Alternative names _}</th>
            <td>
                {% for name in cert.certificate.subject_alt_names %}
                    {{ name|escape }}{% if not forloop.last %}<br/>{% endif %}
                {% endfor %}
            </td>
        </tr>
        <tr>
            <th>{_ Valid till _}</th>
            <td>
                {% if cert.certificate.not_after|in_future %}
                    {{ cert.certificate.not_after|date:"Y-m-d H:i" }}
                {% else %}
                    <span class="text-danger">
                        {{ cert.certificate.not_after|date:"Y-m-d H:i" }}
                        <span class="glyphicon glyphicon-alert"></span>
                    </span>
                {% endif %}
            </td>
        </tr>
    </table>
{% else %}
    <p class="text-warning">{_ This module doesn’t supply a certificate. _}</p>
{% endif %}
