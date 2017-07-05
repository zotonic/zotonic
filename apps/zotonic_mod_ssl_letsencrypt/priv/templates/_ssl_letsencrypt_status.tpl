{% with m.ssl_letsencrypt.status as status %}
    {% if status.request_status == `ok` %}
        <p class="text-success">{_ Let’s Encrypt certificate request was successful! _}</p>
    {% elseif status.request_status == `error` %}
        <p class="text-danger">{_ Sorry, there was an error fetching the Let’s Encrypt certificate. _}</p>
    {% elseif status.request_status == `requesting` %}
        <p class="text-info">
            {_ Requesting a certificate from Let’s Encrypt for _}
            <strong>{{ status.request_hostname|escape }}</strong>
            ...
        </p>
    {% endif %}

    {% if status.cert_is_valid %}
        <p>{_ There is a certificate with the following details: _}</p>

        <table class="table">
            <tr>
                <th>{_ Hostname _}</th>
                <td>{{ status.cert_hostname|escape }}</td>
            </tr>
            <tr>
                <th>{_ Alternative names _}</th>
                <td>
                    {% for name in status.cert_san %}
                        {{ name|escape }}{% if not forloop.last %}<br/>{% endif %}
                    {% endfor %}
                </td>
            </tr>
            <tr>
                <th>{_ Valid till _}</th>
                <td>
                    {{ status.cert_valid_till|date:"Y-m-d H:i" }}
                </td>
            </tr>
        </table>

        <p class="text-muted">
            <span class="glyphicon glyphicon-info-sign"></span> {_ Certificates are automatically renewed before they expire. _}
        </p>
    {% else %}
        <p>{_ There is no certificate from Let’s Encrypt. You can request one with the form. _}</p>
    {% endif %}
{% endwith %}


{#

[{request_status,ok},
 {request_start,{{2016,12,1},{16,46,48}}},
 {request_hostname,<<"cvc.worrell.nl">>},
 {request_san,[]},
 {cert_is_valid,true},
 {cert_hostname,<<"cvc.worrell.nl">>},
 {cert_san,[]},
 {cert_valid_till,{{2017,3,1},{15,47,0}}}]

#}