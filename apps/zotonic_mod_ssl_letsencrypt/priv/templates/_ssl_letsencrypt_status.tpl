{% with m.ssl_letsencrypt.status as status %}
    {% if status.request_status == `ok` %}
        <p class="alert alert-success">
            {_ Let’s Encrypt certificate request was successful! _}
            <span class="text-muted">&mdash; {{ status.request_start|timesince }}</span>
        </p>
    {% elseif status.request_status == `error` %}
        <p class="alert alert-danger">
            {_ Sorry, there was an error fetching the Let’s Encrypt certificate. _}
            <span class="text-muted">&mdash; {{ status.request_start|timesince }}</span>
        </p>
    {% elseif status.request_status == `requesting` %}
        <div class="alert alert-info">
            <p>{_ Requesting a certificate from Let’s Encrypt for _}
                <strong>{{ status.request_hostname|escape }}</strong> ...
            </p>
            <p>{_ This might take a while, hang on! _}</p>
            <p>{_ The certificate status will be updated when the request finishes. _}</p>
        </div>
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
                    {{ status.cert_valid_till|date:"Y-m-d H:i" }}<br>
                    <span class="text-muted">{{ status.cert_valid_till|timesince }}</span>
                </td>
            </tr>
        </table>

        <p>
            <span class="glyphicon glyphicon-info-sign"></span> {_ Certificates are automatically renewed before they expire. _}
        </p>

        {% if status.request_status == `error` %}
            <p>
                {_ Retry the request below. _}
            </p>
        {% elseif status.request_status != `requesting` %}
            <p>
                {_ If you want different hostnames in the certificate then request a new certificate below. _}
            </p>
        {% endif %}
    {% else %}
        <p>{_ There is no certificate from Let’s Encrypt. You can request one with the form. _}</p>
    {% endif %}

    {% if m.acl.use.mod_admin_config %}
        {% if m.sysconfig.port /= 80 %}
            <p class="alert alert-danger">
                {_ The port for HTTP requests must be 80, it is now _} {{ m.sysconfig.port }}<br>
                {_ You can change this by setting <tt>port</tt> in the <tt>zotonic.config</tt> configuration file. _}
            </p>
        {% endif %}

        {% if m.sysconfig.ssl_port /= 443 %}
            <p class="alert alert-danger">
                {_ The port for HTTPS requests must be 443, it is now _} {{ m.sysconfig.ssl_port }}<br>
                {_ You can change this by setting <tt>ssl_port</tt> in the <tt>zotonic.config</tt> configuration file. _}
            </p>
        {% endif %}

        {% if m.sysconfig.port == 80 and m.sysconfig.ssl_port == 443 %}
            {% if status.request_status != `requesting` %}
                {% lazy template="_admin_ssl_letsencrypt_form.tpl" %}
            {% endif %}
        {% else %}
            <p>{_ The errors above need to be corrected before a certificate can be requested. _}</p>
        {% endif %}
    {% else %}
        <p class="alert alert-danger">
            <strong>{_ Not allowed. _}</strong>
            {_ Only admnistrators can request certificates. _}
        </p>
    {% endif %}


{% endwith %}
