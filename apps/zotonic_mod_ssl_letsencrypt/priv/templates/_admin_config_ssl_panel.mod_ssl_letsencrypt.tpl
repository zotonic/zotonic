<div class="widget">
    <a href="https://letsencrypt.org/" target="_blank">
        <img src="/lib/images/letsencrypt-logo-horizontal.png"
             alt="{_ Let’s Encrypt Certificate _}"
             style="height: 40px; margin: 2px 10px 0 0;"
             class="pull-right">
    </a>

    <div class="widget-header">
        {{ nr }}. {_ Let’s Encrypt Certificate _}
    </div>
    <div class="widget-content">
        <p>{_ Let’s Encrypt provides free SSL certificates for websites. Here you can request such a certificate. _}</p>

        <p><a href="https://letsencrypt.org/" target="_blank">{_ Read more at the Let’s Encrypt website _} &raquo;</a></p>

        {% live template="_ssl_letsencrypt_status.tpl" topic="bridge/origin/model/letsencrypt/event/status" %}

        {% if m.acl.use.mod_admin_config %}
            {% if m.sysconfig.port /= 80 %}
                <p class="alert alert-danger">
                    {_ The port for HTTP requests must be 80, it is now _} {{ m.sysconfig.port }}<br/>
                    {_ You can change this by setting <tt>port</tt> in the <tt>zotonic.config</tt> configuration file. _}
                </p>
            {% endif %}

            {% if m.sysconfig.ssl_port /= 443 %}
                <p class="alert alert-danger">
                    {_ The port for HTTPS requests must be 443, it is now _} {{ m.sysconfig.ssl_port }}<br/>
                    {_ You can change this by setting <tt>ssl_port</tt> in the <tt>zotonic.config</tt> configuration file. _}
                </p>
            {% endif %}

            {% if m.sysconfig.port == 80 and m.sysconfig.ssl_port == 443 %}
                {% lazy template="_admin_ssl_letsencrypt_form.tpl" %}
            {% else %}
                <p>{_ The errors above need to be corrected before a certificate can be requested. _}</p>
            {% endif %}
        {% else %}
            <p class="alert alert-danger">
                <strong>{_ Not allowed. _}</strong>
                {_ Only admnistrators can request certificates. _}
            </p>
        {% endif %}

        <p class="help-block">
            <i class="glyphicon glyphicon-info-sign"></i> {_ If there are problems requesting a Let’s Encrypt certificate then check: _}
            <a target="_blank" href="https://letsdebug.net/">letsdebug.net</a>
        </p>
    </div>
</div>
