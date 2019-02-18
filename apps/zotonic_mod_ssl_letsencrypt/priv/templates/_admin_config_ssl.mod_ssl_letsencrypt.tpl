<div class="row">
    <div class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <h3 class="panel-title">{{ nr }}. {_ SSL Let’s Encrypt Certificate _}</h3>
            </div>
            <div class="panel-body">
                <p>{_ Let’s Encrypt provides free SSL certificates for websites. Here you can request such a certificate. _}</p>

                <p><a href="https://letsencrypt.org" target="_blank">{_ Read more at the Let’s Encrypt website _} &raquo;</a></p>

                {% live template="_ssl_letsencrypt_status.tpl" topic="model/letsencrypt/event/status" %}


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
            </div>
        </div>
    </div>
</div>
