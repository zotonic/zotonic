<div class="widget">
    <div class="widget-header">
        {{ nr }}.
        {_ Certificate Authority _}
        <span class="text-muted pull-right">{{ cert.module }}</span>
    </div>
    <div class="widget-content">
        <p>
            {_ This certificate is provided by a certificate authority. _}
        </p>

        {% include "_admin_config_ssl_certinfo.tpl" %}

        {% if cert.certificate %}
            <p>
                <span class="glyphicon glyphicon-info-sign"></span> {_ This certificate is located in _}: <tt>{{ cert.directory|escape }}</tt>
            </p>
        {% else %}
            <p class="help-block">
                {_ You can add certificates to your security directory: _}
                <tt>{{ m.admin_config.security_dir|escape }}</tt>
            </p>
            <p class="help-block">
                {_ The certificate files should be in a subdirectory with the name of the current site and have specific names or extensions: _}<br>
                <ul>
                    <li>
                        <tt>{{ m.site.site }}/ca/<i>filename</i>.crt</tt>
                    </li>
                    <li>
                        <tt>{{ m.site.site }}/ca/<i>filename</i>.key</tt>
                    </li>
                    <li>
                        <tt>{{ m.site.site }}/ca/<i>filename</i>.pem</tt>
                    </li>
                    <li>
                        <tt>{{ m.site.site }}/ca/<i>filename</i>.ca.crt</tt>, <i>or</i><br>
                        <tt>{{ m.site.site }}/ca/cabundle.crt</tt>, <i>or</i>
                        <br><tt>{{ m.site.site }}/ca/bundle.crt</tt>
                    </li>
                </ul>
            </p>

        {% endif %}

    </div>
</div>
