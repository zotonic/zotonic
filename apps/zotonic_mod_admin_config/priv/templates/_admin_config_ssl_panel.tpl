<div class="widget">
    <div class="widget-header">
        {{ nr }}.
        {% if cert.is_zotonic_self_signed %}
            {_ Self signed certificate _}
        {% else %}
            {{ cert.mod_title }}
        {% endif %}
        <span class="text-muted pull-right">{{ cert.module|default:"zotonic" }}</span>
    </div>
    <div class="widget-content">
        {% if cert.is_zotonic_self_signed %}
            <p>{_ This certificate is generated and signed by Zotonic. It will be used if no other certificates are available. _}</p>
        {% elseif cert.mod_description %}
            <p>{{ cert.mod_description }}</p>
        {% endif %}

        {% include "_admin_config_ssl_certinfo.tpl" %}

        {% if cert.is_zotonic_self_signed and m.acl.is_admin %}
            <div class="form-group">
                {% button
                    class="btn btn-default"
                    text=_"Regenerate self-signed certificate"
                    action={confirm
                        title=_"Regenerate self-signed certificate?"
                        text=_"This replaces the current self-signed certificate and private key. Any trust granted to the current certificate must be granted again to the new certificate."
                        cancel=_"Cancel"
                        ok=_"Regenerate certificate"
                        is_danger
                        postback={regenerate_self_signed}
                        delegate=`mod_admin_config`
                    }
                %}
            </div>
        {% endif %}
    </div>
</div>
