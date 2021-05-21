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
    </div>
</div>
