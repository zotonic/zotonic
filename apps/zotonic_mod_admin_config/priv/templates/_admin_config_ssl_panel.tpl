<div class="panel panel-default">
    <div class="panel-heading">
        <h3 class="panel-title">
            {{ nr }}.
            {% if cert.is_zotonic_self_signed %}
                {_ Self signed certificate _}
            {% else %}
                {{ cert.mod_title }}
            {% endif %}
            <span class="text-muted pull-right">{{ cert.module|default:"zotonic" }}</span>
        </h3>
    </div>
    <div class="panel-body">
        {% if cert.is_zotonic_self_signed %}
            <p>{_ This certificate is generated and signed by Zotonic. It will be used if no other certificates are available. _}</p>
        {% elseif cert.mod_description %}
            <p>{{ cert.mod_description }}</p>
        {% endif %}

        {% include "_admin_config_ssl_certinfo.tpl" %}
    </div>
</div>
