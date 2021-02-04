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

        <p>
            <span class="glyphicon glyphicon-info-sign"></span> {_ This certificate is located in _}: <tt>{{ cert.directory|escape }}</tt>
        </p>
    </div>
</div>
