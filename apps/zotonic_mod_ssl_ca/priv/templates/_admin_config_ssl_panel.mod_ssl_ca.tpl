<div class="panel panel-default">
    <div class="panel-heading">
        <h3 class="panel-title">
            {{ nr }}.
            {_ Certificate Authority _}
            <span class="text-muted pull-right">{{ cert.module }}</span>
        </h3>
    </div>
    <div class="panel-body">
        <p>
            {_ This certificate is provided by a certificate authority. _}
        </p>

        {% include "_admin_config_ssl_certinfo.tpl" %}

        <p>
            <span class="glyphicon glyphicon-info-sign"></span> {_ This certificate is located in _}: <tt>{{ cert.directory|escape }}</tt>
        </p>
    </div>
</div>
