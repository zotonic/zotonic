{% extends "admin_base.tpl" %}

{% block title %}{_ SSL Certificates _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Certificates provided by SSL modules _}</h2>

    <p>{_ This is a list of the certificates provided by modules and the system. The first certificate will be used. _}</p>

    {% if not m.modules.active.mod_ssl_letsencrypt %}
        <p>{_ Enable <strong>mod_ssl_letsencrypt</strong> to request free SSL certificates from _}
            <a href="https://letsencrypt.org" target="_blank">Let’s Encrypt</a>
        </p>
    {% endif %}
</div>

<p class="help-block">
    <span class="glyphicon glyphicon-info-sign"></span> {_ The default directory for all site-certificates is  _}: <tt>{{ m.admin_config.security_dir|escape }}</tt>
</p>

<div class="row">
{% for cert in m.admin_config.ssl_certificates %}
    <div class="col-md-6">
        {% if cert.is_zotonic_self_signed %}
            {% include "_admin_config_ssl.tpl" nr=forloop.counter %}
        {% else %}
            {% catinclude "_admin_config_ssl.tpl" [cert.module] cert=cert nr=forloop.counter %}
        {% endif %}
    </div>
{% endfor %}
</div>

{% endblock %}
