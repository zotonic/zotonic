{% extends "admin_base.tpl" %}

{% block title %}{_ Email configuration and settings _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Email configuration and settings _}</h2>

    <p>{_ Here you can see all the email configurations and send test emails. _}</p>
</div>

{% if m.sysconfig.email_override as email %}
    <p class="alert alert-info">
        <span class="glyphicon glyphicon-alert"></span>&nbsp;
        <b>{_ All email is sent to _} &lt;{{ email|escape }}&gt;</b><br>
        <span>{_ This is a setting in the system configuration and can not be overruled. _}</span>
    </p>
{% endif %}

{% if m.sysconfig.email_bcc as bcc %}
    <p class="alert alert-warning">
        <span class="glyphicon glyphicon-info-sign"></span>&nbsp;
        <b>{_ A copy of all email is sent to _} &lt;{{ bcc|escape }}&gt;</b><br>
        <span>{_ This is a setting in the system configuration and can not be overruled. _}</span>
    </p>
{% endif %}

<div class="row">
    <div class="col-md-6">
        <h3>{_ Configuration _}</h3>

        {% if m.acl.is_admin %}
            {% wire type="submit"
                    id="email_settings"
                    postback={config_save module=`site`}
                    delegate=`mod_admin_config`
            %}
            <form id="email_settings" class="form" action="postback">
                {% if m.sysconfig.smtp_relay %}
                    <p>
                        {_ The Zotonic system configuration is set to use a SMTP relay. _}<br>
                        {_ This is a setting in the system configuration and can not be overruled. _}
                    </p>
                    <div class="form-group row">
                        <label class="control-label col-md-offset-3 col-md-6">
                            <input disabled type="checkbox" checked> {_ Use relay to send email _}
                        </label>
                    </div>
                    <div class="form-group row">
                        <label class="control-label col-md-3">{_ Relay hostname _}</label>
                        <div class="col-md-6">
                            <input disabled class="form-control" type="text" value="{{ m.sysconfig.smtp_host|default:'localhost' }}">
                        </div>
                    </div>
                    <div class="form-group row">
                        <label class="control-label col-md-3">{_ Relay port _}</label>
                        <div class="col-md-2">
                            <input disabled class="form-control" type="text" value="{{ m.sysconfig.smtp_port|default:'25' }}">
                        </div>
                    </div>
                    <div class="form-group row">
                        <label class="col-md-3 control-label">{_ Username _}</label>
                        <div class="col-md-6">
                            <input disabled class="form-control" type="text" value="{{ m.sysconfig.smtp_username|escape }}">
                        </div>
                    </div>
                    <div class="form-group row">
                        <label class="control-label col-md-offset-3 col-md-6">
                            <input disabled type="checkbox" {% if m.sysconfig.smtp_ssl %}checked{% endif %}> {_ Always use SSL _}
                        </label>
                    </div>
                {% else %}
                    <p>
                        {_ Zotonic sends email using a built-in email server. You can send the email via an external relay. _}<br>
                        {_ This is useful if this server is not allowed to send email or to use servers with a better reputation. _}
                    </p>
                    <div class="form-group row">
                        <label class="control-label col-md-offset-3 col-md-6">
                            <input type="checkbox" name="smtp_relay" value="1" {% if m.config.site.smtp_relay.value %}checked{% endif %}> {_ Use relay to send email _}
                        </label>
                    </div>
                    <div class="form-group row ">
                        <label class="control-label col-md-3">{_ Relay hostname _}</label>
                        <div class="col-md-6">
                            <input type="text" class="form-control" name="site.smtp_relay_host" value="{{ m.config.site.smtp_relay_host.value|default:'localhost'|escape }}">
                        </div>
                    </div>
                    <div class="form-group row">
                        <label class="control-label col-md-3">{_ Relay port _}</label>
                        <div class="col-md-3">
                            <input type="number" class="form-control" name="site.smtp_relay_port" value="{{ m.config.site.smtp_relay_port.value|default:'25'|escape }}">
                        </div>
                    </div>
                    <div class="form-group row">
                        <label class="control-label col-md-3">{_ Username _}</label>
                        <div class="col-md-6">
                            <input type="text" class="form-control" name="site.smtp_relay_username" value="{{ m.config.site.smtp_relay_username.value|escape }}" autocomplete="off">
                        </div>
                    </div>
                    <div class="form-group row">
                        <label class="control-label col-md-3">{_ Password _}</label>
                        <div class="col-md-6">
                            <input type="text" class="form-control" name="site.smtp_relay_password" value="{{ m.config.site.smtp_relay_password.value|escape }}" autocomplete="off">
                        </div>
                    </div>
                    <div class="form-group row">
                        <label class="control-label col-md-offset-3 col-md-6">
                            <input type="checkbox" name="site.smtp_relay_ssl" value="1" {% if m.config.site.smtp_relay_ssl.value %}checked{% endif %}> {_ Always use SSL _}
                        </label>
                    </div>
                {% endif %}

                {% if not m.sysconfig.email_override %}
                    <div class="form-group row">
                        <label class="control-label col-md-3">{_ E-mail override _}</label>
                        <div class="col-md-6">
                            <input type="email" class="form-control" name="site.email_override" value="{{ m.config.site.email_override.value|escape }}">
                            <p class="help-block">
                                {_ It is possible to intercept all email and send it to a single address. Configure this address here. _}
                            </p>
                        </div>
                    </div>
                {% endif %}

                <div class="form-group row">
                    <label class="control-label col-md-3">{_ SMTP Hostname _}</label>
                    <div class="col-md-6">
                        <input type="text" class="form-control" name="site.smtphost" value="{{ m.config.site.smtphost.value|escape }}" placeholder="{{ m.site.hostname|escape }}">
                        <p class="help-block">
                            {_ Domain for all generated from- and envelop-addresses. _}
                            {_ Defaults to the hostname of the site. _}<br>
                            {_ If you change this then you might need to request a new SSL certificate that includes this hostname. _}
                        </p>
                    </div>
                </div>

                <div class="form-group row">
                    <label class="control-label col-md-3">{_ Email FROM _}</label>
                    <div class="col-md-6">
                        <input type="text" class="form-control" name="site.email_from" value="{{ m.config.site.email_from.value|escape }}" placeholder="noreply@{{ m.site.hostname|escape }}">
                        <p class="help-block">
                            {_ The default FROM address for emails. Defaults to _} &lt;noreply@{{ m.site.hostname|escape }}&gt;
                        </p>
                    </div>
                </div>

                <div class="form-actions row">
                    <div class="col-md-offset-3 col-md-9">
                        <button type="submit" class="btn btn-primary">{_ Save _}</button>
                    </div>
                </div>
            </form>

            <p><br></p>

        {% else %}
            <p class="alert alert-danger">
                {_ You must be an administrator to view or change the email configuration. _}
            </p>
        {% endif %}
    </div>
    <div class="col-md-6">

        <div class="panel panel-default">
            <div class="panel-heading">
                <h3 class="panel-title">
                    {_ Send a test email _}
                    <span class="text-muted pull-right">mod_admin_config</span>
                </h3>
            </div>
            <div class="panel-body">
                <p>{_ Send a test email or check the status of an email address. _}</p>

                {% wire id="test-email"
                        type="submit"
                        postback={test_email}
                        delegate=`mod_admin_email`
                %}
                <form id="test-email" class="form form-inline" action="postback">
                    <input type="email" class="form-control" name="email" value="" placeholder="{_ test@example.com _}">
                    <button type="submit" name="send" class="btn btn-primary">{_ Send _}</button>
                    {% if m.modules.active.mod_email_status %}
                        <button type="submit" name="status" class="btn btn-default">{_ View email status _}</button>
                    {% endif %}
                </form>

                <div id="email_test_result"></div>
            </div>
        </div>

        {% all include "_admin_config_email_panel.tpl" %}

        {#
            TODO:

            * send test email to email address
            * find email in email_status
        #}
    </div>
</div>

{% endblock %}

