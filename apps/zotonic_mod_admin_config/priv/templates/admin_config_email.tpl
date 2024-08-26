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
        <div class="widget">
            <div class="widget-header">{_ Configuration _}</div>
            <div class="widget-content">
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
                            <div class="form-group">
                                <label class="control-label">
                                    <input disabled type="checkbox" checked> {_ Use relay to send email _}
                                </label>
                            </div>
                            <div class="form-group label-floating">
                                <label class="control-label">{_ Relay hostname _}</label>
                                <input disabled class="form-control" type="text" value="{{ m.sysconfig.smtp_host|default:'localhost' }}" placeholder="{_ Relay hostname _}">
                            </div>
                            <div class="form-group label-floating">
                                <input disabled class="form-control" type="text" value="{{ m.sysconfig.smtp_port|default:'25' }}">
                                <label class="control-label">{_ Relay port _}</label>
                            </div>
                            <div class="form-group label-floating">
                                <input disabled class="form-control" type="text" value="{{ m.sysconfig.smtp_username|escape }}" placeholder="{_ Username _}">
                                <label class="control-label">{_ Username _}</label>
                            </div>
                            <div class="form-group">
                                <label class="control-label">
                                    <input disabled type="checkbox" {% if m.sysconfig.smtp_ssl %}checked{% endif %}> {_ Always use SSL _}
                                </label>
                            </div>
                        {% else %}
                            <p>
                                {_ Zotonic sends email using a built-in email server. You can send the email via an external relay. _}<br>
                                {_ This is useful if this server is not allowed to send email or to use servers with a better reputation. _}
                            </p>
                            <div class="form-group">
                                <label class="control-label">
                                    <input type="checkbox" name="smtp_relay" value="1" {% if m.config.site.smtp_relay.value %}checked{% endif %}> {_ Use relay to send email _}
                                </label>
                            </div>
                            <div class="form-group label-floating">
                                <input type="text" class="form-control" name="site.smtp_relay_host" value="{{ m.config.site.smtp_relay_host.value|default:'localhost'|escape }}" placeholder="{_ Relay hostname _}">
                                <label class="control-label">{_ Relay hostname _}</label>
                            </div>
                            <div class="form-group label-floating">
                                <input type="number" class="form-control" name="site.smtp_relay_port" value="{{ m.config.site.smtp_relay_port.value|escape }}" placeholder="{_ Relay port _}">
                                <label class="control-label">{_ Relay port _}</label>
                            </div>
                            <div class="form-group label-floating">
                                <input type="text" class="form-control" name="site.smtp_relay_username" value="{{ m.config.site.smtp_relay_username.value|escape }}" autocomplete="off" autocomplete="{_ Username _}">
                                <label class="control-label">{_ Username _}</label>
                            </div>
                            <div class="form-group label-floating">
                                <input type="text" class="form-control" name="site.smtp_relay_password" value="{{ m.config.site.smtp_relay_password.value|escape }}" autocomplete="off" placeholder="{_ Password _}">
                                <label class="control-label">{_ Password _}</label>
                            </div>
                            <div class="form-group">
                                <label class="control-label">
                                    <input type="checkbox" name="site.smtp_relay_ssl" value="1" {% if m.config.site.smtp_relay_ssl.value %}checked{% endif %}> {_ Always use SSL _}
                                </label>
                            </div>
                        {% endif %}

                        {% if not m.sysconfig.email_override %}
                            <div class="form-group label-floating">
                                <input type="email" class="form-control" name="site.email_override" value="{{ m.config.site.email_override.value|escape }}" placeholder="{_ E-mail intercept _}">
                                <label class="control-label">{_ E-mail intercept _}</label>
                                <p class="help-block">
                                    {_ It is possible to intercept all email and send it to a single address. Configure this address here. _}
                                </p>
                            </div>
                        {% endif %}

                        <div class="form-group label-floating">
                            <textarea class="form-control" name="site.email_override_exceptions" placeholder="{_ E-mail intercept &ndash; exceptions _}">{{ m.config.site.email_override_exceptions.value|escape }}</textarea>
                            <label class="control-label">{_ E-mail intercept &ndash; exceptions _}</label>
                            <p class="help-block">
                                {% if m.sysconfig.email_override as email %}
                                    {% trans "All email is sent to <b>{email}</b>, except for the email addresses and domains defined here." email=email %}
                                {% else %}
                                    {_ If (and only if) there is an e-mail intercept address defined above, then all e-mail is sent to that address <em>except</em> for the e-mail addresses and domains defined here. _}
                                {% endif %}
                                {_ Add email domains using <tt>@example.com</tt> and complete email addresses. Separate multiple entries using whitespace, semicolons or commas. _}
                            </p>
                        </div>

                        <div class="form-group label-floating">
                            <input type="text" class="form-control" name="site.smtphost" value="{{ m.config.site.smtphost.value|escape }}" placeholder="{_ SMTP Hostname _}">
                            <label class="control-label">{_ SMTP Hostname _}</label>
                            <p class="help-block">
                                {_ Domain for all generated from- and envelop-addresses. _}
                                {_ Defaults to the hostname of the site. _}<br>
                                {_ If you change this then you might need to request a new SSL certificate that includes this hostname. _}
                            </p>
                        </div>

                        <div class="form-group label-floating">
                            <input type="text" class="form-control" name="site.client_smtphost" value="{{ m.config.site.client_smtphost.value|escape }}" placeholder="{_ Hostname SMTP Client _}">
                            <label class="control-label">{_ Hostname SMTP Client _}</label>
                            <p class="help-block">
                                {_ The hostname to use as client when delivering e-mail to external SMTP servers.  _}
                                {_ Defaults to the SMTP hostname of the site. _}<br>
                                {_ It is usually set to the reverse DNS name of the machine. _}
                            </p>
                        </div>

                        <div class="form-group label-floating">
                            <input type="text" class="form-control" name="site.email_from" value="{{ m.config.site.email_from.value|escape }}" placeholder="{_ Email FROM _}">
                            <label class="control-label">{_ Email FROM _}</label>
                            <p class="help-block">
                                {_ The default FROM address for emails. Defaults to _} &lt;noreply@{{ m.site.hostname|escape }}&gt;
                            </p>
                        </div>

                        <div class="form-group">
                            <label class="control-label">
                                <input type="checkbox" name="site.email_images_noembed" value="1" {% if m.config.site.email_images_noembed.value %}checked{% endif %}> {_ Download images in emails after opening the message _}
                            </label>
                            <p class="help-block">
                                {_ Per default images smaller than 1MB are embedded in email messages. Using a separate download for images results in a smaller message size but non-public images will not be visible. Leave this disabled if you are sending non-public images. _}
                            </p>
                        </div>

                        <div class="form-actions">
                            <button type="submit" class="btn btn-primary">{_ Save _}</button>
                        </div>
                    </form>

                    <p><br></p>

                {% else %}
                    <p class="alert alert-danger">
                        {_ You must be an administrator to view or change the email configuration. _}
                    </p>
                {% endif %}
            </div>
        </div>
    </div>
    <div class="col-md-6">

        <div class="widget">
            <div class="widget-header">
                {_ Send a test email _}
                <span class="text-muted pull-right">mod_admin_config</span>
            </div>
            <div class="widget-content">
                <p>{_ Send a test email or check the status of an email address. _}</p>

                {% wire id="test-email"
                        type="submit"
                        postback={test_email}
                        delegate=`mod_admin_config`
                %}
                <form id="test-email" class="form form-inline" action="postback">
                    <input type="email" required class="form-control" name="email" value="" placeholder="{_ test@example.com _}" style="width:40ch;">
                    <button type="submit" name="send" class="btn btn-primary">{_ Send _}</button>
                    {% if m.modules.active.mod_email_status %}
                        <button type="submit" name="status" class="btn btn-default">{_ View email status _}</button>
                    {% endif %}
                </form>

                <br>

                <div id="email_test_result"></div>
            </div>
        </div>

        {% all include "_admin_config_email_panel.tpl" %}

    </div>
</div>

{% endblock %}

