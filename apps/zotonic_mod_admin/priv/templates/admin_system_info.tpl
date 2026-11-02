{% extends "admin_base.tpl" %}
{% block title %}{_ System information _}{% endblock %}
{% block content %}
<div class="admin-header">
    <h2>{_ System information _}</h2>
    <p>{_ Software versions and installation paths. Runtime and host information is shared by all sites on this node. _}</p>
</div>
{% include "_admin_system_navigation.tpl" %}
{% if m.acl.is_admin %}
<div class="row">
    <div class="col-md-4">
        <section class="widget">
            <h3 class="widget-header">{_ Software _}</h3>
            <div class="widget-content">
                <dl>
                    <dt>{_ Zotonic _}</dt><dd>{{ m.admin_status.zotonic_version|escape }}</dd>
                    <dt>{_ Revision _}</dt><dd>{{ m.admin_status.zotonic_git_version|default:"—"|escape }}</dd>
                    <dt>{_ Erlang/OTP _}</dt><dd>{{ m.admin_status.otp_version|escape }}</dd>
                    <dt>{_ PostgreSQL _}</dt><dd>{{ m.admin_status.database_version|escape }}</dd>
                </dl>
            </div>
        </section>
    </div>
    <div class="col-md-8">
        <section class="widget">
            <h3 class="widget-header">{_ Installation _}</h3>
            <div class="widget-content table-responsive">
                <table class="table"><tbody>
<tr><th scope="row">{_ Site files directory _}</th><td><code>{{ m.admin_status.files_dir|escape }}</code></td></tr>
<tr><th scope="row">{_ Data directory _}</th><td><code>{{ m.admin_status.data_dir|escape }}</code></td></tr>
<tr><th scope="row">{_ Log directory _}</th><td><code>{{ m.admin_status.log_dir|escape }}</code></td></tr>
<tr><th scope="row">{_ Config directory _}</th><td><code>{{ m.admin_status.config_dir|escape }}</code></td></tr>
<tr><th scope="row">{_ Security directory _}</th><td><code>{{ m.admin_status.security_dir|escape }}</code></td></tr>
<tr><th scope="row">{_ Home directory _}</th><td><code>{{ m.admin_status.init_arguments.home|escape }}</code></td></tr>
<tr><th scope="row">{_ Work directory _}</th><td><code>{{ m.admin_status.work_dir|escape }}</code></td></tr>
<tr><th scope="row">{_ Erlang installation root _}</th><td><code>{{ m.admin_status.init_arguments.root|escape }}</code></td></tr>
                    <tr><th scope="row">{_ Erlang init files _}</th><td>{% for file in m.admin_status.init_arguments.config %}<code>{{ file|escape }}</code><br>{% endfor %}</td></tr>
                </tbody></table>
            </div>
        </section>
    </div>
</div>
{% else %}
<p class="alert alert-info">{_ Installation details are available to system administrators. _}</p>
{% endif %}
{% endblock %}
