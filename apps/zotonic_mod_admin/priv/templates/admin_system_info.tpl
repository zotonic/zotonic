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
                    {% with m.admin_status.media_processing as media %}
                        <dt>{_ ImageMagick (local) _}</dt>
                        <dd>{% if media.local_imagemagick.available %}{{ media.local_imagemagick.version|escape }}{% else %}{_ Unavailable _}{% endif %}</dd>
                        {% for runner in media.runners %}
                            <dt>{_ ImageMagick (media runner) _} — {{ runner.url|escape }}</dt>
                            <dd>
                                {% if not runner.reachable %}
                                    {_ Could not contact the media runner. _}
                                {% elseif runner.imagemagick.available %}
                                    {{ runner.imagemagick.version|escape }}
                                {% else %}
                                    {_ Unavailable _}
                                {% endif %}
                            </dd>
                        {% endfor %}
                        <dt>{_ Media processing _}</dt>
                        <dd>
                            {% if media.configuration_error %}
                                {_ The media runner configuration is invalid. _}
                            {% elseif media.remote %}
                                {% if media.local_fallback %}{_ Remote runners with local fallback _}{% else %}{_ Remote runners only _}{% endif %}
                            {% else %}
                                {_ Local _}
                            {% endif %}
                        </dd>
                    {% endwith %}
                </dl>
            </div>
        </section>
    </div>
    <div class="col-md-8">
        <section class="widget">
            <h3 class="widget-header">{_ Installation _}</h3>
            <div class="widget-content table-responsive">
                {% with m.modules.active.mod_development and m.development.can_open_directory as can_open_directory %}
                <table class="table"><tbody>
<tr><th scope="row">{_ Site files directory _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.files_dir directory_key="files_dir" %}</td></tr>
<tr><th scope="row">{_ Data directory _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.data_dir directory_key="data_dir" %}</td></tr>
<tr><th scope="row">{_ Log directory _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.log_dir directory_key="log_dir" %}</td></tr>
<tr><th scope="row">{_ Config directory _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.config_dir directory_key="config_dir" %}</td></tr>
<tr><th scope="row">{_ Security directory _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.security_dir directory_key="security_dir" %}</td></tr>
<tr><th scope="row">{_ Home directory _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.init_arguments.home directory_key="home" %}</td></tr>
<tr><th scope="row">{_ Work directory _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.work_dir directory_key="work_dir" %}</td></tr>
<tr><th scope="row">{_ Erlang installation root _}</th><td>{% include "_admin_system_info_directory.tpl" can_open_directory=can_open_directory directory=m.admin_status.init_arguments.root directory_key="root" %}</td></tr>
                </tbody></table>
                {% endwith %}
            </div>
        </section>
        {% include "_admin_system_info_config_panel.tpl" %}
    </div>
</div>
{% else %}
<p class="alert alert-info">{_ Installation details are available to system administrators. _}</p>
{% endif %}
{% endblock %}
