{% if not m.admin_status.is_ssl_application_configured %}
<div class="alert alert-warning" role="alert">
    <strong>{_ Warning! _}</strong>
    {_ SSL Application uses Erlang defaults, it is recommended to change this configuration in your <tt>erlang.config</tt>. _}
    <br />
    <em>{_ See also: <a href="http://docs.zotonic.com/en/stable/ref/configuration/zotonic-configuration.html#the-erlang-config-file">The erlang.config file</a> _}</em>
</div>
{% endif %}

<div class="well well-lg">
    <dl class="dl-horizontal">
        {% if m.acl.is_admin %} {# Only admins are allowed to see the full paths #}
            <dt>{_ Home Directory _}</dt>
            <dd>{{ m.admin_status.init_arguments.home }}</dd>

            <dt>{_ Erlang Init Files _}</dt>
            <dd>{{ m.admin_status.init_arguments.config | join:"<br>" }}</dd>

            <dt>{_ Erlang Installation Root  _}</dt>
            <dd>{{ m.admin_status.init_arguments.root }}</dd>
        {% endif %}

        <dt>{_ Zotonic Version _}</dt>
        <dd>{{ m.admin_status.zotonic_version }}<dd>
    </dl>
</div>