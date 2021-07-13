{% if not m.admin_status.is_ssl_application_configured %}
<div class="alert alert-warning" role="alert">
    <strong>{_ Warning! _}</strong>
    {_ SSL Application uses Erlang defaults, it is recommended to change this configuration in your <tt>erlang.config</tt>. _}
    <br />
    <em>{_ See also: <a href="http://docs.zotonic.com/en/stable/ref/configuration/zotonic-configuration.html#the-erlang-config-file">The erlang.config file</a> _}</em>
</div>
{% endif %}
