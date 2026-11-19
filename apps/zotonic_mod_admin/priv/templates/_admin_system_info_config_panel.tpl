<section class="widget">
    <h3 class="widget-header">{_ Configuration files _}</h3>
    <div class="widget-content table-responsive">
        <h4>{_ Zotonic init files _}</h4>
        <table class="table">
            <tbody>
                {% for file in m.admin_status.zotonic_config_files %}
                    <tr><td>{% include "_admin_system_info_config.tpl" file=file %}</td></tr>
                {% empty %}
                    <tr><td>{_ No configuration files found. _}</td></tr>
                {% endfor %}
            </tbody>
        </table>
        <h4>{_ Site config files _}</h4>
        <table class="table">
            <tbody>
                {% for file in m.admin_status.site_config_files %}
                    <tr><td>{% include "_admin_system_info_config.tpl" file=file %}</td></tr>
                {% empty %}
                    <tr><td>{_ No configuration files found. _}</td></tr>
                {% endfor %}
            </tbody>
        </table>
    </div>
</section>
