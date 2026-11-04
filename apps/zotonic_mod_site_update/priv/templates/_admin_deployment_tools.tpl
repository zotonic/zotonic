{% if m.acl.use.mod_site_update %}
    <div class="form-group">
        {% if m.site_update.is_vcs_site %}
            {% button class="btn btn-default" text=_"Deploy latest" postback=`vcs_up` delegate=`mod_site_update` %}
            {% include "_site_deploy_status.tpl" %}
            <span class="help-block">{_ Review incoming changes before deploying the latest version. _}</span>
        {% else %}
            <p class="text-warning">
                <span class="fa fa-warning"></span>
                {_ Site update is not available as the site does not use a known version control system, like git. _}
            </p>
        {% endif %}
    </div>
{% endif %}

