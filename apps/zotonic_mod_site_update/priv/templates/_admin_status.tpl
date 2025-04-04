{% if m.acl.use.mod_site_update %}
    <div class="form-group">
        {% if m.site_update.is_vcs_site %}
            {% button class="btn btn-default" text=_"Update site" postback=`vcs_up` delegate=`mod_site_update` %}
            <span class="help-block">{_ Update the site and templates from the version control system. _}</span>
        {% else %}
            <p class="text-warning">
                <span class="glyphicon glyphicon-exclamation-sign"></span>
                {_ Site update is not available as the site does not use a known version control system, like git. _}
            </p>
        {% endif %}
    </div>
{% endif %}

