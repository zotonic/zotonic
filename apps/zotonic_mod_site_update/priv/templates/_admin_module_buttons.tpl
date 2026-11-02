{% if m.acl.is_admin %}
    {% if m.site_update.user_modules[module] as repository %}
        {% button text=_"Deploy latest"
            title=_"Review incoming changes and deploy this module’s Git repository."
            class="btn btn-default btn-xs"
            postback={vcs_up module=module}
            delegate=`mod_site_update`
        %}
    {% endif %}
{% endif %}
