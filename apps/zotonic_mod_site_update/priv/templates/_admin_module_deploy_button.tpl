{% if m.acl.is_admin %}
    {% button text=_"Deploy latest"
        title=_"Review incoming changes and deploy this module’s Git repository."
        class="btn btn-default btn-xs"
        postback={vcs_up module=module}
        delegate=`mod_site_update`
    %}
{% endif %}
