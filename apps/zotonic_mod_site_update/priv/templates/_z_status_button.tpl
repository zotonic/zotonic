{% if m.site_update.vcs_site[name] %}
    {% button text=_"Deploy latest"
        class="btn btn-warning btn-xs"
        title=_"Review and deploy the latest version of this site."
        delegate=`mod_site_update`
        postback={vcs_up site=name}
    %}
    {% button text=_"Reset" class="btn btn-default btn-xs"
        postback={vcs_reset site=name} delegate=`mod_site_update` %}
    {% include "_site_deploy_status.tpl" %}
{% endif %}
