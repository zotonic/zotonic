{% if m.site_update.vcs_site[name] %}
    {% button text=_"Deploy latest"
        class="btn btn-warning btn-xs"
        title=_"Review and deploy the latest version of this site."
        delegate=`mod_site_update`
        postback={vcs_up site=name}
    %}
{% endif %}
