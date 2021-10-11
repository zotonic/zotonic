{% if m.site_update.vcs_site[name] %}
    {% button text=_"update"
        class="btn btn-warning btn-xs"
        title=_"Perform a version control (git/mercurial) update this site."
        delegate=`mod_site_update`
        postback={vcs_up site=name}
    %}
{% endif %}
