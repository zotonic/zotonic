{% if vcs[name] %}
{% button text=_"update"
                          class="btn btn-mini"
			  title=_"Perform a version control (git/mercurial) update this site." 
			  delegate="mod_zotonic_status_vcs"
			  postback={vcs_up site=name}
	%}
{% endif %}
