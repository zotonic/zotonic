{% if hg[name] %}
	{% button text=_"update" 
			  title=_"Perform a Mercurial update this site." 
			  delegate="mod_zotonic_status_mercurial"
			  postback={hgup site=name}
	%}
{% endif %}
