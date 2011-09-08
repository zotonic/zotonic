{% if hg_zotonic and has_user %}
	{% button text=_"Update Zotonic" 
			  title=_"Perform a Mercurial update of Zotonic." 
			  delegate="mod_zotonic_status_mercurial"
			  postback={hgup zotonic}
	%}
{% endif %}	
{% button text=_"Rebuild Zotonic"
		  title=_"Recompile changed Erlang files in the background."
		  delegate="mod_zotonic_status_mercurial"
		  postback="make"
%}
