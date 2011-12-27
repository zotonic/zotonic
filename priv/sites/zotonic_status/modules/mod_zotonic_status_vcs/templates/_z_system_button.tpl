{% if vcs_zotonic %}
{% button text=_"Update Zotonic" 
        title=_"Perform a version update of Zotonic (through git)." 
        delegate="mod_zotonic_status_vcs"
        postback={vcs_up zotonic}
	%}
{% endif %}
{% button text=_"Rebuild Zotonic"
        title=_"Recompile changed Erlang files in the background."
	delegate="mod_zotonic_status_vcs"
	postback="make"
%}
