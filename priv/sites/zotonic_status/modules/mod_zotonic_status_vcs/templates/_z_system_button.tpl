<div class="form-group">
    <div>
        {% if vcs_zotonic %}
        {% button text=_"Update Zotonic"
                class="btn btn-default"
                title=_"Perform a version update of Zotonic (through git)." 
                delegate="mod_zotonic_status_vcs"
                postback={vcs_up zotonic}
	        %}
        {% endif %}

        {% button text=_"Rebuild Zotonic"
                class="btn btn-default"
                title=_"Recompile changed Erlang files in the background."
	        delegate="mod_zotonic_status_vcs"
	        postback="make"
        %}
    </div>
</div>
