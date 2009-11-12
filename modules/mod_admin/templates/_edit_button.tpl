{# 
	Simple edit button for use on pages.  
	Parameters:
		id		id of the page
#}
{% if m.rsc[id].is_editable %}
	{% button text=_"edit this page" action={redirect dispatch="admin_edit_rsc" id=id} %}
{% endif %}
