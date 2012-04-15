{# 
	Simple edit button for use on pages.  
	Parameters:
		id		id of the page
#}
{% if m.rsc[id].is_editable %}
        <a href="{% url admin_edit_rsc id=id %}" class="btn btn-primary btn-mini">{_ edit this page _}</a>
{% endif %}
