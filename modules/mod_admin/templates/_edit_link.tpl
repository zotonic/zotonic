{# 
	Simple edit link for use on pages.  
	Parameters:
		id		id of the page
        title   the hover title
#}
{% if m.rsc[id].is_editable %}
<style type="text/css">
a.editpencil {
    width: 16px; height: 16px; float: left;
    margin: 1px;
    font-size: 1px;
    color: white; 
    background: white url(/lib/images/edit_pencils.png) no-repeat;
    background-position: -1px -37px;
}
a.editpencil:hover {
    background-position: -1px -1px;
}
</style>
<a class="editpencil" href="{% url admin_edit_rsc id=id %}" title="{{ title|default:_"Edit this page" }}">&nbsp;</a>
{% endif %}
